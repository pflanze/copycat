;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 svg ;; lib/svg
	 2d-shape
	 2d-polar
         svg-logo-lib
         (table-1 table-delete!)
         svg-logo-1)


(export (interface logo-command
          (interface path-command
            (class draw)
            (class jump)
            (class rotate)
            (class closed)
            (class paintopts)
            (class svgfragment))))


(include "lib/cj-standarddeclares.scm")



(definterface path-command
  extends: logo-command

  (defclass (draw [real? distance])

    ;; (should this be a method? (one of the "delegates, now
    ;; they're coming?.." style?))
    (def (perhaps-de-paint v)
         (if (painted? v)
             (painted.value v)
             v))
	    
    (defmethod (process v state)
      (let-draw
       ((distance) v)
       (let. ((shapes closed? paintoptionss stack) state)
             (let-turtlepos
              ((pos angle) (.turtlepos state))
              (let ((pos* (.+ pos (.point (2d-polar angle distance)))))
                (drawing-state
                 (turtlepos pos*
                            angle)
                 #t
                 closed?
                 paintoptionss
                 (let* ((otherwise
                         (&
                          (cons (painted
                                 paintoptionss
                                 (2d-path (list pos* pos)
                                          ;; ^ in reverse to
                                          ;; match .points-add
                                          closed?))
                                shapes)))
                        (maybe-shape
                         (and (pair? shapes)
                              (car shapes))))
                   (if maybe-shape
                       (let-painted
                        ((opts bareshape) maybe-shape)
                        (if (and (2d-path? bareshape)
                                 (.pen-down? state))
                            (begin
                              (assert
                               (.almost=
                                (car (.points bareshape))
                                pos
                                1e-10))
                              (cons (painted opts
                                             (.points-add bareshape
                                                          pos*))
                                    (cdr shapes)))
                            (otherwise)))
                       (otherwise)))
                 stack)))))))

  (defclass (jump [real? distance])
    (defmethod (process v state)
      (let-jump ;; <- ah and here's a difference
       ((distance) v)
       (let. ((pos angle) (.turtlepos state))
             (=> state
                 (.turtlepos-set
                  (turtlepos
                   (.+ pos (.point (2d-polar angle distance)))
                   angle))
                 (.pen-down?-set #f))))))
 
  (defclass (rotate [real? angle])
    (defmethod (process v state)
      (let-rotate
       ((angle2) v)
       (.turtlepos-update state
                          (C .angle-update _ (C + _ angle2))))))

  (defclass ((closed _closed)
             [(list-of path-command?) path-commands])
    (def (closed . cmds)
         (_closed (flatten cmds)))
    (defmethod (process v state)
      (let ((state*
             (_svn-logo-process (.path-commands v)
                                (drawing-state (.turtlepos state)
                                               #f
                                               #t
                                               (.paintoptionss state)
                                               '()
                                               (.stack state)))))
        (drawing-state
         (.turtlepos state*)
         #f
         (.closed? state)
         (.paintoptionss state)
         ;; ^ not state*, right? (don't have set, just wrap)

         ;; Can't use rappend, it would destroy
         ;; the z layering.
         (append (.shapes state*)
                 (.shapes state))
         (.stack state*)))))

  ;; can't reuse name |painted|, nor type paintoptions
  (defclass ((paintopts _paintopts)
             [paintoptions? paintoptions]
             [(list-of path-command?) path-commands])
    (def (paintopts opts . cmds)
         (_paintopts opts (flatten cmds)))
    (defmethod (process v state)
      (let ((state*
             (_svn-logo-process
              (.path-commands v)
              (=> state
                  (.paintoptionss-update
                   (C cons (.paintoptions v) _))
                  (.pen-down?-set #f)))))
        (drawing-state
         (.turtlepos state*)
         #f
         (.closed? state) ;; should be == in state*
         (.paintoptionss state)
         (.shapes state*)
         (.stack state*)))))

  ;; hmm does this fit here? HACK anyway?
  (defclass (svgfragment [procedure? value/pos])
    ;; ^ have to avoid name conflict, gah
    (defmethod (process v state)
      (drawing-state
       (.turtlepos state)       ;; unchanged pos?
       (.pen-down? state)       ;; or #f ?
       (.closed? state)         ;; or #f ?
       (.paintoptionss state)   ;; hmm pass to fragment *function*?
       (cons (svg-fragment ((.value/pos v)
                            (.turtlepos state)))
             (.shapes state))
       (.stack state)))))
