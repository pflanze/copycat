;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 svg ;; lib/svg
	 2d-shape
	 2d-polar)


(def p 2d-point)


(def (° deg)
     (* (/ deg 360) 2 pi))

(TEST
 > (° 180)
 3.141592653589793)


(def north-angle (° -90))

(defclass (turtlepos [2d-point? pos]
                     [real? angle]))

;; (turtle-state ? No, it also carries shapes.)
(defclass (drawing-state [turtlepos? turtlepos]
                         [boolean? pen-down?]
                         ;; ^ the (cursor, actor) lifts the pen after
                         ;;   any action that doesn't draw
                         [boolean? closed?]
                         ;; ^ whether new paths should be closed
                         [(list-of paintoptions?) paintoptionss]
                         [(either pair? null?) shapes]
                         [list? stack]))

(definterface logo-command

  (defclass (start [real? x]
		   [real? y])
    (defmethod (process v state)
      (drawing-state (let-start ((x y) v)
                                (turtlepos (2d-point x y)
                                           north-angle))
                     #f
                     (.closed? state)
                     (.paintoptionss state)
                     (.shapes state)
                     (.stack state))))

  (defclass (push-pos)
    (defmethod (process v state)
      (.stack-update state (C cons (.turtlepos state) _))))

  (defclass (pop-pos)
    (defmethod (process v state)
      (let. ((stack) state)
            (if (pair? stack)
                (chain state
                       (.turtlepos-set (car stack))
                       (.stack-set (cdr stack)))
                (error "svg-logo stack is empty")))))

  (defclass (dup)
    (defmethod (process v state)
      (.stack-update state
                     (lambda (stack)
                       (if (pair? stack)
                           (cons (car stack)
                                 stack)
                           (error "svg-logo stack is empty"))))))
 
  (defclass (cursor)
    (defmethod (process v state)
      (let-turtlepos
       ((pos angle) (.turtlepos state))
       (let ((pos* (.+ pos
                       (.point (2d-polar angle 2)))))
         (.shapes-update
          state
          (C cons*
             (painted
              (paint fill-color: (colorstring "yellow")
                     ;; mis-use of stroke-width for radius
                     stroke-width: 0.7)
              pos*)
             (painted
              (colorstring "red")
              (2d-line pos pos*))
             _))))))
 

  (definterface path-command

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
               (chain state
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
                (chain state
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
         (.turtlepos state)               ;; unchanged pos?
         (.pen-down? state)               ;; or #f ?
         (.closed? state)                 ;; or #f ?
         (.paintoptionss state) ;; hmm pass to fragment *function*?
         (cons (svg-fragment ((.value/pos v)
                              (.turtlepos state)))
               (.shapes state))
         (.stack state))))))



(def (_repeat [natural0? n] l)
     (cond ((zero? n)
	    '())
	   ((= n 1)
	    l)
	   (else
	    (append l (_repeat (dec n) l)))))

(TEST
 > (_repeat 0 '(a))
 ()
 > (_repeat 1 '(a))
 (a)
 > (_repeat 2 '(a))
 (a a)
 > (_repeat 2 '(a b))
 (a b a b))


(def (list-repeat n . l)
     (_repeat n l))

;; (def (svn-logo . cmds)
;;      (flatten cmds))

(def (_svn-logo-process l state)
     ;; ^ shapes is actually reverse, but same name used in .process
     ;; methods, too
     (if (null? l)
	 state
	 (let-pair
	  ((a l*) l)
	  (_svn-logo-process l* (.process a state)))))

(def (svn-logo-process l state)
     (reverse (.shapes (_svn-logo-process l state))))


(def default-drawing-state
     (drawing-state
      (turtlepos (2d-point 50 50)
		 north-angle)
      #f
      #f
      (list (paint stroke-color: (colorstring "black")
		   fill-color: (colorstring "none")
		   stroke-width: 1))
      '()
      '()))

(def (show-svn-logo . cmds)
     (xsystem "killall" "display")
     (showsvg*
      (2d-point 400 400) ;; just wtever  scaling danach klar
      (2d-window (2d-point -50 -50)
		 (2d-point 150 150))
      (svn-logo-process (flatten
			 (list cmds
			       (cursor)))
			default-drawing-state)))

(def (save-svn-logo . options)
     (lambda cmds
       (apply showsvg
	      (svn-logo-process (flatten cmds)
				default-drawing-state)
	      options)))


;; Deutsch

(defmacro (wieder name forms . body)
  `(##let ,name
	  ,(source-map (lambda (form)
			 (mcase form
				(`(`var `val)
				 ;; ah, unchanged
				 form)))
		       forms)
	  (##list ,@body)))

(defmacro (wenn c e)
  `(##if ,c
	 ,e
	 '()))

;; start
(def m draw) ;; malen
(def g jump) ;; gehen
(def (r° deg) ;; rechts
     (rotate (° deg)))
(def (l° deg) ;; links
     (rotate (- (° deg))))

(def (rr part) ;; rechts
     (rotate (* 2 pi part)))
(def (ll part) ;; links
     (rotate (- (* 2 pi part))))

(def (r angle)
     (rotate angle))
(def (l angle)
     (rotate (- angle)))

(def w list-repeat) ;; wieder
(def zeig show-svn-logo) ;; zeigen  zeichnen

(def zu closed)

(def (zeig* width #!optional (border 4))
     (save-svn-logo size: (partial-2d-point width #f)
		    border: border))


;; English:

;; start --?
;; push-pos
;; pop-pos
;; dup
;; cursor --? ah  internally gl

(def d draw)
(def j jump)
;; r°
;; l°
;; r
;; l
;; closed
(def sshow show-svn-logo)
(def sshow* zeig*)
;; (z is only defined locally in logo-arrows.scm)
(def rep list-repeat)

;; paintopts




;; to help convert <path d="..."/> into turtle graphics (path-commands):

;; XX hacky
(include "lib.scm")

(def coordinates.2d-points
     (compose (cut map (applying 2d-point) <>) list.pairs))

(TEST
 > (def ps (coordinates.2d-points (list 1 2 4 5 6 5 6 -1)))
 > ps
 ([(2d-point) 1 2] [(2d-point) 4 5] [(2d-point) 6 5] [(2d-point) 6 -1]))


(def (2d-point.path-command a b)
     (list (rotate (- (.angle b) (.angle a)))
	   (draw (.distance b))))


;; (coordinates.2d-points l) XX 
(def (2d-points.path-commands l)
     (let* ((ds (list.diffs (flip .-) l))
	    ;; and then the diff of the diff or so, well.
	    (cs (list.diffs 2d-point.path-command ds)))
       cs))

(TEST
 > (list.diffs (flip .-) ps)
 ([(2d-point) 3 3] [(2d-point) 2 0] [(2d-point) 0 -6])
 > (2d-points.path-commands ps)
 (([(rotate) -.7853981633974483] [(draw) 2])
  ([(rotate) -1.5707963267948966] [(draw) 6])))

(def coordinates.path-commands
     (compose 2d-points.path-commands coordinates.2d-points))
;; but what purpose. also, n-ary.

;;name?
(def (path-coordinates . vs)
     (coordinates.path-commands vs))

(TEST
 > (path-coordinates 1 2 4 5 6 5)
 (([(rotate) -.7853981633974483] [(draw) 2]))
 ;; need at least 3 coordinate pairs, first just used for direction of
 ;; pointer
 > (path-coordinates 1 2 4 5)
 ())

