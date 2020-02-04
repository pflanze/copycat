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
         svg-logo-1
         svg-logo-2)

(export save-svn-logo
        list-repeat
        save&possibly-show-svn-logo
        show-svn-logo
        save-svn-logo ;; without cursor!
        ;; aliases
        ;;p

        ;; re-exports:
        svg-logo#°
        north-angle
        svn-logo-process
        (class turtlepos)
        (class drawing-state)
        (interface logo-command
          (class start)
          (class push-pos)
          (class pop-pos)
          (class svg-logo#dup)
          (class cursor)
          (interface path-command
            (class draw)
            (class jump)
            (class rotate)
            (class closed)
            (class paintopts)
            (class svgfragment)))

        #!optional
        default-drawing-state
        coordinates.2d-points
        2d-point.path-command
        2d-points.path-commands
        coordinates.path-commands
        path-coordinates)


(include "lib/cj-standarddeclares.scm")


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

(defparameter svg-logo:current-path "logo.svg") ;; path-string?
(defparameter svg-logo:current-viewer-cmd '("eog" "--new-instance"))
;; (nonempty-list-of string?) # or, #f hole for path? function?

(def svg-logo:viewer-instances ;; path => future
     ;; XX mutex wrapper needed
     (table))


(defmacro (while test . body)
  (with-gensym
   LP
   `(let ,LP ()
         (when ,test
           ,@body
           (,LP)))))

(def (svg-logo:possibly-start-viewer-for [path-string? svg-logo-path])
     (let (t svg-logo:viewer-instances)
       (or (table-ref t svg-logo-path #f)
           (let (v
                 (let-pair ((cmdpath cmdargs) (svg-logo:current-viewer-cmd))
                           (future
                            (let (p (open-process
                                     `(
                                       path:
                                       ,cmdpath
                                       arguments:
                                       ,(append cmdargs
                                                (list svg-logo-path))
                                       stderr-redirection: #t
                                       stdout-redirection: #t)))
                              (future
                               ;; redirect to dev null instead?
                               (while (not (eof-object? (read-line p)))))
                              ;; HACK to try to avoid race....
                              (thread-sleep! 1)
                              (process-status p)
                              (table-delete! t svg-logo-path)))))
             (table-set! t svg-logo-path v)
             v))))

(defparameter svg-logo:canvas-length 800)

(def (save&possibly-show-svn-logo show? cmds)
     (let (path (svg-logo:current-path))
       (.sxml-file (svg (2d-point (svg-logo:canvas-length)
                                  (svg-logo:canvas-length))
                        (2d-window (2d-point -50 -50)
                                   (2d-point 150 150))
                        (svn-logo-process (flatten
                                           (list cmds
                                                 (cursor)))
                                          default-drawing-state)
                        background-color: (colorstring "white"))
                   path)
       (when show?
         (svg-logo:possibly-start-viewer-for path))))

(def (show-svn-logo . cmds)
     (save&possibly-show-svn-logo #t cmds))


(def (save-svn-logo . options)
     (lambda cmds
       (apply showsvg
	      (svn-logo-process (flatten cmds)
				default-drawing-state)
	      options)))



;; paintopts




;; to help convert <path d="..."/> into turtle graphics (path-commands):

(def coordinates.2d-points
     (compose (cut map (applying 2d-point) <>) list.pairs))

(TEST
 > (def ps (coordinates.2d-points (list 1 2 4 5 6 5 6 -1)))
 > (.show ps)
 (list (2d-point 1 2) (2d-point 4 5) (2d-point 6 5) (2d-point 6 -1)))


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
 > (.show (list.diffs (flip .-) ps))
 (list (2d-point 3 3) (2d-point 2 0) (2d-point 0 -6))
 > (.show (2d-points.path-commands ps))
 (list (list (rotate -.7853981633974483) (draw 2))
       (list (rotate -1.5707963267948966) (draw 6))))

(def coordinates.path-commands
     (compose 2d-points.path-commands coordinates.2d-points))
;; but what purpose. also, n-ary.

;;name?
(def (path-coordinates . vs)
     (coordinates.path-commands vs))

(TEST
 > (.show (path-coordinates 1 2 4 5 6 5))
 (list (list (rotate -.7853981633974483) (draw 2)))
 ;; need at least 3 coordinate pairs, first just used for direction of
 ;; pointer
 > (path-coordinates 1 2 4 5)
 ())

