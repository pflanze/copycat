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
         (table-1 table-delete!))


(export svg-logo#°
        north-angle
        (interface logo-command)
        (class turtlepos)
        (class drawing-state)
        (interface logo-command
          (class start)
          (class push-pos)
          (class pop-pos)
          (class svg-logo#dup)
          (class cursor)))


(include "lib/cj-standarddeclares.scm")


;; (def p 2d-point)


(def (svg-logo#° deg)
     (* (/ deg 360) 2 pi))

(TEST
 > (svg-logo#° 180)
 3.141592653589793)


(def north-angle (svg-logo#° -90))



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


(definterface logo-command)


(defclass ((svg-logo-1 #f))
  implements: logo-command

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
                (=> state
                    (.turtlepos-set (car stack))
                    (.stack-set (cdr stack)))
                (error "svg-logo stack is empty")))))

  (defclass (svg-logo#dup)
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
                       (.point (2d-polar angle 3)))))
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
             _)))))))
