;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         copycat-interpreter
         copycat-std
         svg-logo)

(export)
;; again, exports via side effect.

"Turtle graphics (Logo-inspired)"


(include "lib/cj-standarddeclares.scm")


(definterface angle
  (defclass (° [real? degrees])
    (defmethod (rad s)
      (* degrees (insert-result-of (* (/ 360) 2 pi))))
    (defmethod (neg s)
      (° (- degrees))))
  (defclass (rad [real? rad])
    (defmethod (degrees s)
      (* rad (insert-result-of (/ (* (/ 360) 2 pi)))))
    (defmethod (neg s)
      (rad (- rad)))))

(TEST
 > (=> 180 ° .rad)
 3.141592653589793
 > (=> # rad .degrees)
 180.)


(====cc-category (turtle)
                 "Turtle graphics; see [doc/intro](doc/intro.md).")

(cc-defhost ° ([real? x] -> °?)
            "Wrap `x` in an object denoting an angle in degrees.")
(cc-defhost rad ([real? x] -> rad?)
            "Wrap `x` in an object denoting an angle in radians.")
(cc-defhost/try .rad (s)
                "Convert s (e.g. a rad or ° object) to a bare number
in radians.")
(cc-defhost/try .degrees (s)
                "Convert s (e.g. a rad or ° object) to a bare number
in degrees.")

(defparameter turtle-commands '())

(def (get-turtle-commands)
     (reverse (turtle-commands)))

;; all side effects on I/O ~

(cc-def new (->)
        "Delete turtle canvas, set position to the center, and point
up (north)."
        (turtle-commands '())
        (cc-return))

(cc-def commands (-> (list-of logo-command?))
        "Get the currently accumulated turtle commands."
        (cc-return (get-turtle-commands)))

(cc-def view (->)
        "Show the currently accumulated turtle commands (generate SVG
graph, open viewer if running under X-windows)."
        (copycat:try
         (save&possibly-show-svn-logo (and (getenv "DISPLAY" #f) #t)
                                      (get-turtle-commands))
         (cc-return)))

;; 'delegates'

(defmacro (def-turtle-delegate/0 name docstring)
  `(cc-def ,name (->)
           ,docstring
           (parameter-push! turtle-commands (,name))
           (cc-return)))

(defmacro (def-turtle-delegate/1 name type docstring)
  `(cc-def ,name ([,type x] ->)
           ,docstring
           (parameter-push! turtle-commands (,name x))
           (cc-return)))

(defmacro (def-turtle-delegate/2 name type1 type2 docstring)
  `(cc-def ,name ([,type1 x] [,type2 y] ->)
           ,docstring
           (parameter-push! turtle-commands (,name x y))
           (cc-return)))


(def-turtle-delegate/1 draw real?
  "Draw a stroke of the given length.")
(def-turtle-delegate/1 jump real?
  "Move the given length without drawing.")
(cc-def rotate ([angle? x])
        (parameter-push! turtle-commands (rotate (.rad x)))
        (cc-return))

;; oops, and we are in first-class approach (monad)
;; XX try it out...
(cc-def closed ([(list-of path-command?) l] -> path-command?)
        ;;"XX "
        (cc-return (_closed l)))

(cc-def paintopts ([(list-of path-command?) l] [paintoptions? opts]
                   -> path-command?)
        (cc-return (_paintopts opts l)))

;;(def-turtle-delegate/1 svgfragment "...")

;; XX and/but these *currently* (even, already) don't allow to inspect
;; the pushed state.
(def-turtle-delegate/0 push-pos
  "Push the current position onto a separate position stack.")
(def-turtle-delegate/0 pop-pos
  "Pop the position that was last pushed to the position stack.")

(def-turtle-delegate/2 start real? real?
  "Set (x,y) position to the given numbers.")


(cc-defguest 'right 'rotate alias
             (: left [angle? x] -> (.neg right))
             (: r (° right))
             (: l (° left))
             'forward 'draw alias
             (: back [real? x] -> (neg draw))
             'f 'forward alias
             'b 'back alias
             'j 'jump alias)


(====cc-category (turtle DE)
                 "German translations.")

(cc-defguest 'drehen 'rotate alias
             'links 'left alias
             'rechts 'drehen alias
             'vorwärts 'forward alias
             'v 'vorwärts alias
             'zurück 'back alias
             'neu 'new alias
             'zeig 'view alias
             'kommandos 'commands alias)


(====cc-category) ;; prevent the previous setting from being carried
                  ;; over to other modules (meh)
