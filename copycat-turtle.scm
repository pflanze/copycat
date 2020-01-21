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
                 "turtle graphics; see doc/intro")

(cc-defhost ° ([real? x] -> °?))
(cc-defhost rad ([real? x] -> rad?))
(cc-defhost/try .rad (s))
(cc-defhost/try .degrees (s))

(defparameter turtle-commands '())

(def (get-turtle-commands)
     (reverse (turtle-commands)))

;; all side effects on I/O ~

(cc-def new (->)
        "starte frische Grafik"
        (turtle-commands '())
        (cc-return))

(cc-def commands (-> (list-of logo-command?))
        "zeig die aktuellen Grafik kommandos"
        (cc-return (get-turtle-commands)))

(cc-def view (->)
        (copycat:try
         (save&possibly-show-svn-logo (and (getenv "DISPLAY" #f) #t)
                                      (get-turtle-commands))
         (cc-return)))

(cc-def sleep ([nonnegative-real? seconds] ->)
        (thread-sleep! seconds)
        (cc-return))

;; 'delegates'

(defmacro (turtle-def-delegate/0 name)
  `(cc-def ,name (->)
           (parameter-push! turtle-commands (,name))
           (cc-return)))

(defmacro (turtle-def-delegate/1 name type)
  `(cc-def ,name ([,type x] ->)
           (parameter-push! turtle-commands (,name x))
           (cc-return)))

(defmacro (turtle-def-delegate/2 name type1 type2)
  `(cc-def ,name ([,type1 x] [,type2 y] ->)
           (parameter-push! turtle-commands (,name x y))
           (cc-return)))


(turtle-def-delegate/1 draw real?)
(turtle-def-delegate/1 jump real?)
(cc-def rotate ([angle? x])
        (parameter-push! turtle-commands (rotate (.rad x)))
        (cc-return))

;; oops, and we are in first-class approach (monad)
;; XX try it out...
(cc-def closed ([(list-of path-command?) l] -> path-command?)
        ""
        (cc-return (_closed l)))

(cc-def paintopts ([(list-of path-command?) l] [paintoptions? opts]
                   -> path-command?)
        (cc-return (_paintopts opts l)))

;;(turtle-def-delegate/1 svgfragment)

;; XX and/but these *currently* (even, already) don't allow to inspect
;; the pushed state.
(turtle-def-delegate/0 push-pos)
(turtle-def-delegate/0 pop-pos)

(turtle-def-delegate/2 start real? real?)


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
                 "german translations")

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
