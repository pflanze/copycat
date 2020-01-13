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


(cc-defhost ° ([real? x] -> °?))
(cc-defhost rad ([real? x] -> rad?))
(cc-defhost/try .rad (s))
(cc-defhost/try .degrees (s))

(defparameter turtle-commands '())

(def (get-turtle-commands)
     (reverse (turtle-commands)))

;; all side effects on I/O ~

(cc-def neu (->)
        "starte frische Grafik"
        (turtle-commands '())
        (cc-return))

(cc-def kommandos (-> (list-of logo-command?))
        "zeig die aktuellen Grafik kommandos"
        (cc-return (get-turtle-commands)))

(cc-def zeig (->)
        (copycat:try
         (show-svn-logo (get-turtle-commands))
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
;;(turtle-def-delegate/1 closed)
;;(turtle-def-delegate/1 paintopts)
;;(turtle-def-delegate/1 svgfragment)
(turtle-def-delegate/0 push-pos)
(turtle-def-delegate/0 pop-pos)
(turtle-def-delegate/2 start real? real?)

(cc-defhost/try .neg (x -> x))

(cc-defguest (: drehen [angle? x] -> (rotate))
             (: rechts (drehen))
             (: r (drehen))
             (: °r (° rechts))
             (: links [angle? x] -> (.neg rechts))
             (: °l (° links))
             (: l (links))
             (: forwärts [real? x] -> (draw))
             (: zurück [real? x] -> (neg draw))
             (: f (forwärts))
             (: z (zeig))
             (: k (kommandos))
             (: n (neu)))

