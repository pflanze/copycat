;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         svg-logo)

(include "lib/cj-standarddeclares.scm")


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
     (rotate (svg-logo#° deg)))
(def (l° deg) ;; links
     (rotate (- (svg-logo#° deg))))

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

