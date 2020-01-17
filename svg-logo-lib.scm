(require easy test)

;XXX move elsewhere

(include "lib/cj-standarddeclares.scm")


;; XX move to lib  svg or so  where ° is   inverse of °
(def (radian->° angle)
     (* 360 (/ angle 2 pi)))


;; XX surely have this already.  ?
(def (list.pairs l)
     (if (null? l)
	 '()
	 (let-pair ((a l) l)
		   (if (null? l)
		       (error "list does not contain even number of elements")
		       (let-pair ((b l) l)
				 (cons (list a b)
				       (list.pairs l)))))))

(TEST
 > (list.pairs '())
 ()
 > (list.pairs '(1 2))
 ((1 2))
 > (%try-error (list.pairs '(1 2 3)))
 [error "list does not contain even number of elements"]
 > (list.pairs '(1 2 3 4))
 ((1 2) (3 4)))


;; double-pairings i mean step 1 pairing vs above which is step 2
;; pairing. Otherwise the same, identical hm?
(def (list.diffs diff l)
     (if (null? l)
	 '()
	 (let-pair ((a l*) l)
		   (if (null? l*)
		       '()
		       (let-pair ((b l**) l*)
				 (cons (diff a b)
				       (list.diffs diff l*)))))))

(TEST
 ;; is that ok to require flip ? old question. Where was it last time?
 > (list.diffs (flip -) '(1 3 4 7 6 7))
 (2 1 3 -1 1))

