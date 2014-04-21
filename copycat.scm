
(def forth-words
     (make-table))

(defstruct forthword
  #(symbol? origname)
  #(natural0? numargs)
  #(procedure? op))

(defmacro (forth-def name args . body)
  (assert* symbol? name
	   (lambda_
	    `(table-set! forth-words
			 ',name
			 (forthword ',name
				    ,(length (source-code args))
				    (lambda ,(cons '$s (source-code args))
				      ;; ^ HEH that |source-code| is
				      ;; required. otherwise gambit has a
				      ;; problem, 'Identifier expected'
				      ,@body))))))

(defmacro (forth-return . es)
  `(cons* ,@(reverse es) $s))


(forth-def + (a b)
	   (forth-return (+ a b)))

(forth-def - (a b)
	   (forth-return (- a b)))

(forth-def * (a b)
	   (forth-return (* a b)))

(forth-def / (a b)
	   (forth-return (/ a b)))

(forth-def dup (a)
	   (forth-return a a))

(forth-def swap (a b)
	   (forth-return b a))

(forth-def print (v)
	   (print v)
	   $s)

(forth-def write (v)
	   (write v)
	   $s)

(forth-def newline ()
	   (newline)
	   $s)

(forth-def println (v)
	   (println v)
	   $s)

(forth-def eval (v)
	   (forth-eval $s v))

(def (forth-apply stack #(symbol? word))
     (let-forthword
      ((origname numargs op) (table-ref forth-words word))
      (case numargs
	((0) (op stack))
	((1) (op (cdr stack) (car stack)))
	((2) (op (cddr stack) (cadr stack) (car stack)))
	(else
	 ;; split-at-reverse?
	 (letv ((args stack) (split-at stack numargs))
	       (apply op (cons stack (reverse args))))))))

(def (forth-eval stack prog)
     (if (null? prog)
	 stack
	 (let-pair ((item prog*) prog)
		   (cond ((eq? item ':)
			  ;; takes 2 arguments from program (name,
			  ;; prog), not stack
			  (let ((name (car prog*))
				(subprog (cadr prog*)))
			    (table-set! forth-words
					name
					(forthword name
						   0
						   (lambda ($s)
						     (forth-eval $s subprog))))
			    (forth-eval stack (cddr prog*))))
			 ((symbol? item)
			  (forth-eval (forth-apply stack item) prog*))
			 (else
			  (forth-eval (cons item stack) prog*))))))

(TEST
 > (forth-eval '() '(4 5 5 *))
 (25 4)
 > (forth-eval '() '(4 5 5 * -))
 (-21)
 > (forth-eval '() '(4 5 dup * -))
 (-21)
 > (forth-eval '() '(4 5 swap dup * -))
 (-11)
 > (forth-eval '() '(4 (5) eval))
 (5 4)
 > (forth-eval '() '(4 (5 1 +) eval))
 (6 4)
 > (forth-eval '() '(: square (dup *) 4 square))
 (16)
 )
