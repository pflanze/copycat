
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

;; -- functions

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

(forth-def zero? (v)
	   (forth-return (zero? v)))

(forth-def = (a b)
	   (forth-return (= a b)))

;; eval
(forth-def eval (v)
	   (forth-eval $s v))


;; -- side-effecting statements

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

;; ----------------------------

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
		   (cond 
		    ((symbol? item)
		     (case item
		       ((:)
			;; takes 2 arguments from program (name,
			;; prog), not stack
			(let ((name (car prog*))
			      (subprog (cadr prog*))
			      (cont (cddr prog*)))
			  (table-set! forth-words
				      name
				      (forthword name
						 0
						 (lambda ($s)
						   (forth-eval $s subprog))))
			  (forth-eval stack cont)))
		       ((thenelse)
			;; takes 2 arguments from program (truebranch,
			;; falsebranch), and 1 from stack (test value)
			(let ((cont (cddr prog*)))
			  (forth-eval (forth-eval (cdr stack)
						  (if (car stack)
						      (car prog*)
						      (cadr prog*)))
				      cont)))
		       (else
			(let ((app (thunk (forth-apply stack item))))
			  (if (null? prog*)
			      (app)
			      (forth-eval (app) prog*))))))
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
 > (forth-eval '(5) '(zero?))
 (#f)
 > (forth-eval '(5) '(zero? thenelse (1) (0)))
 (0)
 > (forth-eval '(0) '(zero? thenelse (1) (0)))
 (1)
 )
