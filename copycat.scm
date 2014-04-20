
(def forth-words
     (make-table))

;; Note: currently the forth-words table is used for every word
;; reference at runtime. This could be made more efficient by moving
;; the lookups to a parsing step (symbol creation, store as part of
;; symbol data structure). ("Linker step")

(defstruct forthforeigncall
  #(natural0? numargs)
  #(procedure? op))

;; (defstruct forthprogram
;;   list)
;; no, just store the list directly. Let instead forthforeigncall be a
;; type visible by Forth.

;; setting a word with a Forth program
(def (forth-word-set! #(symbol? name) val)
     (table-set! forth-words name val))

;; setting a word to a Scheme program
(defmacro (forth-def name args . body)
  (assert* symbol? name
	   (lambda_
	    `(forth-word-set! ',name
			      (forthforeigncall
			       ,(length (source-code args))
			       (lambda ,(cons '$s (source-code args))
				 ;; ^ HEH that |source-code| is
				 ;; required. otherwise gambit has a
				 ;; problem, 'Identifier expected'
				 ,@body))))))

(defmacro (forth-return . es)
  `(cons* ,@(reverse es) $s))

(defmacro (forth-defhost name args)
  `(forth-def ,name ,args
	      (forth-return ,(cons name (source-code args)))))

;; -- functions

(forth-defhost + (a b))
(forth-defhost - (a b))
(forth-defhost * (a b))
(forth-defhost / (a b))

(forth-def dup (a)
	   (forth-return a a))

(forth-def drop (a)
	   $s)

(forth-def swap (a b)
	   (forth-return b a))

(forth-defhost zero? (v))
(forth-defhost = (a b))
(forth-defhost < (a b))
(forth-defhost <= (a b))
(forth-defhost > (a b))
(forth-defhost >= (a b))
(forth-def != (a b)
	   (forth-return (not (= a b))))
(forth-defhost eq? (a b))
(forth-def !eq? (a b)
	   (forth-return (not (eq? a b))))

;; eval
(forth-def eval (v)
	   (forth-eval $s v))


;; -- side-effecting statements

(forth-def nop ()
	   $s)

(forth-def rot (n)
	   ;; (letv ((args stack*) (split-at $s n))
	   ;; 	 (append (cons (last args) (butlast args)) stack*))
	   ;;or, saving on intermediates:
	   (let lp ((n n)
		    (tmp '())
		    (stack $s))
	     (if (> n 1)
		 (lp (dec n)
		     (cons (car stack) tmp)
		     (cdr stack))
		 (cons (car stack)
		       (rappend tmp (cdr stack))))))

(forth-def set! (prog name)
	   (forth-word-set! name prog)
	   $s)

(forth-def ref (name)
	   (forth-return (table-ref forth-words name)))

(forth-def thenelse (val truebranch falsebranch)
	   (forth-eval $s (if val truebranch falsebranch)))

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
     (let ((w (table-ref forth-words word)))
       (if (forthforeigncall? w)
	   (let-forthforeigncall
	    ((numargs op) w)
	    (case numargs
	      ((0) (op stack))
	      ((1) (op (cdr stack) (car stack)))
	      ((2) (op (cddr stack) (cadr stack) (car stack)))
	      (else
	       ;; split-at-reverse?
	       (letv ((args stack) (split-at stack numargs))
		     (apply op (cons stack (reverse args)))))))
	   (forth-eval stack w))))

(def (forth-eval stack prog)
     (if (null? prog)
	 stack
	 (let-pair ((item prog*) prog)
		   (cond 
		    ((symbol? item)
		     ;; check for special syntax (XX should this be
		     ;; made extensible at runtime by using special
		     ;; word values?)
		     (case item
		       ((:)
			;; takes 2 arguments from program (name,
			;; prog), not stack
			(let ((name (car prog*))
			      (subprog (cadr prog*))
			      (cont (cddr prog*)))
			  (forth-word-set! name subprog)
			  (forth-eval stack cont)))
		       ((THENELSE)
			;; takes 2 arguments from program (truebranch,
			;; falsebranch), and 1 from stack (test value)
			(let ((cont (cddr prog*)))
			  (forth-eval (forth-eval (cdr stack)
						  (if (car stack)
						      (car prog*)
						      (cadr prog*)))
				      cont)))
		       ((QUOTE)
			;; takes 1 argument from program, puts it on
			;; the stack
			(let ((cont (cdr prog*)))
			  (forth-eval (cons (car prog*) stack) cont)))
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

 ;; sublists are representing sub-programs, which are only evaluated
 ;; on demand:
 > (forth-eval '() '(4 (5) eval))
 (5 4)
 > (forth-eval '() '(4 (5 1 +) eval))
 (6 4)

 ;; words can be quoted by way of QUOTE:
 > (forth-eval '() '(QUOTE 1))
 (1)
 > (forth-eval '() '(QUOTE foo))
 (foo)

 ;; "syntax-based" word definition form: |:| takes a name, and a
 ;; program to its right, syntactically
 > (forth-eval '() '(: square (dup *) 4 square))
 (16)
 ;; stack-based word definition form (works like a normal word):
 ;; |set!| takes a program and a name from the stack at runtime. (I
 ;; don't know why the original Forth chooses to use such
 ;; "syntax-based" features when it could do with program and symbol
 ;; quoting and then just words like this, other than visual
 ;; preference.)
 > (forth-eval '() '((dup *) QUOTE sqr set! 4 sqr))
 (16)

 ;; "syntax-based" branching facility: takes a truebranch and a
 ;; falsebranch to its right, syntactically, as well as a boolean
 ;; value from the stack at runtime.
 > (forth-eval '(5) '(zero?))
 (#f)
 > (forth-eval '(5) '(zero? THENELSE (1) (0)))
 (0)
 > (forth-eval '(0) '(zero? THENELSE (1) (0)))
 (1)
 ;; stack-based branching facility (works like a normal word): takes
 ;; boolean value, truebranch and falsebranch from the stack at
 ;; runtime
 > (forth-eval '(5) '(zero? (1) (0) thenelse))
 (0)
 > (forth-eval '(0) '(zero? (1) (0) thenelse))
 (1)
 ;; rot takes a number denoting the number of elements to rotate, and
 ;; rotates their position on the stack so that the last of those
 ;; becomes the first:
 > (forth-eval '((no) (yes) #t 7) '(3 rot))
 (#t (no) (yes) 7)
 ;; write a word-based branching facility ourselves, using the
 ;; syntax-based one internally:
 > (forth-eval '() '((3 rot THENELSE (drop eval) (swap drop eval))
		     QUOTE if set!))
 > (forth-eval '(5) '(zero? (1) (0) if))
 (0)
 > (forth-eval '(0) '(zero? (1) (0) if))
 (1)
 ;; write a word-based branching facility ourselves, using the
 ;; stack-based one internally:
 > (forth-eval '() '((thenelse) QUOTE if* set!))
 > (forth-eval '(5) '(zero? (1) (0) if*))
 (0)
 > (forth-eval '(0) '(zero? (1) (0) if*))
 (1)
 ;; alias the branching facility by simply storing it to a different
 ;; word:
 > (forth-eval '() '(QUOTE if* ref QUOTE anotherif set!))
 > (forth-eval '(5) '(zero? (1) (0) anotherif))
 (0)
 > (forth-eval '(0) '(zero? (1) (0) anotherif))
 (1)

 ;; factorial
 > (forth-eval '(0) '(: fact (dup zero? THENELSE (drop 1) (dup 1 - fact *))))
 ;; or:
 > (forth-eval '(0) '(: fact (dup zero? (drop 1) (dup 1 - fact *) thenelse)))
 > (forth-eval '(0) '(fact))
 (1)
 > (forth-eval '(1) '(fact))
 (1)
 > (forth-eval '(2) '(fact))
 (2)
 > (forth-eval '(3) '(fact))
 (6)
 > (forth-eval '(20) '(fact))
 (2432902008176640000)
 )
