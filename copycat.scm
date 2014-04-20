;; Cat: a concatenative language using S-expressions

;; This implements a concatenative ("Forth-like") language. It uses
;; S-expressions for basic syntax, and deviates from normal Forth by
;; using lists to quote subprograms, and maybe a few other changes. I
;; think it is closer to [Joy][] than Forth.
;; 
;; [Joy]: https://en.wikipedia.org/wiki/Joy_(programming_language),
;;        http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language
;; [Forth]: https://en.wikipedia.org/wiki/Forth_(programming_language)


;; table to store the values of words
(def cat-words
     (make-table))

;; Note: currently the cat-words table is used for every word
;; reference at runtime. This could be made more efficient by moving
;; the lookups to a parsing step (symbol creation, store as part of
;; symbol data structure). ("Linker step")

(def (cat-word-set! #(symbol? name) val)
     (table-set! cat-words name val))


;; We need a way to fall back onto the host system to implement
;; primitives, thus provide part of a foreign function interface:

(defstruct catforeigncall
  #(natural0? numargs)
  #(procedure? op))

;; setting a word to a Scheme program
(defmacro (cat-def name args . body)
  (assert* symbol? name
	   (lambda_
	    `(cat-word-set! ',name
			      (catforeigncall
			       ,(length (source-code args))
			       (lambda ,(cons '$s (source-code args))
				 ;; ^ HEH that |source-code| is
				 ;; required. otherwise gambit has a
				 ;; problem, 'Identifier expected'
				 ,@body))))))

(defmacro (cat-return . es)
  `(cons* ,@(reverse es) $s))

(defmacro (cat-defhost name args)
  `(cat-def ,name ,args
	      (cat-return ,(cons name (source-code args)))))

;; -- functions

(cat-defhost + (a b))
(cat-defhost - (a b))
(cat-defhost * (a b))
(cat-defhost / (a b))

(cat-def dup (a)
	   (cat-return a a))

(cat-def drop (a)
	   $s)

(cat-def swap (a b)
	   (cat-return b a))

(cat-defhost zero? (v))
(cat-defhost = (a b))
(cat-defhost < (a b))
(cat-defhost <= (a b))
(cat-defhost > (a b))
(cat-defhost >= (a b))
(cat-def != (a b)
	   (cat-return (not (= a b))))
(cat-defhost eq? (a b))
(cat-def !eq? (a b)
	   (cat-return (not (eq? a b))))

(cat-def rot (n)
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


;; -- procedures (for side-effects)

(cat-def eval (v)
	   (cat-eval $s v))

(cat-def nop ()
	   $s)

(cat-def set! (prog name)
	   (cat-word-set! name prog)
	   $s)

(cat-def ref (name)
	   (cat-return (table-ref cat-words name)))

(cat-def thenelse (val truebranch falsebranch)
	   (cat-eval $s (if val truebranch falsebranch)))

(cat-def print (v)
	   (print v)
	   $s)

(cat-def write (v)
	   (write v)
	   $s)

(cat-def newline ()
	   (newline)
	   $s)

(cat-def println (v)
	   (println v)
	   $s)

;; ----------------------------

(def (cat-apply stack #(symbol? word))
     (let ((w (table-ref cat-words word)))
       (if (catforeigncall? w)
	   (let-catforeigncall
	    ((numargs op) w)
	    (case numargs
	      ((0) (op stack))
	      ((1) (op (cdr stack) (car stack)))
	      ((2) (op (cddr stack) (cadr stack) (car stack)))
	      (else
	       ;; split-at-reverse?
	       (letv ((args stack) (split-at stack numargs))
		     (apply op (cons stack (reverse args)))))))
	   (cat-eval stack w))))

(def (cat-eval stack prog)
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
			  (cat-word-set! name subprog)
			  (cat-eval stack cont)))
		       ((THENELSE)
			;; takes 2 arguments from program (truebranch,
			;; falsebranch), and 1 from stack (test value)
			(let ((cont (cddr prog*)))
			  (cat-eval (cat-eval (cdr stack)
						  (if (car stack)
						      (car prog*)
						      (cadr prog*)))
				      cont)))
		       ((QUOTE)
			;; takes 1 argument from program, puts it on
			;; the stack
			(let ((cont (cdr prog*)))
			  (cat-eval (cons (car prog*) stack) cont)))
		       (else
			(let ((app (thunk (cat-apply stack item))))
			  (if (null? prog*)
			      (app)
			      (cat-eval (app) prog*))))))
		    (else
		     (cat-eval (cons item stack) prog*))))))

(TEST
 > (cat-eval '() '(4 5 5 *))
 (25 4)
 > (cat-eval '() '(4 5 5 * -))
 (-21)
 > (cat-eval '() '(4 5 dup * -))
 (-21)
 > (cat-eval '() '(4 5 swap dup * -))
 (-11)

 ;; sublists are representing sub-programs, which are only evaluated
 ;; on demand:
 > (cat-eval '() '(4 (5) eval))
 (5 4)
 > (cat-eval '() '(4 (5 1 +) eval))
 (6 4)

 ;; words can be quoted by way of QUOTE:
 > (cat-eval '() '(QUOTE 1))
 (1)
 > (cat-eval '() '(QUOTE foo))
 (foo)

 ;; "syntax-based" word definition form: |:| takes a name, and a
 ;; program to its right, syntactically
 > (cat-eval '() '(: square (dup *) 4 square))
 (16)
 ;; stack-based word definition form (works like a normal word):
 ;; |set!| takes a program and a name from the stack at runtime. (I
 ;; don't know why the original Cat chooses to use such
 ;; "syntax-based" features when it could do with program and symbol
 ;; quoting and then just words like this, other than visual
 ;; preference.)
 > (cat-eval '() '((dup *) QUOTE sqr set! 4 sqr))
 (16)

 ;; "syntax-based" branching facility: takes a truebranch and a
 ;; falsebranch to its right, syntactically, as well as a boolean
 ;; value from the stack at runtime.
 > (cat-eval '(5) '(zero?))
 (#f)
 > (cat-eval '(5) '(zero? THENELSE (1) (0)))
 (0)
 > (cat-eval '(0) '(zero? THENELSE (1) (0)))
 (1)
 ;; stack-based branching facility (works like a normal word): takes
 ;; boolean value, truebranch and falsebranch from the stack at
 ;; runtime
 > (cat-eval '(5) '(zero? (1) (0) thenelse))
 (0)
 > (cat-eval '(0) '(zero? (1) (0) thenelse))
 (1)
 ;; rot takes a number denoting the number of elements to rotate, and
 ;; rotates their position on the stack so that the last of those
 ;; becomes the first:
 > (cat-eval '((no) (yes) #t 7) '(3 rot))
 (#t (no) (yes) 7)
 ;; write a word-based branching facility ourselves, using the
 ;; syntax-based one internally:
 > (cat-eval '() '((3 rot THENELSE (drop eval) (swap drop eval))
		     QUOTE if set!))
 > (cat-eval '(5) '(zero? (1) (0) if))
 (0)
 > (cat-eval '(0) '(zero? (1) (0) if))
 (1)
 ;; write a word-based branching facility ourselves, using the
 ;; stack-based one internally:
 > (cat-eval '() '((thenelse) QUOTE if* set!))
 > (cat-eval '(5) '(zero? (1) (0) if*))
 (0)
 > (cat-eval '(0) '(zero? (1) (0) if*))
 (1)
 ;; alias the branching facility by simply storing it to a different
 ;; word:
 > (cat-eval '() '(QUOTE if* ref QUOTE anotherif set!))
 > (cat-eval '(5) '(zero? (1) (0) anotherif))
 (0)
 > (cat-eval '(0) '(zero? (1) (0) anotherif))
 (1)

 ;; factorial
 > (cat-eval '(0) '(: fact (dup zero? THENELSE (drop 1) (dup 1 - fact *))))
 ;; or:
 > (cat-eval '(0) '(: fact (dup zero? (drop 1) (dup 1 - fact *) thenelse)))
 > (cat-eval '(0) '(fact))
 (1)
 > (cat-eval '(1) '(fact))
 (1)
 > (cat-eval '(2) '(fact))
 (2)
 > (cat-eval '(3) '(fact))
 (6)
 > (cat-eval '(20) '(fact))
 (2432902008176640000)
 )
