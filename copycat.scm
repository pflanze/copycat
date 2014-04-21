;; Copycat: a concatenative language using S-expressions

;; This implements a concatenative ("Forth-like") language. It uses
;; S-expressions for basic syntax, and deviates from [Forth][] by
;; using lists to quote subprograms, and maybe a few other changes. I
;; think it is closest to [Joy][], or [tcK][]. See also [Cat][].

;; [Forth]: https://en.wikipedia.org/wiki/Forth_(programming_language)
;; [Joy]: https://en.wikipedia.org/wiki/Joy_(programming_language),
;;        http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language
;; [tcK]: "tiny concatenative K", http://archive.vector.org.uk/art10000360
;; [Cat]: http://www.cat-language.com/


;; table to store the values of words
(def copycat-words
     (make-table))
;; XX: Forth has a *tree* of binding maps? Context dependent? What kind
;; of context? Here it's just one map.

;; Note: currently the copycat-words table is used for every word
;; reference at runtime. This could be made more efficient by moving
;; the lookups to a parsing step (symbol creation, store as part of
;; symbol data structure). ("Linker step")

(def (copycat-word-set! #(symbol? name) val)
     (table-set! copycat-words name val))


;; We need a way to fall back onto the host system to implement
;; primitives, thus provide part of a foreign function interface:

(defstruct copycatforeigncall
  #(natural0? numargs)
  #(procedure? op))

;; setting a word to a Scheme program
(defmacro (copycat-def name args . body)
  (assert* symbol? name
	   (lambda_
	    `(copycat-word-set! ',name
			      (copycatforeigncall
			       ,(length (source-code args))
			       (lambda ,(cons '$s (source-code args))
				 ;; ^ HEH that |source-code| is
				 ;; required. otherwise gambit has a
				 ;; problem, 'Identifier expected'
				 ,@body))))))

(defmacro (copycat-return . es)
  `(cons* ,@(reverse es) $s))

(defmacro (copycat-defhost name args)
  `(copycat-def ,name ,args
	      (copycat-return ,(cons name (source-code args)))))

;; -- functions

(copycat-defhost + (a b))
(copycat-defhost - (a b))
(copycat-defhost * (a b))
(copycat-defhost / (a b))

(copycat-def dup (a)
	   (copycat-return a a))

(copycat-def drop (a)
	   $s)

(copycat-def swap (a b)
	   (copycat-return b a))

(copycat-defhost zero? (v))
(copycat-defhost = (a b))
(copycat-defhost < (a b))
(copycat-defhost <= (a b))
(copycat-defhost > (a b))
(copycat-defhost >= (a b))
(copycat-def != (a b)
	   (copycat-return (not (= a b))))
(copycat-defhost eq? (a b))
(copycat-def !eq? (a b)
	   (copycat-return (not (eq? a b))))

(copycat-def rot (n)
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

(copycat-def eval (v)
	   (copycat-eval $s v))

(copycat-def nop ()
	   $s)

(copycat-def set! (prog name)
	   (copycat-word-set! name prog)
	   $s)

(copycat-def ref (name)
	   (copycat-return (table-ref copycat-words name)))

(copycat-def thenelse (val truebranch falsebranch)
	   (copycat-eval $s (if val truebranch falsebranch)))

(copycat-def print (v)
	   (print v)
	   $s)

(copycat-def write (v)
	   (write v)
	   $s)

(copycat-def newline ()
	   (newline)
	   $s)

(copycat-def println (v)
	   (println v)
	   $s)

;; ----------------------------

(def (copycat-apply stack #(symbol? word))
     (let ((w (table-ref copycat-words word)))
       (if (copycatforeigncall? w)
	   (let-copycatforeigncall
	    ((numargs op) w)
	    (case numargs
	      ((0) (op stack))
	      ((1) (op (cdr stack) (car stack)))
	      ((2) (op (cddr stack) (cadr stack) (car stack)))
	      (else
	       ;; split-at-reverse?
	       (letv ((args stack) (split-at stack numargs))
		     (apply op (cons stack (reverse args)))))))
	   (copycat-eval stack w))))

(def (copycat-eval stack prog)
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
			  (copycat-word-set! name subprog)
			  (copycat-eval stack cont)))
		       ((THENELSE)
			;; takes 2 arguments from program (truebranch,
			;; falsebranch), and 1 from stack (test value)
			(let ((cont (cddr prog*)))
			  (copycat-eval (copycat-eval (cdr stack)
						  (if (car stack)
						      (car prog*)
						      (cadr prog*)))
				      cont)))
		       ((QUOTE)
			;; takes 1 argument from program, puts it on
			;; the stack
			(let ((cont (cdr prog*)))
			  (copycat-eval (cons (car prog*) stack) cont)))
		       (else
			(let ((app (thunk (copycat-apply stack item))))
			  (if (null? prog*)
			      (app)
			      (copycat-eval (app) prog*))))))
		    (else
		     (copycat-eval (cons item stack) prog*))))))

(TEST
 > (copycat-eval '() '(4 5 5 *))
 (25 4)
 > (copycat-eval '() '(4 5 5 * -))
 (-21)
 > (copycat-eval '() '(4 5 dup * -))
 (-21)
 > (copycat-eval '() '(4 5 swap dup * -))
 (-11)

 ;; sublists are representing sub-programs, which are only evaluated
 ;; on demand:
 > (copycat-eval '() '(4 (5) eval))
 (5 4)
 > (copycat-eval '() '(4 (5 1 +) eval))
 (6 4)

 ;; words can be quoted by way of QUOTE:
 > (copycat-eval '() '(QUOTE 1))
 (1)
 > (copycat-eval '() '(QUOTE foo))
 (foo)

 ;; "syntax-based" word definition form: |:| takes a name, and a
 ;; program to its right, syntactically
 > (copycat-eval '() '(: square (dup *) 4 square))
 (16)
 ;; stack-based word definition form (works like a normal word):
 ;; |set!| takes a program and a name from the stack at runtime. (I
 ;; don't know why the original Copycat chooses to use such
 ;; "syntax-based" features when it could do with program and symbol
 ;; quoting and then just words like this, other than visual
 ;; preference.)
 > (copycat-eval '() '((dup *) QUOTE sqr set! 4 sqr))
 (16)

 ;; "syntax-based" branching facility: takes a truebranch and a
 ;; falsebranch to its right, syntactically, as well as a boolean
 ;; value from the stack at runtime.
 > (copycat-eval '(5) '(zero?))
 (#f)
 > (copycat-eval '(5) '(zero? THENELSE (1) (0)))
 (0)
 > (copycat-eval '(0) '(zero? THENELSE (1) (0)))
 (1)
 ;; stack-based branching facility (works like a normal word): takes
 ;; boolean value, truebranch and falsebranch from the stack at
 ;; runtime
 > (copycat-eval '(5) '(zero? (1) (0) thenelse))
 (0)
 > (copycat-eval '(0) '(zero? (1) (0) thenelse))
 (1)
 ;; rot takes a number denoting the number of elements to rotate, and
 ;; rotates their position on the stack so that the last of those
 ;; becomes the first:
 > (copycat-eval '((no) (yes) #t 7) '(3 rot))
 (#t (no) (yes) 7)
 ;; write a word-based branching facility ourselves, using the
 ;; syntax-based one internally:
 > (copycat-eval '() '((3 rot THENELSE (drop eval) (swap drop eval))
		     QUOTE if set!))
 > (copycat-eval '(5) '(zero? (1) (0) if))
 (0)
 > (copycat-eval '(0) '(zero? (1) (0) if))
 (1)
 ;; write a word-based branching facility ourselves, using the
 ;; stack-based one internally:
 > (copycat-eval '() '((thenelse) QUOTE if* set!))
 > (copycat-eval '(5) '(zero? (1) (0) if*))
 (0)
 > (copycat-eval '(0) '(zero? (1) (0) if*))
 (1)
 ;; alias the branching facility by simply storing it to a different
 ;; word:
 > (copycat-eval '() '(QUOTE if* ref QUOTE anotherif set!))
 > (copycat-eval '(5) '(zero? (1) (0) anotherif))
 (0)
 > (copycat-eval '(0) '(zero? (1) (0) anotherif))
 (1)

 ;; factorial
 > (copycat-eval '(0) '(: fact (dup zero? THENELSE (drop 1) (dup 1 - fact *))))
 ;; or:
 > (copycat-eval '(0) '(: fact (dup zero? (drop 1) (dup 1 - fact *) thenelse)))
 > (copycat-eval '(0) '(fact))
 (1)
 > (copycat-eval '(1) '(fact))
 (1)
 > (copycat-eval '(2) '(fact))
 (2)
 > (copycat-eval '(3) '(fact))
 (6)
 > (copycat-eval '(20) '(fact))
 (2432902008176640000)
 )
