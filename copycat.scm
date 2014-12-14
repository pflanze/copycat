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
(def cc-words
     (make-table))
;; XX: Forth has a *tree* of binding maps? Context dependent? What kind
;; of context? Here it's just one map.

;; Note: currently the cc-words table is used for every word
;; reference at runtime. This could be made more efficient by moving
;; the lookups to a parsing step (symbol creation, store as part of
;; symbol data structure). ("Linker step")

(def (cc-word-set! #(symbol? name) val)
     (table-set! cc-words name val))


;; We need a way to fall back onto the host system to implement
;; primitives, thus provide part of a foreign function interface:

(defstruct ccforeigncall
  #(natural0? numargs)
  #(procedure? op))

;; setting a word to a Scheme program
(defmacro (cc-def name args . body)
  (assert* symbol? name
	   (lambda_
	    `(cc-word-set! ',name
			      (ccforeigncall
			       ,(length (source-code args))
			       (lambda ,(cons '$s (source-code args))
				 ;; ^ HEH that |source-code| is
				 ;; required. otherwise gambit has a
				 ;; problem, 'Identifier expected'
				 ,@body))))))

(defmacro (cc-return . es)
  `(cons* ,@(reverse es) $s))

(defmacro (cc-defhost name args)
  `(cc-def ,name ,args
	      (cc-return ,(cons name (source-code args)))))

;; -- functions

(cc-defhost + (a b))
(cc-defhost - (a b))
(cc-defhost * (a b))
(cc-defhost / (a b))

(cc-def dup (a)
	   (cc-return a a))

(cc-def drop (a)
	   $s)

(cc-def swap (a b)
	   (cc-return b a))

(cc-defhost zero? (v))
(cc-defhost = (a b))
(cc-defhost < (a b))
(cc-defhost <= (a b))
(cc-defhost > (a b))
(cc-defhost >= (a b))
(cc-def != (a b)
	   (cc-return (not (= a b))))
(cc-defhost eq? (a b))
(cc-def !eq? (a b)
	   (cc-return (not (eq? a b))))

(cc-def cons (a b)
	(cc-return (cons b a)))
(cc-defhost car (a))
(cc-defhost cdr (a))
(cc-defhost pair? (a))
(cc-defhost null? (a))

(cc-defhost error/1 (a))
(cc-defhost error/2 (a))

(cc-def rot (n)
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

;; like dup but copy the value behind the last
(cc-def dup1 ()
	(cons (cadr $s) $s))
;; etc.
(cc-def dup2 ()
	(cons (caddr $s) $s))
(cc-def dup3 ()
	(cons (cadddr $s) $s))

;; like rot but copy instead of moving
(cc-def pick (n)
	(cons (list-ref $s n) $s))


;; -- procedures (for side-effects)

(cc-def eval (v)
	   (cc-eval $s v))

(cc-def nop ()
	   $s)

(cc-def set! (prog name)
	   (cc-word-set! name prog)
	   $s)

(cc-def ref (name)
	   (cc-return (table-ref cc-words name)))

(cc-def thenelse (val truebranch falsebranch)
	   (cc-eval $s (if val truebranch falsebranch)))

(cc-def print (v)
	   (print v)
	   $s)

(cc-def write (v)
	   (write v)
	   $s)

(cc-def newline ()
	   (newline)
	   $s)

(cc-def println (v)
	   (println v)
	   $s)


;; -- debugging

;; print stack, enter a repl; enter ,(c $s) to continue!
(cc-def D ()
	(pretty-print $s)
	(##repl))

;; ----------------------------

(def (cc-apply stack #(symbol? word))
     (let ((w (table-ref cc-words word)))
       (if (ccforeigncall? w)
	   (let-ccforeigncall
	    ((numargs op) w)
	    (case numargs
	      ((0) (op stack))
	      ((1) (op (cdr stack) (car stack)))
	      ((2) (op (cddr stack) (cadr stack) (car stack)))
	      (else
	       ;; split-at-reverse?
	       (letv ((args stack) (split-at stack numargs))
		     (apply op (cons stack (reverse args)))))))
	   (cc-eval stack w))))

(def (cc-eval stack prog)
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
			  (cc-word-set! name subprog)
			  (cc-eval stack cont)))
		       ((THENELSE)
			;; takes 2 arguments from program (truebranch,
			;; falsebranch), and 1 from stack (test value)
			(let ((cont (cddr prog*)))
			  (cc-eval (cc-eval (cdr stack)
						  (if (car stack)
						      (car prog*)
						      (cadr prog*)))
				      cont)))
		       ((QUOTE)
			;; takes 1 argument from program, puts it on
			;; the stack
			(let ((cont (cdr prog*)))
			  (cc-eval (cons (car prog*) stack) cont)))
		       (else
			(let ((app (thunk (cc-apply stack item))))
			  (if (null? prog*)
			      (app)
			      (cc-eval (app) prog*))))))
		    (else
		     (cc-eval (cons item stack) prog*))))))

(TEST
 > (cc-eval '() '(4 5 5 *))
 (25 4)
 > (cc-eval '() '(4 5 5 * -))
 (-21)
 > (cc-eval '() '(4 5 dup * -))
 (-21)
 > (cc-eval '() '(4 5 swap dup * -))
 (-11)
 > (cc-eval '(1 2) '(dup1))
 (2 1 2)
 > (cc-eval '(1 2 3) '(dup2))
 (3 1 2 3)
 > (cc-eval '(1 2 3) '(2 pick))
 (3 1 2 3)

 > (cc-eval '() '(() 1 cons))
 ((1))
 
 ;; sublists are representing sub-programs, which are only evaluated
 ;; on demand:
 > (cc-eval '() '(4 (5) eval))
 (5 4)
 > (cc-eval '() '(4 (5 1 +) eval))
 (6 4)

 ;; words can be quoted by way of QUOTE:
 > (cc-eval '() '(QUOTE 1))
 (1)
 > (cc-eval '() '(QUOTE foo))
 (foo)

 ;; "syntax-based" word definition form: |:| takes a name, and a
 ;; program to its right, syntactically
 > (cc-eval '() '(: square (dup *) 4 square))
 (16)
 ;; stack-based word definition form (works like a normal word):
 ;; |set!| takes a program and a name from the stack at runtime. (I
 ;; don't know why the original Cc chooses to use such
 ;; "syntax-based" features when it could do with program and symbol
 ;; quoting and then just words like this, other than visual
 ;; preference.)
 > (cc-eval '() '((dup *) QUOTE sqr set! 4 sqr))
 (16)

 ;; "syntax-based" branching facility: takes a truebranch and a
 ;; falsebranch to its right, syntactically, as well as a boolean
 ;; value from the stack at runtime.
 > (cc-eval '(5) '(zero?))
 (#f)
 > (cc-eval '(5) '(zero? THENELSE (1) (0)))
 (0)
 > (cc-eval '(0) '(zero? THENELSE (1) (0)))
 (1)
 ;; stack-based branching facility (works like a normal word): takes
 ;; boolean value, truebranch and falsebranch from the stack at
 ;; runtime
 > (cc-eval '(5) '(zero? (1) (0) thenelse))
 (0)
 > (cc-eval '(0) '(zero? (1) (0) thenelse))
 (1)
 ;; rot takes a number denoting the number of elements to rotate, and
 ;; rotates their position on the stack so that the last of those
 ;; becomes the first:
 > (cc-eval '((no) (yes) #t 7) '(3 rot))
 (#t (no) (yes) 7)
 ;; write a word-based branching facility ourselves, using the
 ;; syntax-based one internally:
 > (cc-eval '() '((3 rot THENELSE (drop eval) (swap drop eval))
		  QUOTE if set!))
 > (cc-eval '(5) '(zero? (1) (0) if))
 (0)
 > (cc-eval '(0) '(zero? (1) (0) if))
 (1)
 ;; write a word-based branching facility ourselves, using the
 ;; stack-based one internally:
 > (cc-eval '() '((thenelse) QUOTE if* set!))
 > (cc-eval '(5) '(zero? (1) (0) if*))
 (0)
 > (cc-eval '(0) '(zero? (1) (0) if*))
 (1)
 ;; alias the branching facility by simply storing it to a different
 ;; word:
 > (cc-eval '() '(QUOTE if* ref QUOTE anotherif set!))
 > (cc-eval '(5) '(zero? (1) (0) anotherif))
 (0)
 > (cc-eval '(0) '(zero? (1) (0) anotherif))
 (1)

 ;; factorial
 > (cc-eval '(0) '(: fact (dup zero? THENELSE (drop 1) (dup 1 - fact *))))
 ;; or:
 > (cc-eval '(0) '(: fact (dup zero? (drop 1) (dup 1 - fact *) thenelse)))
 > (cc-eval '(0) '(fact))
 (1)
 > (cc-eval '(1) '(fact))
 (1)
 > (cc-eval '(2) '(fact))
 (2)
 > (cc-eval '(3) '(fact))
 (6)
 > (cc-eval '(20) '(fact))
 (2432902008176640000)

 ;; <lis> <code> map
 > (cc-eval '() '(: inc (1 +)))
 > (cc-eval '()
	    '(:
	      rmap-iter ;; <code> <lis> <result>
	      (dup1
	       pair?
	       ( ;; change result
		dup1
		car dup3 eval cons
		;; and lis
		swap cdr swap
		rmap-iter)
	       (dup1
		null?
	        (swap drop swap drop) ;; optimize?
		("improper list" error/1)
		if)
	       if)
	      :
	      rmap
	      (swap
	       ()
	       rmap-iter)))
 > (cc-eval '((1 2)) '((inc) rmap))
 ((3 2))
 > (cc-eval '()
	    '(:
	      reverse-iter ;; <lis> <result>
	      (dup1
	       pair?
	       (dup1 car cons
		     swap cdr swap
		     reverse-iter)
	       (dup1
		null?
		(swap drop)
		("improper list" error/1)
		if)
	       if)
	      :
	      reverse
	      (() reverse-iter)
	      :
	      map
	      (rmap reverse)))
 > (cc-eval '(()) '((inc) map))
 (())
 > (cc-eval '((5 6 7)) '((inc) map))
 ((6 7 8))

 ;; test tail call optimization: this must run indefinitely and not
 ;; run out of memory:
 ;; > (cc-eval '() '(: lp (lp) lp))
 ;; (comment out (generate-proper-tail-calls #f) in .gambcini for this to work)
 )
