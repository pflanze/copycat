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


(require easy
         (oo-util-lazy ilist-of)
         table
         Maybe
         Result
         srfi-1-Maybe
         error ;; for built-in exception .show
         (cj-typed typed-lambda-expand)
         )

(export cc-eval
        cc-repl)



;; table to store the values of words
(def cc-words
     (make-table))
;; XX: Forth has a *tree* of binding maps? Context dependent? What kind
;; of context? Here it's just one map.

;; Note: currently the cc-words table is used for every word
;; reference at runtime. This could be made more efficient by moving
;; the lookups to a parsing step (symbol creation, store as part of
;; symbol data structure). ("Linker step")

(def (cc-word-set! [symbol? name] val) -> void?
     (table-set! cc-words name val))


;; We need a way to fall back onto the host system to implement
;; primitives, thus provide part of a foreign function interface:

(defclass (ccforeigncall [natural0? numargs]
                         [procedure? op]))


;; We represent guest language errors as (Error-of
;; copycat-runtime-error?):

;; XX Really bundle the stack with the error, or keep those separate?
(defclass (copycat-runtime-error [copycat-stack? stack])
  (defclass (copycat-runtime-error/symbol
             ;; Context, i.e. the word reporting the error, or the
             ;; word that wasn't found.
             [symbol? symbol])
    (defclass (copycat-unbound-symbol))
    (defclass (copycat-missing-arguments proc))
    (defclass (copycat-division-by-zero a b))
    (defclass (copycat-type-error predicate value))
    (defclass (copycat-host-error exception))))

(def copycat-stack? (ilist-of any?))
(def copycat-runtime-result?
     (Result-of copycat-stack?
                copycat-runtime-error?))


;; like Result:try but converts non-|copycat-runtime-error|s
(defmacro (copycat:try/name name . body)
  `(with-exception-catcher (C copycat:Error $s ',name _)
                           (lambda () (Ok (begin ,@body)))))

(def (copycat:Error $s name e)
     (Error
      (cond ((copycat-runtime-error? e) e)
            ;;((type-exception? e) ...)
            ;; ^ XX but have to change copycat-type-error to take multiple values.
            (else
             (copycat-host-error $s name e)))))


(def (copycat:_type-check-error $s $word
                                use-source-error?
                                expr-str
                                pred-str
                                pred
                                val)
     (Error (copycat-type-error $s
                                $word
                                ($ "($pred-str $expr-str)")
                                val)))

;; Macro so that it can unhygienically catch $s and $word. Evil?
(defmacro (copycat:type-check-error use-source-error?
                                    expr-str
                                    pred-str
                                    pred
                                    val)
  `(copycat:_type-check-error $s $word
                              ,use-source-error?
                              ,expr-str
                              ,pred-str
                              ,pred
                              ,val))

(defmacro (copycat-lambda args . body)
  (typed-lambda-expand stx args body '##begin
                       `copycat:type-check-error))

;; setting a word to a Scheme program (not translating Scheme
;; exceptions)
(defmacro (cc-def name args . body)
  (assert* symbol? name
           (lambda_
            `(cc-word-set! ',name
                           (ccforeigncall
                            ,(length (source-code args))
                            (let (($word ',name))
                              (copycat-lambda
                               ,(cons '$s (source-code args))
                               ;; ^ HEH that |source-code| is
                               ;; required. otherwise gambit has a
                               ;; problem, 'Identifier expected'
                               (in-monad Result
                                         ,@body))))))))

(defmacro (cc-return . es)
  `(Ok (cons* ,@(reverse es) $s)))

(defmacro (cc-defhost name args)
  `(cc-def ,name ,args
           (cc-return ,(cons name
                             (map perhaps-typed.var (source-code args))))))

(defmacro (cc-defhost/try name args)
  `(cc-def ,name ,args
           (>>= (copycat:try/name ,name
                                  ,(cons name (source-code args)))
                (C cc-return _))))

;; -- functions

(cc-defhost + ([number? a] [number? b]))
(cc-defhost - ([number? a] [number? b]))
(cc-defhost * ([number? a] [number? b]))
(cc-def / ([number? a] [number? b])
        (if (and (exact? b)
                 (zero? b))
            (Error (copycat-division-by-zero $s '/ a b))
            (cc-return (/ a b))))
(cc-def inv ([number? x])
        (cc-return (/ x)))
(cc-defhost inc ([fixnum? n]))
(cc-defhost dec ([fixnum? n]))
(cc-defhost square ([number? x]))
(cc-defhost sqrt ([number? x]))

(cc-defhost zero? ([number? v]))
(cc-defhost = ([number? a] [number? b]))
(cc-defhost < ([number? a] [number? b]))
(cc-defhost <= ([number? a] [number? b]))
(cc-defhost > ([number? a] [number? b]))
(cc-defhost >= ([number? a] [number? b]))
(cc-def != (a b)
        (>>= (copycat:try/name !=
                               (not (= a b)))
             (C cc-return _)))
(cc-defhost eq? (a b))
(cc-def !eq? (a b)
        (cc-return (not (eq? a b))))

(cc-def cons (a b)
        (cc-return (cons b a)))
(cc-defhost car ([pair? a]))
(cc-defhost cdr ([pair? a]))
(cc-defhost first ([pair? a]))
(cc-defhost rest ([pair? a]))
(cc-defhost pair? (a))
(cc-defhost null? (a))

(cc-defhost/try append (a b))
(cc-defhost string-append ([string? a] [string? b]))
(cc-defhost/try strings-append (l))
(cc-defhost string-split ([string? str]
                          ;; XX todo: allow forth preds by
                          ;; wrapping
                          [char? char-or-pred]))

(cc-defhost error/1 (a))
(cc-defhost error/2 (a))

;; -- stack ops
;; as shown on http://wiki.laptop.org/go/Forth_stack_operators

(cc-def dup (a)
        (cc-return a a))
(cc-def drop (a)
        (cc-return))
(cc-def swap (a b)
        (cc-return b a))
(cc-def rot (a b c)
        (cc-return b c a))
(cc-def -rot (a b c)
        (cc-return c a b))
(cc-def nip (a b)
        (cc-return b))

(cc-def roll (n)
        ;; (letv ((args stack*) (split-at $s n))
        ;;       (append (cons (last args) (butlast args)) stack*))
        ;;or, saving on intermediates:
        (let lp ((n n)
                 (tmp '())
                 (stack $s))
          (if (> n 1)
              (lp (dec n)
                  (cons (car stack) tmp)
                  (cdr stack))
              (Ok (cons (car stack)
                        (rappend tmp (cdr stack)))))))


(def (copycat:pick $s n name)
     (if-Just ((it (list-Maybe-ref $s n)))
              (cc-return it)
              (Error (copycat-missing-arguments
                      $s
                      name
                      ;; proc expected here, evil:
                      n))))
(cc-def over ()
        (copycat:pick $s 1 'over))
(cc-def pick2 ()
        (copycat:pick $s 2 'pick2))
(cc-def pick3 ()
        (copycat:pick $s 3 'pick3))
(cc-def pick (n)
        (copycat:pick $s n 'pick))

;; my own ideas for stack ops:

(cc-def dropn (n)
        (if (fixnum-natural0? n)
            (if-Just ((it (Maybe-drop $s n)))
                     (Ok it)
                     (Error
                      (copycat-missing-arguments $s 'dropn n)))
            (Error (copycat-type-error $s 'dropn 'fixnum-natural0? n))))

(cc-def clear ()
        (Ok '()))

;; -- procedures (for side-effects)

(cc-def eval (v)
        (cc-eval $s v))

(cc-def nop ()
        (cc-return))

(cc-def set! (prog name)
        (cc-word-set! name prog)
        (cc-return))

(cc-def ref (name)
        (if-Just ((v (table.Maybe-ref cc-words name)))
                 (cc-return v)
                 (Error (copycat-unbound-symbol $s name))))

(cc-def thenelse (val truebranch falsebranch)
        (cc-eval $s (if val truebranch falsebranch)))

(cc-def print (v)
        (mdo (copycat:try/name print
                               (print v))
             (cc-return)))

(cc-def write (v)
        (mdo (copycat:try/name write
                               (write v))
             (cc-return)))

(cc-def newline ()
        (mdo (copycat:try/name newline
                               (newline))
             (cc-return)))

(cc-def println (v)
        (mdo (copycat:try/name println
                               (println v))
             (cc-return)))


;; -- debugging

;; print stack, enter a repl; enter ,(c $s) to continue!
(cc-def D ()
        (mdo (copycat:try/name D
                               (pretty-print $s))
             (##repl)))

;; print stack
(cc-def P ()
        (mdo (copycat:try/name P
                               (pretty-print $s))
             (cc-return)))

(cc-def P* (a)
        (mdo (copycat:try/name P*
                               (display a)
                               (display ": ")
                               (pretty-print $s))
             (cc-return)))

;; ----------------------------

(def (cc-apply [copycat-stack? stack] [symbol? word])
     -> copycat-runtime-result?
     (if-Just ((w (table.Maybe-ref cc-words word)))
              (let (err
                    (lambda ()
                      (Error (copycat-missing-arguments stack
                                                        word
                                                        w))))
                (if (ccforeigncall? w)
                    (let-ccforeigncall
                     ((numargs op) w)
                     (case numargs
                       ((0) (op stack))
                       ((1) (if-let-pair
                             ((a r) stack)
                             (op r a)
                             (err)))
                       ((2) (if (length->= stack 2)
                                (op (cddr stack) (cadr stack) (car stack))
                                (err)))
                       (else
                        (if-Just ((it (Maybe-split-at-reverse stack numargs)))
                                 (letv ((rargs stack) it)
                                       (apply op stack rargs))
                                 (err)))))
                    (cc-eval stack w)))
              (Error (copycat-unbound-symbol stack word))))

(def (cc-eval stack prog) ;; -> copycat-runtime-result? ;; XX don't break TCO!
     (in-monad
      Result
      (if (null? prog)
          (Ok stack)
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
                         (let (err (lambda (notpair)
                                     (Error
                                      (copycat-missing-arguments
                                       stack ':
                                       ;; XX again, why "proc" argument?
                                       notpair))))
                           (if-let-pair
                            ((name r) prog*)
                            (if-let-pair
                             ((subprog cont) r)
                             (if (list? subprog)
                                 (begin (cc-word-set! name subprog)
                                        (cc-eval stack cont))
                                 (Error
                                  (copycat-type-error stack
                                                      ':
                                                      'list?
                                                      subprog)))
                             (err r))
                            (err prog*))))
                        ((THENELSE)
                         ;; takes 2 arguments from program (truebranch,
                         ;; falsebranch), and 1 from stack (test value)
                         (let ((cont (cddr prog*)))
                           (>>= (cc-eval (cdr stack)
                                         (if (car stack)
                                             (car prog*)
                                             (cadr prog*)))
                                (C cc-eval _ cont))))
                        ((QUOTE)
                         ;; takes 1 argument from program, puts it on
                         ;; the stack
                         (let ((cont (cdr prog*)))
                           (cc-eval (cons (car prog*) stack) cont)))
                        (else
                         (let ((app (thunk (cc-apply stack item))))
                           (if (null? prog*)
                               (app)
                               (>>= (app)
                                    (C cc-eval _ prog*)))))))
                     (else
                      (cc-eval (cons item stack) prog*)))))))

(TEST
 > (def (t stack prog)
        (.show (cc-eval stack prog)))
 > (t '() '(4 5 5 *))
 (Ok (list 25 4))
 > (t '() '(4 5 5 * -))
 (Ok (list -21))
 > (t '() '(4 5 dup * -))
 (Ok (list -21))
 > (t '() '(4 5 swap dup * -))
 (Ok (list -11))
 > (t '(1 2) '(over))
 (Ok (list 2 1 2))
 > (t '(1) '(over))
 (Error (copycat-missing-arguments (list 1) 'over 1))
 > (t '(1 2 3) '(pick2))
 (Ok (list 3 1 2 3))
 > (t '(1 2) '(pick2))
 (Error (copycat-missing-arguments (list 1 2) 'pick2 2))
 > (t '(a b c) '(2 pick))
 (Ok (list 'c 'a 'b 'c))
 > (t '(a b c) '(3 pick))
 (Error (copycat-missing-arguments (list 'a 'b 'c) 'pick 3))
 > (t '(c b a) '(rot))
 (Ok (list 'a 'c 'b))
 > (t '(c b a) '(3 roll))
 (Ok (list 'a 'c 'b))
 > (t '(c b a) '(-rot))
 (Ok (list 'b 'a 'c))
 > (t '(c b a) '(rot rot))
 (Ok (list 'b 'a 'c))
 > (t '(c b a x) '(rot -rot))
 (Ok (list 'c 'b 'a 'x))
 > (t '(c b a x) '(nip))
 (Ok (list 'c 'a 'x))
 > (t '(1 2 3) '(clear))
 (Ok (list))
 > (t '(6 7 8) '(3 dropn))
 (Ok (list))
 > (t '(6 7 8) '(4 dropn))
 (Error (copycat-missing-arguments (list 6 7 8) 'dropn 4))
 > (t '(6 7 8) '(-4 dropn))
 (Error (copycat-type-error (list 6 7 8) 'dropn 'fixnum-natural0? -4))
 > (t '() '("f" inv))
 (Error (copycat-type-error (list) 'inv "(number? x)" "f"))
 > (t '() '("f" 5 +))
 (Error (copycat-type-error (list) '+ "(number? a)" "f"))
 > (t '() '(5 "f" +))
 (Error (copycat-type-error (list) '+ "(number? b)" "f"))

 > (t '() '(() 1 cons))
 (Ok (list (list 1)))

 > (t '() '("foo bar" #\space string-split strings-append))
 (Ok (list "foobar"))
 
 > (t '() '('f 42 +))
 (Error (copycat-type-error
         (list) '+
         "(number? a)" (list 'quote 'f)))
 
 ;; sublists are representing sub-programs, which are only evaluated
 ;; on demand:
 > (t '() '(4 (5) eval))
 (Ok (list 5 4))
 > (t '() '(4 (5 1 +) eval))
 (Ok (list 6 4))

 ;; words can be quoted by way of QUOTE:
 > (t '() '(QUOTE 1))
 (Ok (list 1))
 > (t '() '(QUOTE foo))
 (Ok (list 'foo))

 ;; "syntax-based" word definition form: |:| takes a name, and a
 ;; program to its right, syntactically
 > (t '() '(: square (dup *) 4 square))
 (Ok (list 16))
 ;; stack-based word definition form (works like a normal word):
 ;; |set!| takes a program and a name from the stack at runtime. (I
 ;; don't know why the original Cc chooses to use such
 ;; "syntax-based" features when it could do with program and symbol
 ;; quoting and then just words like this, other than visual
 ;; preference.)
 > (t '() '((dup *) QUOTE sqr set! 4 sqr))
 (Ok (list 16))

 ;; "syntax-based" branching facility: takes a truebranch and a
 ;; falsebranch to its right, syntactically, as well as a boolean
 ;; value from the stack at runtime.
 > (t '(5) '(zero?))
 (Ok (list #f))
 > (t '(5) '(zero? THENELSE (1) (0)))
 (Ok (list 0))
 > (t '(0) '(zero? THENELSE (1) (0)))
 (Ok (list 1))
 ;; stack-based branching facility (works like a normal word): takes
 ;; boolean value, truebranch and falsebranch from the stack at
 ;; runtime
 > (t '(5) '(zero? (1) (0) thenelse))
 (Ok (list 0))
 > (t '(0) '(zero? (1) (0) thenelse))
 (Ok (list 1))
 ;; roll takes a number denoting the number of elements to rotate, and
 ;; rotates their position on the stack so that the last of those
 ;; becomes the first:
 > (t '((no) (yes) #t 7) '(3 roll))
 (Ok (list #t (list 'no) (list 'yes) 7))
 ;; write a word-based branching facility ourselves, using the
 ;; syntax-based one internally:
 > (t '() '((3 roll THENELSE (drop eval) (swap drop eval))
            QUOTE if set!))
 ;;(Ok (list))
 > (t '(5) '(zero? (1) (0) if))
 (Ok (list 0))
 > (t '(0) '(zero? (1) (0) if))
 (Ok (list 1))
 ;; write a word-based branching facility ourselves, using the
 ;; stack-based one internally:
 > (t '() '((thenelse) QUOTE if* set!))
 > (t '(5) '(zero? (1) (0) if*))
 (Ok (list 0))
 > (t '(0) '(zero? (1) (0) if*))
 (Ok (list 1))
 ;; alias the branching facility by simply storing it to a different
 ;; word:
 > (t '() '(QUOTE if* ref QUOTE anotherif set!))
 > (t '(5) '(zero? (1) (0) anotherif))
 (Ok (list 0))
 > (t '(0) '(zero? (1) (0) anotherif))
 (Ok (list 1))

 ;; factorial
 > (t '(0) '(: fact (dup zero? THENELSE (drop 1) (dup 1 - fact *))))
 ;; or:
 > (t '(0) '(: fact (dup zero? (drop 1) (dup 1 - fact *) thenelse)))
 > (t '(0) '(fact))
 (Ok (list 1))
 > (t '(1) '(fact))
 (Ok (list 1))
 > (t '(2) '(fact))
 (Ok (list 2))
 > (t '(3) '(fact))
 (Ok (list 6))
 > (t '(20) '(fact))
 (Ok (list 2432902008176640000))

 ;; <lis> <code> map
 ;; iterative version:
 > (t '()
      '(:
        rmap-iter ;; <code> <lis> <result>
        (over
         pair?
         ( ;; change result
          over
          car pick3 eval cons
          ;; and lis
          swap cdr swap
          rmap-iter)
         (over
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
 > (t '((1 2)) '((inc) rmap))
 (Ok (list (list 3 2)))
 > (t '()
      '(:
        reverse-iter ;; <lis> <result>
        (over
         pair?
         (over car cons
               swap cdr swap
               reverse-iter)
         (over
          null?
          (swap drop)
          ("improper list" error/1)
          if)
         if)
        :
        reverse
        (() reverse-iter)
        :
        imap
        (rmap reverse)))
 > (t '(()) '((inc) imap))
 (Ok (list (list)))
 > (t '((5 6 7)) '((inc) imap))
 (Ok (list (list 6 7 8)))
 ;; recursive definition:
 > (t '()
      '(:
        map-recur ;; <fn> <lis> -> <fn> <res>
        (dup
         pair?
         ( ;; P
          dup
          car
          pick2
          ;; "vor eval" P*
          eval
          ;; "after evap" P*
          swap rot swap cdr
          ;; "after rot" P*
          ;; D
          map-recur
          ;; "after recur" P*
          rot
          cons)
         ()
         if)
        :
        map
        (swap map-recur swap drop)))
 > (t '((5 6 7)) '((inc) map))
 (Ok (list (list 6 7 8)))

 ;; test tail call optimization: this must run indefinitely and not
 ;; run out of memory:
 ;; > (cc-eval '() '(: lp (lp) lp))
 ;; (comment out (generate-proper-tail-calls #f) in .gambcini for this to work)


 ;; More error testing:
 > (t '() '(: foo))
 (Error (copycat-missing-arguments (list) ': (list)))
 > (t '() '(: foo 4))
 ;; yes, could be more expressive. "not a program". ?
 (Error (copycat-type-error (list) ': 'list? 4))
 > (t '() '(: foo () 4))
 (Ok (list 4)))



(def (cc-repl #!optional (stack '())) -> !
     (in-monad
      Result
      (pp (reverse stack))
      (display "$ ")
      (if-Ok (>>= (let ($s stack)
                    (copycat:try/name
                     cc-repl
                     (with-input-from-string (read-line) read-all)))
                  (C cc-eval stack _))
             (cc-repl it)
             (begin
               (warn "Error:" (try-show it))
               (cc-repl stack)))))

