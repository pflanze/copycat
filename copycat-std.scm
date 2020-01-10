;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         copycat-interpreter
         test)

(export)
;; XX offer them in an exported fashion instead of mutating the global
;; symbol table?

;; -- stack ops
;; as shown on http://wiki.laptop.org/go/Forth_stack_operators

(cc-def dup (a -> a a)
        (cc-return a a))
(cc-def drop (a ->)
        (cc-return))
(cc-def swap (a b -> b a)
        (cc-return b a))
(cc-def rot (a b c -> b c a)
        (cc-return b c a))
(cc-def -rot (a b c -> c a b)
        (cc-return c a b))
(cc-def nip (a b -> b)
        (cc-return b))

(cc-def roll ([fixnum-natural0? n] ->)
        "take the last n elements on the stack, roll them around so
that the oldest one becomes the newest"
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


(def (copycat:pick $word $s n)
     (if-Just ((it (list-Maybe-ref $s n)))
              (cc-return it)
              (Error (copycat-missing-arguments
                      $word
                      'copycat:pick ;; XX?
                      (inc n)
                      (length $s)))))
(cc-def over (-> any?)
        "copy the second-last element from the stack"
        (copycat:pick $word $s 1))
(cc-def pick2 (-> any?)
        "copy the third-last element from the stack"
        (copycat:pick $word $s 2))
(cc-def pick3 (-> any?)
        "copy the fourth-last element from the stack"
        (copycat:pick $word $s 3))
(cc-def pick (n -> any?)
        "copy the element from the stack found after skipping n elements"
        (copycat:pick $word $s n))

;; my own ideas for stack ops:

(cc-def dropn ([fixnum-natural0? n] ->)
        "drop the n last elements from the stack"
        (if-Just ((it (Maybe-drop $s n)))
                 (Ok it)
                 (Error
                  (copycat-missing-arguments $word
                                             (list 'dropn n)
                                             n
                                             (length $s)))))

(cc-def clear ()
        "drop all elements from the stack"
        (Ok '()))
;; and with a shorter name:
(cc-def c ()
        "drop all elements from the stack"
        (Ok '()))


;; -- pure functions (except for error handling!)

(cc-defhost + ([number? a] [number? b] -> number?))
(cc-defhost - ([number? a] [number? b] -> number?))
(cc-defhost * ([number? a] [number? b] -> number?))
(cc-def / ([number? a] [number? b] -> number?)
        (if (and (exact? b)
                 (zero? b))
            (Error (copycat-division-by-zero $word a b))
            (cc-return (/ a b))))
(cc-def inv ([number? x] -> number?)
        "1/x"
        (cc-return (/ x)))
(cc-defhost inc ([fixnum? n] -> fixnum?)
            "the successor number to n")
(cc-defhost dec ([fixnum? n] -> fixnum?)
            "the predecessor number to n")
(cc-defhost square ([number? x] -> number?)
            "x * x")
(cc-defhost sqrt ([number? x] -> number?)
            "the square root of x")
(cc-defhost expt ([number? base] [number? exponent] -> number?)
            "the base `base` raised to the power of `exponent`")
(cc-defhost log ([number? x] -> number?)
            "the logarithm of x with base e")
(cc-def log2 ([number? x] -> number?)
        "the logarithm of x with base 2"
        (cc-return (/ (log x) (log 2))))
(cc-def log10 ([number? x] -> number?)
        "the logarithm of x with base 10"
        (cc-return (/ (log x) (log 10))))


(cc-defhost zero? ([number? a] -> boolean?)
            "whether a is zero")
(cc-defhost = ([number? a] [number? b] -> boolean?)
            "whether a is numerically equal to b")
(cc-defhost < ([number? a] [number? b] -> boolean?)
            "whether a is smaller than b")
(cc-defhost <= ([number? a] [number? b] -> boolean?)
            "whether a is smaller than or equal to b")
(cc-defhost > ([number? a] [number? b] -> boolean?)
            "whether a is larger than b")
(cc-defhost >= ([number? a] [number? b] -> boolean?)
            "whether a is larger than or equal to b")
(cc-def != ([number? a] [number? b] -> boolean?)
        "whether a and b are unequal"
        (cc-return (not (= a b))))
(cc-defhost eq? (a b -> boolean?)
            "whether a and b are the same objects")
(cc-def !eq? (a b -> boolean?)
        "whether a and b are *not* the same objects"
        (cc-return (not (eq? a b))))

(cc-def cons (l e -> pair?) "prepend e to the given list l"
        (cc-return (cons e l)))
(cc-defhost car ([pair? a] -> any?)
            "return the element slot from the given pair")
(cc-defhost cdr ([pair? a] -> any?)
            "return the rest slot of the given pair")
(cc-defhost first ([pair? l] -> any?)
            "return the first element of the given list")
(cc-defhost rest ([pair? l] -> any?)
            "drop the first element from the given list")
(cc-defhost pair? (a -> boolean?)
            "whether a is a pair (non-empty list)")
(cc-defhost null? (a -> boolean?)
            "whether a is an empty list")

(def (cc:Rlist $s $word numargs reverse?)
     (if-Just ((it (Maybe-split-at-reverse $s numargs)))
              (letv ((rargs stack*) it)
                    (Ok (cons (if reverse? rargs (reverse rargs))
                              stack*)))
              (Error (copycat-missing-arguments $word
                                                'list ;; ?
                                                n
                                                (length $s)))))
(cc-def list ([fixnum-natural0? n] -> (list-of-length n))
        "takes n elements from the stack and returns them as a list"
        (cc:Rlist $s $word n #t))
(cc-def rlist ([fixnum-natural0? n] -> (list-of-length n))
        "takes n elements from the stack and returns them as a reversed list"
        (cc:Rlist $s $word n #f))

(cc-defhost/try append (a b -> ilist?))
(cc-defhost string-append ([string? a] [string? b] -> string?))
(cc-defhost/try strings-append (l -> string?))
(cc-defhost/try strings-join ([(ilist-of string?) l] [string? inbetween]
                              -> string?))
(cc-defhost string-split ([string? str]
                          ;; XX todo: allow forth preds by
                          ;; wrapping
                          [char? char-or-pred]
                          -> (ilist-of string?)))

(cc-def and ([any? a] [ilist? b] -> any?)
        "this is not strictly a boolean operator, but a 'maybe' type
style one (monadic >>)"
        (if a
            (cc-eval $s b)
            (cc-return a)))

(cc-def or ([any? a] [ilist? b] -> any?)
        "this is not strictly a boolean operator, but a 'maybe' type
style one"
        (if a
            (cc-return a)
            (cc-eval $s b)))


;; (cc-defhost error/1 (a))
;; (cc-defhost error/2 (a))


;; -- procedures (for side-effects)

(cc-def eval (prog)
        "evaluate prog (a list of instructions)"
        (copycat:try (cc-eval $s prog)))

(cc-def nop (->)
        "no operation"
        (cc-return))

(cc-def set! ([(either ilist? ccproc?) prog] [symbol? name] ->)
        "set the word with the given name to prog, which must be
either a list of instructions, or a ccproc data structure as retrieved
from `ref`"
        (let (prog (xcond ((ccproc? prog)
                           prog)
                          ((ilist? prog)
                           (ccguestproc #f ;; docstring
                                        (cc-type-unknown #f)
                                        prog))))
          (cc-word-set! name prog))
        (cc-return))

(cc-defhost string->symbol ([string? s] -> symbol?)
            "turn s into a symbol with the same name")

(cc-defhost/try .symbol (s -> symbol?)
                "generic to try to turn s into a symbol")

(cc-defhost symbol->string ([symbol? s] -> string?)
            "return the underlying name string of the given symbol")

(cc-def ref ([symbol? name] -> ccproc?)
        "return the ccproc data structure associated with name (giving
an error if not bound)"
        (if-Just ((v (table.Maybe-ref cc-words name)))
                 (cc-return v)
                 (Error (copycat-unbound-symbol $word name))))

(cc-defhost/try .docstring (s))
(cc-defhost/try .string (s))
(cc-defhost/try .type (s))
(cc-defhost/try .maybe-original (s))
(cc-defhost source-code (s)
            "strips location information from source (i.e. return the
code embedded in a source object); if s is not a source object, return
s (i.e. never fails)")


;; XX lib
(def (pretty-string v)
     (fst (with-output-to-string (& (pretty-print (cj-desourcify v))))))

(cc-defhost pretty-string (s -> string?)
            "pretty-print s to a string")

(cc-eval '() (quote-source
              (
               (: help-string [symbol? word] -> string?
                  "give help string on the given word"
                  (
                   dup .string ": " string-append ;; intro
                   swap ref
                   dup
                   .type .maybe-original pretty-string ;; type
                   swap
                   .docstring source-code ("(no help text)") or ;; help
                   2 list "\n" strings-join
                   2 list strings-append))

               (: help [symbol? word] ->
                  "print help on the given word"
                  (help-string println)))))

(cc-def thenelse ([boolean? val] [ilist? truebranch] [ilist? falsebranch])
        (cc-eval $s (if val truebranch falsebranch)))

(cc-def print (v ->)
        (mdo (copycat:try-Ok (print v))
             (cc-return)))

(cc-def write (v ->)
        (mdo (copycat:try-Ok (write v))
             (cc-return)))

(cc-def show (v ->)
        (mdo (copycat:try-Ok (pretty-print (try-show v)))
             (cc-return)))

(cc-def newline (->)
        (mdo (copycat:try-Ok (newline))
             (cc-return)))

(cc-def println (v ->)
        (mdo (copycat:try-Ok (println v))
             (cc-return)))

(cc-def read-source ([string? path] -> ilist?)
        "read the contents of the file at path as a list of
s-expressions, enriched with location information"
        (>>= (copycat:try-Ok
              (call-with-input-file path read-all-source))
             (C cc-return _)))

(cc-eval '() (quote-source
              (
               (: load [string? path]
                  "read and evaluate the given file"
                  (read-source eval)))))


;; -- debugging

(cc-def D ()
        "(for debugging) print stack, enter a repl; enter ,(c (Ok $s))
to continue!"
        (mdo (copycat:try-Ok (pretty-print $s))
             (##repl)))

(cc-def P (->)
        "(for debugging) print the location of P and then the current
stack"
        (mdo (copycat:try-Ok
              (show-source-location $word)
              (pretty-print $s))
             (cc-return)))

(cc-def P* ([string? msg] ->)
        "(for debugging) print msg and then the current stack"
        (mdo (copycat:try-Ok (display msg)
                             (display ": ")
                             (pretty-print $s))
             (cc-return)))


(TEST
 > (def (t stack prog)
        (.show (cc-eval stack prog)))
 > (t '() '(4 5 5 *))
 (Ok (list 25 4))
 > (t '() (quote-source (4 5 5 * -)))
 (Ok (list -21))
 > (t '() (quote-source (4 5 dup * -)))
 (Ok (list -21))
 > (t '() '(4 5 swap dup * -))
 (Ok (list -11))
 > (t '(1 2) '(over))
 (Ok (list 2 1 2))
 > (t '(1) '(over))
 (Error (copycat-missing-arguments 'over 'copycat:pick 2 1))
 > (t '(1 2 3) '(pick2))
 (Ok (list 3 1 2 3))
 > (t '(1 2)
      (source* (list (source* 'pick2 '(console) 10 16))
               '(console) 10 15))
 (Error (copycat-missing-arguments
         (source* 'pick2 (list 'console) 10 16)
         'copycat:pick
         3
         2))
 > (t '(a b c) '(2 pick))
 (Ok (list 'c 'a 'b 'c))
 > (t '(a b c) '(3 pick))
 (Error (copycat-missing-arguments 'pick 'copycat:pick 4 3))
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
 (Error (copycat-missing-arguments 'dropn (list 'dropn 4) 4 3))
 > (t '(6 7 8) '(-4 dropn))
 (Error (copycat-type-error 'dropn "(fixnum-natural0? n)" -4))
 > (t '() '("f" inv))
 (Error (copycat-type-error 'inv "(number? x)" "f"))
 > (t '() '("f" 5 +))
 (Error (copycat-type-error '+ "(number? a)" "f"))
 > (t '() '(5 "f" +))
 (Error (copycat-type-error '+ "(number? b)" "f"))

 > (t '() '(() 1 cons))
 (Ok (list (list 1)))

 > (t '() '("foo bar" #\space string-split strings-append))
 (Ok (list "foobar"))
 
 > (t '() '('f 42 +))
 (Error (copycat-type-error '+ "(number? a)" 'f))
 > (t '() '("foo" string->symbol 'bar))
 (Ok (list 'bar 'foo))
 > (t '() '(1 2 "+" .symbol 1 list eval))
 (Ok (list 3))

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
 ;; The variant in examples/fact.scm
 > (t '() '("examples/fact.scm" load))
 (Ok (list))
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
 ;; Or, with THENELSE syntax:
 > (t '(0) '(: fact2 (dup zero? THENELSE (drop 1) (dup 1 - fact *))))
 > (t '(20) '(fact2))
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
 (Error (copycat-missing-arguments ': (list) 2 1))
 > (t '() '(: foo 4))
 ;; yes, could be more expressive. "not a program". ?
 (Error (copycat-type-error ': "list?" 4))
 > (t '() '(: foo () 4))
 (Ok (list 4)))
