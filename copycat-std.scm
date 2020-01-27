;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         copycat-interpreter
         copycat-std-part1
         copycat-std-part2
         copycat-std-part3
         test)

(export)
;; XX offer them in an exported fashion instead of mutating the global
;; symbol table?


"The standard library for Copycat (see copycat-std-part* for the
implementation)"


(include "lib/cj-standarddeclares.scm")

(TEST
 > (include "copycat-std-test--include.scm"))


;; -- Remaining tests for functionality in copycat-std-part* --------------

(TEST
 > (t '(4 5 5 *))
 (Ok (list 25 4))
 > (t (quote-source (4 5 5 * -)))
 (Ok (list -21))
 > (t (quote-source (4 5 dup * -)))
 (Ok (list -21))
 > (t '(4 5 swap dup * -))
 (Ok (list -11))
 > (t '(2 1 over))
 (Ok (list 2 1 2))
 > (t '(1 over))
 (Error (copycat-missing-arguments 'over 'copycat:pick 2 1))
 > (t '(3 2 1 pick2))
 (Ok (list 3 1 2 3))
 > (t (source* (list 2 1 (source* 'pick2 '(console) 10 16))
               '(console) 10 15))
 (Error (copycat-missing-arguments
         (source* 'pick2 (list 'console) 10 16)
         'copycat:pick
         3
         2))
 > (t '('c 'b 'a 2 pick))
 (Ok (list 'c 'a 'b 'c))
 > (t '('a 'b 'c 3 pick))
 (Error (copycat-missing-arguments 'pick 'copycat:pick 4 3))
 > (t '('a 'b 'c rot))
 (Ok (list 'a 'c 'b))
 > (t '('a 'b 'c 3 roll))
 (Ok (list 'a 'c 'b))
 > (t '('a 'b 'c -rot))
 (Ok (list 'b 'a 'c))
 > (t '('a 'b 'c rot rot))
 (Ok (list 'b 'a 'c))
 > (t '('x 'a 'b 'c rot -rot))
 (Ok (list 'c 'b 'a 'x))
 > (t '('x 'a 'b 'c nip))
 (Ok (list 'c 'a 'x))
 > (t '(1 2 3 clear))
 (Ok (list))
 > (t '(6 7 8 3 dropn))
 (Ok (list))
 > (t '(6 7 8 4 dropn))
 (Error (copycat-missing-arguments 'dropn (list 'dropn 4) 4 3))
 > (t '(6 7 8 -4 dropn))
 (Error (copycat-type-error 'dropn "(fixnum-natural0? n)" -4))
 > (t '("f" inv))
 (Error (copycat-type-error 'inv "(number? x)" "f"))
 > (t '("f" 5 +))
 (Error (copycat-type-error '+ "(number? a)" "f"))
 > (t '(5 "f" +))
 (Error (copycat-type-error '+ "(number? b)" "f"))

 > (t '(() 1 cons))
 (Ok (list (list 1)))

 > (t '("foo bar" #\space string.split strings-append))
 (Ok (list "foobar"))
 > (t '("foo bar" #\space .split strings-append))
 (Ok (list "foobar"))
 
 > (t '('f 42 +))
 (Error (copycat-type-error '+ "(number? a)" 'f))
 > (t '("foo" string.symbol 'bar))
 (Ok (list 'bar 'foo))
 > (t '(1 2 "+" .symbol 1 list eval))
 (Ok (list 3))

 ;; sublists are representing sub-programs, which are only evaluated
 ;; on demand:
 > (t '(4 (5) eval))
 (Ok (list 5 4))
 > (t '(4 (5 1 +) eval))
 (Ok (list 6 4))

 ;; Things can be explicitly made literals (prevented from
 ;; interpretation) by wrapping them (`..`) with `(quote ..)`, or the
 ;; S-expression `'` shortcut:
 > (t '((quote foo)))
 (Ok (list 'foo))
 > (t '('foo))
 (Ok (list 'foo))
 > (t '('1))
 (Ok (list 1))

 ;; "syntax-based" word definition form: |:| takes a name, and a
 ;; program to its right, syntactically
 > (t '(: square (dup *) 4 square))
 (Ok (list 16))
 ;; stack-based word definition form (works like a normal word):
 ;; |set!| takes a program and a name from the stack at runtime. (I
 ;; don't know why the original Cc chooses to use such
 ;; "syntax-based" features when it could do with program and symbol
 ;; quoting and then just words like this, other than visual
 ;; preference.)
 > (t '((dup *) 'sqr set! 4 sqr))
 (Ok (list 16))

 ;; "syntax-based" branching facility: takes a truebranch and a
 ;; falsebranch to its right, syntactically, as well as a boolean
 ;; value from the stack at runtime.
 > (t '(5 zero?))
 (Ok (list #f))
 > (t '(5 zero? THENELSE (1) (0)))
 (Ok (list 0))
 > (t '(0 zero? THENELSE (1) (0)))
 (Ok (list 1))
 ;; stack-based branching facility (works like a normal word): takes
 ;; boolean value, truebranch and falsebranch from the stack at
 ;; runtime
 > (t '(5 zero? (1) (0) thenelse))
 (Ok (list 0))
 > (t '(0 zero? (1) (0) thenelse))
 (Ok (list 1))
 ;; roll takes a number denoting the number of elements to rotate, and
 ;; rotates their position on the stack so that the last of those
 ;; becomes the first:
 > (t '(7 #t (yes) (no) 3 roll))
 (Ok (list #t (list 'no) (list 'yes) 7))

 ;; Test user-space |if| facility:
 > (t '(5 zero? (1) (0) if))
 (Ok (list 0))
 > (t '(0 zero? (1) (0) if))
 (Ok (list 1))
 ;; write a word-based branching facility ourselves, using the
 ;; stack-based one internally:
 > (t '((thenelse) 'if* set!))
 > (t '(5 zero? (1) (0) if*))
 (Ok (list 0))
 > (t '(0 zero? (1) (0) if*))
 (Ok (list 1))
 ;; alias the branching facility by simply storing it to a different
 ;; word:
 > (t '('if* ref 'anotherif set!))
 > (t '(5 zero? (1) (0) anotherif))
 (Ok (list 0))
 > (t '(0 zero? (1) (0) anotherif))
 (Ok (list 1))

 ;; factorial
 ;; The variant in examples/fact.scm
 > (t '("examples/fact.scm" load))
 (Ok (list))
 ;; > (t '(0) '(fact))
 ;; (Ok (list (source* 1 "examples/fact.scm" 2 25)))
 > (t '(0 fact source?))
 (Ok (list #t))
 > (t '(1 fact))
 (Ok (list 1))
 > (t '(2 fact))
 (Ok (list 2))
 > (t '(3 fact))
 (Ok (list 6))
 > (t '(20 fact))
 (Ok (list 2432902008176640000))
 ;; Or, with THENELSE syntax:
 > (t '(0 : fact2 (dup zero? THENELSE (drop 1) (dup 1 - fact *))))
 > (t '(20 fact2))
 (Ok (list 2432902008176640000))

 ;; <lis> <code> mymap
 ;; iterative version:
 > (t '(:
        rmymap-iter ;; <code> <lis> <result>
        (over
         pair?
         ( ;; change result
          over
          car pick3 eval cons
          ;; and lis
          swap cdr swap
          rmymap-iter)
         (over
          null?
          (swap drop swap drop) ;; optimize?
          ("improper list" error/1)
          if)
         if)
        :
        rmymap
        (swap
         ()
         rmymap-iter)))
 > (t '((1 2) (inc) rmymap))
 (Ok (list (list 3 2)))
 > (t '(:
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
        imymap
        (rmymap reverse)))
 > (t '(() (inc) imymap))
 (Ok (list (list)))
 > (t '((5 6 7) (inc) imymap))
 (Ok (list (list 6 7 8)))
 ;; recursive definition:
 > (t '(:
        mymap-recur ;; <fn> <lis> -> <fn> <res>
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
          mymap-recur
          ;; "after recur" P*
          rot
          cons)
         ()
         if)
        :
        mymap
        (swap mymap-recur swap drop)))
 > (t '((5 6 7) (inc) mymap))
 (Ok (list (list 6 7 8)))

 ;; test tail call optimization: this must run indefinitely and not
 ;; run out of memory:
 ;; > (cc-interpreter.eval (fresh-cc-interpreter) '(: lp (lp) lp))
 ;; (comment out (generate-proper-tail-calls #f) in .gambcini for this to work)


 ;; More error testing:
 > (t '(: foo))
 (Error (copycat-missing-arguments ': (list) 2 1))
 > (t '(: foo 4))
 ;; yes, could be more expressive. "not a program". ?
 (Error (copycat-type-error ': "list?" 4))
 > (t '(: foo () 4))
 (Ok (list 4)))


;; source handling
(TEST
 > (t (quote-source (9 .string)))
 (Ok (list "9"))
 > (t (quote-source ((10) first .string)))
 (Ok (list "10")))



(====cc-category) ;; prevent the previous setting from being carried
                  ;; over to other modules (meh)
