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

(include "lib/cj-standarddeclares.scm")

(TEST
 > (include "copycat-std-test--include.scm"))



(====cc-category (stack forth)
                 "As shown on http://wiki.laptop.org/go/Forth_stack_operators")

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
        "Take the last n elements on the stack, roll them around so
that the oldest one becomes the newest."
        ;; (letv ((args stack*) (split-at $s n))
        ;;       (append (cons (last args) (butlast args)) stack*))
        ;;or, saving on intermediaries:
        (let lp ((n n)
                 (tmp '())
                 (stack $s))
          (if (> n 1)
              (lp (dec n)
                  (cons (car stack) tmp)
                  (cdr stack))
              (Ok (cc-interpreter.stack-set
                   $cci
                   (cons (car stack)
                         (rappend tmp (cdr stack))))))))


(def (copycat:pick $word $cci $s n)
     (if-Just ((it (list-Maybe-ref $s n)))
              (cc-return it)
              (Error (copycat-missing-arguments
                      $word
                      'copycat:pick ;; XX?
                      (inc n)
                      (length $s)))))
(cc-def over (-> any?)
        "Copy the second-last element from the stack."
        (copycat:pick $word $cci $s 1))
(cc-def pick2 (-> any?)
        "Copy the third-last element from the stack."
        (copycat:pick $word $cci $s 2))
(cc-def pick3 (-> any?)
        "Copy the fourth-last element from the stack."
        (copycat:pick $word $cci $s 3))
(cc-def pick (n -> any?)
        "Copy the element from the stack found after skipping n
elements."
        (copycat:pick $word $cci $s n))


(====cc-category (stack christian)
                 "My own ideas for stack ops.")

(cc-def drop2 (a b ->)
        (cc-return))
(cc-def drop3 (a b c ->)
        (cc-return))
(cc-def drop4 (a b c d->)
        (cc-return))
(cc-def dropn ([fixnum-natural0? n] ->)
        "Drop the n last elements from the stack."
        (if-Just ((it (Maybe-drop $s n)))
                 (Ok (cc-interpreter.stack-set $cci it))
                 (Error
                  (copycat-missing-arguments $word
                                             (list 'dropn n)
                                             n
                                             (length $s)))))

(cc-def clear ()
        "Drop all elements from the stack."
        (Ok (cc-interpreter.stack-set $cci '())))

;; (cc-defguest 'c 'clear alias)
;; `alias` is not defined yet, thus move that to later


(====cc-category (booleans)
                 "Operations on booleans.")

(cc-defhost not ([boolean? v] -> boolean?)
            "Returns #t if `v` is #f, and #f if `v` is #t.")


(====cc-category (numbers)
                 "Operations on numbers.")

(cc-defhost string.maybe-number ([string? s] -> (maybe number?))
            "Convert given string to a number if possible, #f if not.")

(cc-defhost number? (v -> boolean?))
(cc-defhost exact? ([number? v] -> boolean?))
(cc-defhost inexact? ([number? v] -> boolean?))
(cc-defhost even? ([integer? v] -> boolean?))
(cc-defhost odd? ([integer? v] -> boolean?))
(cc-defhost real? (v -> boolean?))
(cc-defhost rational? (v -> boolean?)
            "Same as `real?` (and thus useless)?")
(cc-defhost complex? (v -> boolean?))
(cc-defhost integer? (v -> boolean?))
(cc-defhost fixnum? (v -> boolean?))

(cc-defhost + ([number? a] [number? b] -> number?))
(cc-defhost - ([number? a] [number? b] -> number?))

(cc-def neg ([number? a] -> number?)
        (cc-return (- a)))


;; for non-numbers as well:
(cc-defhost/try .neg (x -> -x)
                "Negate x (generic).")
;; make .neg also work on normal numbers:
(def. (number.neg x) (- x))


(cc-defhost * ([number? a] [number? b] -> number?))
(cc-def / ([number? a] [number? b] -> number?)
        (if (and (exact? b)
                 (zero? b))
            (Error (copycat-division-by-zero $word a))
            (cc-return (/ a b))))
(cc-def inv ([number? x] -> number?)
        "1/x"
        (cc-return (/ x)))
(cc-defhost inc ([incrementable-fixnum? n] -> fixnum?)
            "The successor number of `n`.")
(cc-defhost dec ([decrementable-fixnum? n] -> fixnum?)
            "The predecessor number to `n`.")
(cc-defhost square ([number? x] -> number?)
            "x * x")
(cc-defhost sqrt ([number? x] -> number?)
            "The square root of `x`.")
(cc-defhost expt ([number? base] [number? exponent] -> number?)
            "The base `base` raised to the power of `exponent`.")
(cc-defhost log ([number? x] -> number?)
            "The logarithm of `x` with base `e`.")
(cc-def log2 ([number? x] -> number?)
        "The logarithm of `x` with base `2`."
        (cc-return (/ (log x) (log 2))))
(cc-def log10 ([number? x] -> number?)
        "The logarithm of `x` with base `10`."
        (cc-return (/ (log x) (log 10))))


(cc-defhost zero? ([number? a] -> boolean?)
            "Whether `a` is `0`.")
(cc-defhost = ([number? a] [number? b] -> boolean?)
            "Whether `a` is numerically equal to `b`.")
(cc-defhost < ([number? a] [number? b] -> boolean?)
            "Whether `a` is smaller than `b`.")
(cc-defhost <= ([number? a] [number? b] -> boolean?)
            "Whether `a` is smaller than or equal to `b`.")
(cc-defhost > ([number? a] [number? b] -> boolean?)
            "Whether `a` is larger than `b`.")
(cc-defhost >= ([number? a] [number? b] -> boolean?)
            "Whether `a` is larger than or equal to `b`.")
(cc-def != ([number? a] [number? b] -> boolean?)
        "Whether `a` and `b` are numerically unequal."
        (cc-return (not (= a b))))
(cc-defhost eq? (a b -> boolean?)
            "Whether `a` and `b` are the same object.")
(cc-def !eq? (a b -> boolean?)
        "Whether `a` and `b` are *not* the same object."
        (cc-return (not (eq? a b))))


(cc-defhost/try .length (a -> fixnum-natural0?)
                "Try to call the length method on `a`.")


(====cc-category (lists)
                 "Operations on linked lists.")

(cc-defhost list? (v -> boolean?)
            "Return true if `v` is a proper list.")
(cc-defhost ilist? (v -> boolean?)
            "Return true if `v` starts off as a proper list (careful,
does not check to the end of the list, for performance).")

(cc-def cons (l e -> pair?) "Prepend `e` to the given list `l`."
        (cc-return (cons e l)))
(cc-defhost car ([pair? a] -> any?)
            "Return the element slot from the given pair.")
(cc-defhost cdr ([pair? a] -> any?)
            "Return the rest slot of the given pair.")
(cc-defhost first ([pair? l] -> any?)
            "Return the first element of the given list.")
(cc-defhost rest ([pair? l] -> any?)
            "Drop the first element from the given list.")
(cc-def first+rest ([pair? l] -> any? any?)
        "Returns the two slots in the pair."
        ;; XX should cc-return generally work in the other direction?
        (cc-return (rest l) (first l)))

(TEST
 > (t '((3 4 5) first+rest))
 (Ok (list 3 (list 4 5))))

(cc-defhost pair? (a -> boolean?)
            "Whether `a` is a pair (as used by a non-empty list).")
(cc-defhost null? (a -> boolean?)
            "Whether `a` is an empty list.")

(def (cc:Rlist $cci $s $word numargs reverse?)
     (if-Just ((it (Maybe-split-at-reverse $s numargs)))
              (letv ((rargs stack*) it)
                    (Ok (cc-interpreter.stack-set
                         $cci
                         (cons (if reverse? rargs (reverse rargs))
                               stack*))))
              (Error (copycat-missing-arguments $word
                                                'Rlist ;; ?
                                                numargs
                                                (length $s)))))
(cc-def list ([fixnum-natural0? n] -> (list-of-length n))
        "Takes `n` elements from the stack and returns them as a
list."
        (cc:Rlist $cci $s $word n #t))
(cc-def rlist ([fixnum-natural0? n] -> (list-of-length n))
        "Takes `n` elements from the stack and returns them as a
reversed list."
        (cc:Rlist $cci $s $word n #f))

(cc-defhost/try append (a b -> ilist?)
                ;; XX argument order?
                "Append the lists `a` and `b`.")

(cc-defhost/try list-drop ([ilist-of-possibly-source? l]
                           [fixnum-natural0? n] -> ilist?)
                "Drop `n` elements from the start of `l`.")
;; ^ Todo: better error messages would be good. Really want
;; Maybe-list-drop or even Result-list-drop. Or proper exceptions.

(cc-defhost/try list-take ([ilist-of-possibly-source? l]
                           [fixnum-natural0? n] -> ilist?)
                "Take `n` elements from the start of `l`.")

(cc-def list-rmap ([ilist-of-possibly-source? l]
                   [ilist-of-possibly-source? prog] -> ilist?)
        "Create the list that, for each element value `v` in reverse
order of those in `l`, contains the value left at the top of the stack
after putting `v` on the stack and running `prog`."
        ;; also see alternative map definition created in the test
        ;; suite below!
        (let lp ((res '())
                 (l (source-code l))
                 ($cci $cci))
          (if (null? l)
              (let ($s (cc-interpreter.stack $cci))
                ;; onto current $cci and $s:
                (cc-return res))
              (if-let-pair
               ((a l*) l)
               (==> (cc-interpreter.push* $cci a $word) ;; or use free push ?
                    (cc-interpreter.eval prog)
                    ((lambda (cci*)
                       (if-let-pair
                        ((b stack**) (cc-interpreter.stack cci*))
                        (lp (cons b res)
                            l*
                            (cc-interpreter.stack-set cci* stack**))
                        (Error (copycat-missing-arguments
                                $word
                                ;; (XX why do I check for stack type
                                ;; errors below but not here? Use
                                ;; typed-list ?)
                                (list "empty stack after running prog:"
                                      prog) ;; XX evil
                                1
                                0))))))
               (Error (copycat-type-error $word
                                          "list?"
                                          l))))))

(cc-defhost list-reverse ([list? l] -> ilist?)
            "Reverse the order of the elements in `l`.")

(cc-defhost/try list-ref ([list? l] [fixnum-natural0? i] -> ilist?)
                "Get item at index `i` in `l`.")

(cc-defhost list-length ([list? l] -> fixnum-natural0?)
            "The number of items in `l`.")

(cc-defguest (: list-map
                [ilist-of-possibly-source? l]
                [ilist-of-possibly-source? prog] -> ilist?
                "Create the list that, for each element value `v` in `l`,
contains the value left at the top of the stack after putting `v` on
the stack and running `prog`."
                (list-rmap list-reverse)))

(TEST
 > (t '((1 4 5) (inc square) list-map))
 (Ok (list (list 4 25 36)))
 > (t (quote-source ((1 4 5) (inc square) list-rmap)))
 (Ok (list (list 36 25 4)))

 > (=> (.eval (fresh-cc-interpreter)
              (quote-source ((1 4 . 5) (inc square) list-map)))
       ;; reports list-rmap location within list-map; how to track original?
       ;; Well, todo call stack inspection. Anyway, strip it here:
       Error.value
       ((dup copycat-type-error.predicate
             (comp source-code copycat-type-error.value))))
 ("list?" 5))

