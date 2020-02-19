;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         copycat-interpreter
         copycat-std-part1
         test)

(export)
;; XX offer them in an exported fashion instead of mutating the global
;; symbol table?

(include "lib/cj-standarddeclares.scm")

(TEST
 > (include "copycat-std-test--include.scm"))


(====cc-category (vectors)
                 "Operations on vectors (arrays).")

(cc-defhost vector? (v -> boolean?)
            "Whether `v` is a vector.")

(cc-defhost vector.length ([vector? vec] -> fixnum-natural0?)
            "The number of items in `vec`.")


(def (cc:Rvector $word $cci $s numargs reverse? drop-args?
                 make-VECTOR VECTOR-set! item? item?-string)
     -> copycat-runtime-result?
     (let ((v (make-VECTOR numargs))
           (end (dec numargs)))
       (let lp ((i end)
                (s $s))
         (if (negative? i)
             (if drop-args?
                 (=> (cc-interpreter.stack-set $cci s)
                     (cc-interpreter.push v)
                     Ok)
                 (cc-return v))
             (if-let-pair ((a r) s)
                          (if (item? a)
                              (begin (VECTOR-set! v
                                                  (if reverse? i (- end i))
                                                  a)
                                     (lp (dec i) r))
                              (Error (copycat-type-error $word
                                                         item?-string
                                                         a)))
                          (Error (copycat-missing-arguments $word
                                                            'Rvector ;; ?
                                                            numargs
                                                            (length $s))))))))
(cc-def vector ([fixnum-natural0? n] -> (list-of-length n))
        "Takes `n` elements from the stack and returns them as a
vector."
        (cc:Rvector $word $cci $s n #t #t
                    make-vector vector-set! any? "bug"))

(cc-def rvector ([fixnum-natural0? n] -> (list-of-length n))
        "Takes `n` elements from the stack and returns them as a
reversed vector."
        (cc:Rvector $word $cci $s n #f #t
                    make-vector vector-set! any? "bug"))

(cc-def copy-vector ([fixnum-natural0? n] -> (vector-of-length n))
        "Copy `n` elements from the stack (leaving them there) and
returns them as a vector."
        (cc:Rvector $word $cci $s n #t #f
                    make-vector vector-set! any? "bug"))

(cc-def copy-rvector ([fixnum-natural0? n] -> (vector-of-length n))
        "Copy `n` elements from the stack (leaving them there) and
returns them as a reversed vector."
        (cc:Rvector $word $cci $s n #f #f
                    make-vector vector-set! any? "bug"))


(cc-def vector.ref ([vector? v] [fixnum-natural0? i] -> any?)
        "Retrieve from `v` the element at index `i`."
        (if (< i (vector-length v))
            (cc-return (vector-ref v i))
            (Error (copycat-out-of-bounds-access $word
                                                 i
                                                 (vector-length v)))))

(cc-def vector.set! ([vector? v] [fixnum-natural0? i] val -> vector?)
        "Set the element at index `i` in `v` to `val`, via mutation;
returns `v`."
        (if (< i (vector-length v))
            (begin (vector-set! v i val)
                   (cc-return v))
            (Error (copycat-out-of-bounds-access $word
                                                 i
                                                 (vector-length v)))))

;; This is a different implementation than vector.set in Scheme (to
;; get nice error message); XX D'oh is that this means that Copycat's
;; |.set| on vectors behaves slightly different than |vector.set|.
(cc-def vector.set ([vector? v] [fixnum-natural0? i] val -> vector?)
        "Returns a copy of `v` with the element at index `i` set to
`val`."
        (if (< i (vector-length v))
            (let (v (vector-copy v))
              (vector-set! v i val)
              (cc-return v))
            (Error (copycat-out-of-bounds-access $word
                                                 i
                                                 (vector-length v)))))


(TEST
 > (t '('a 'b 'c 2 rvector)) 
 (Ok (list (vector 'c 'b) 'a))
 > (t '('a 'b 'c 2 vector)) 
 (Ok (list (vector 'b 'c) 'a))
 > (t '('a 'b 'c 2 copy-rvector)) 
 (Ok (list (vector 'c 'b) 'c 'b 'a))
 > (t '('a 'b 'c 2 copy-vector)) 
 (Ok (list (vector 'b 'c) 'c 'b 'a))
 > (t '([a b c] 2 vector.ref))
 (Ok (list 'c))
 > (t '([a b c] 3 vector.ref))
 (Error (copycat-out-of-bounds-access 'vector.ref 3 3))
 > (t '([a b c] dup 0 "hi" vector.set!))
 (Ok (list (vector "hi" 'b 'c) (vector "hi" 'b 'c)))
 > (t '([a b c] dup 0 "hi" vector.set))
 (Ok (list (vector "hi" 'b 'c) (vector 'a 'b 'c))))



(====cc-category (strings)
                 "Operations on unicode capable strings.")

(cc-defhost string? (v -> boolean?)
            "Whether `v` is a string.")
(cc-defhost string.length ([string? str] -> fixnum-natural0?)
            "The number of characters in str.")
(cc-defhost string.append ([string? a] [string? b] -> string?)
            "Append the two strings `a` and `b`.")
(cc-defhost/try strings-append ([(ilist-of string?) l] -> string?)
                "Append all the elements (which must be strings) in l.")
(cc-defhost/try strings-join ([(ilist-of string?) l] [string? inbetween]
                              -> string?))
(cc-defhost string.split ([string? str]
                          ;; XX todo: allow forth preds by
                          ;; wrapping
                          [char? char-or-pred]
                          -> (ilist-of string?)))

(def. string.chomp chomp) ;; consistency

(cc-defhost string.chomp ([string? str] -> string?)
            "Remove one trailing newline character from str if present.")

(cc-def string ([fixnum-natural0? n] -> string?)
        "Take `n` elements from the stack (which must be chars) and
make a string out of them."
        (cc:Rvector $word $cci $s n #t #t
                    make-string string-set! char? "char?"))

(cc-def rstring ([fixnum-natural0? n] -> string?)
        "Take `n` elements from the stack (which must be chars) and
make a string out of them in reverse order."
        (cc:Rvector $word $cci $s n #f #t
                    make-string string-set! char? "char?"))

(cc-def copy-string ([fixnum-natural0? n] -> string?)
        "Copy `n` elements from the stack (which must be chars) and
make a string out of them."
        (cc:Rvector $word $cci $s n #t #f
                    make-string string-set! char? "char?"))

(cc-def copy-rstring ([fixnum-natural0? n] -> string?)
        "Copy `n` elements from the stack (which must be chars) and
make a string out of them in reverse order."
        (cc:Rvector $word $cci $s n #f #f
                    make-string string-set! char? "char?"))

(TEST
 > (t '(#\a #\b #\c 3 string))
 (Ok (list "abc"))
 > (t '(#\a #\b #\c 3 rstring))
 (Ok (list "cba"))
 > (t '(#\a #\b #\c 3 copy-string))
 (Ok (list "abc" #\c #\b #\a))
 > (t '(#\a #\b #\c 3 copy-rstring))
 (Ok (list "cba" #\c #\b #\a)))

(cc-defhost/try .string (s -> string?)
                "Apply the .string method to s, which is expected to
convert s to a string or extract a string from s.")

(cc-defhost/try .split (s item-or-pred -> ilist?)
                "Try to call the .split method on `s`, which is
supposed to split `s` into multiple parts of the same type as `s`,
with `item-or-pred` representing the items denoting the places that
should be cut out and leading to the fragments. Is *not* supposed to
coalesce multiple matching items in a row into a single hole (instead,
it will return empty fragments from between those items in the
result).")


(====cc-category (chars)
                 "Operations on unicode capable characters.")

(cc-defhost char? (v)
            "Whether v is a character.")
(cc-defhost char.integer ([char? c] -> fixnum?))
(cc-defhost/try integer.char (n -> char?))

(cc-defhost string.list ([string? s] -> (ilist-of char?))
            "Convert string s into a list of all of its characters.")
(cc-defhost char-list.string ([(list-of char?) l] -> string?)
            "Convert string s into a list of all of its characters.")


(====cc-category (lists)
                 (strings)
                 (vectors))

(cc-defhost/try .ref (v [fixnum-natural0? i] -> any?)
                "Try to call the ref method on the (supposedly
vector-like) `v`, returning the element of `v` at index `i`.")

(cc-defhost/try .set (v [fixnum-natural0? i] val -> v*)
                "Try to call the set method on the (supposedly
vector-like) `v`, returning a new version of `v` that contains `val`
at index `i`.")

(cc-defhost/try .append (a b -> c)
                "Try to call the append method on the given arguments,
returning a container of the same type as a but with c appended.")


(TEST
 > (equal? (t* '(10 11 12 3 vector dup 1 "hi" .set))
           (Ok '([10 "hi" 12] [10 11 12])))
 #t
 > (t '("foo" "bar" .append))
 (Ok (list "foobar"))
 > (t '("hello" 1 .ref))
 (Ok (.list "e")))


(====cc-category (strings)
                 (vectors))

(cc-def .set! (v [fixnum-natural0? i] val -> v)
        "Try to call the set! method on the (supposedly vector-like)
`v`, mutating `v` to contain `val` at index `i`. The identical (but
mutated) `v` is returned for consistency with the `.set` method."
        (mdo (copycat:try-Ok (.set! v i val))
             (cc-return v)))

(TEST
 > (equal? (t* '(10 11 12 3 vector dup 1 "hi" .set!))
           (Ok '([10 "hi" 12] [10 "hi" 12])))
 #t
 > (equal? (t* '(#\a #\b #\c 3 string dup 1 #\Z .set!))
           (Ok '("aZc" "aZc")))
 #t)


(====cc-category (control-flow maybe)
                 "The 'maybe' type is inhabited by either the boolean
#f value representing a missing value, or any other value which is
representing the case of a present value; this prohibits the use of
the #f value as part of present values (and can lead to mistakes), but
can also be convenient (TODO: offer nesting 'Maybe' type)."
                 (booleans maybe))

(cc-def if-just ([any? a]
                 [ilist-of-possibly-source? then]
                 [ilist-of-possibly-source? else])
        "Unlike `if`, this accepts non-boolean values for `a`, in
which case `a` is put back and the `then` branch is evaluated,
otherwise `else` is evaluated (without putting back the false value
from `a`)."
        (if a
            (=> (cc-interpreter.push $cci a)
                (cc-interpreter.eval then))
            (cc-interpreter.eval $cci else)))

(TEST
 > (t '(#f ('yes) ('no) if-just))
 (Ok (list 'no))
 > (t '(#t ('yes) ('no) if-just))
 (Ok (list 'yes #t))
 > (t '("other" ('yes) ('no) if-just))
 (Ok (list 'yes "other"))
 ;; unlike:
 > (t '(#t ('yes) ('no) if))
 (Ok (list 'yes))
 > (t '("other" ('yes) ('no) if))
 (Error (copycat-type-error 'if "(boolean? val)" "other")))


(cc-def and ([any? a] [ilist-of-possibly-source? b] -> any?)
        "This is not strictly a boolean operator, but a 'maybe' type
style one (monadic >>)."
        (if a
            (cc-interpreter.eval $cci b)
            (cc-return a)))

(cc-defguest (: maybe->>= [any? a] [ilist-of-possibly-source? b] -> any?
                "This is the monadic >>= ('bind') operator for the
'maybe' type: if `a` is #f, it will return #f; otherwise, it will put
`a` back on the stack (unlike `and` which does not do this) and
evaluate `b`."
                ((#f) if-just)))

(TEST
 > (def i (t* '(#f (10 +) maybe->>=)))
 > (cj-desourcify i)
 [(Ok) (#f)]
 ;; (Ok (list #f)) but #f has location information
 > (=> i (vector-ref 1) .first maybe-source-location just?)
 #t
 > (t '(11 (10 +) maybe->>=))
 (Ok (list 21)))


(cc-def or ([any? a] [ilist-of-possibly-source? b] -> any?)
        "This is not strictly a boolean operator, but a 'maybe' type
style one."
        (if a
            (cc-return a)
            (cc-interpreter.eval $cci b)))

(TEST
 > (t '(#f (1 2 +) and))
 (Ok (list #f))
 > (t '(9 (1 2 +) and))
 (Ok (list 3))
 > (t '(#f (1 2 +) or))
 (Ok (list 3))
 > (t '(9 (1 2 +) or))
 (Ok (list 9)))


(cc-defhost/try cat-maybes ([ilist? l] -> ilist?)
                "Remove the items in l which are #f.")



;; (cc-defhost error/1 (a))
;; (cc-defhost error/2 (a))


(====cc-category (control-flow Maybe)
                 "Similar to `maybe`, a type that represents two
cases, one with a value and one without, but unlike `maybe`, `Maybe`
is a wrapper, which means it can enclose any value, including the
missing value (it can nest).")

(cc-defhost Maybe? (v -> boolean?)
            "Whether `v` is wrapped in a `Maybe` type (an `Ok` or
`Just` object).")

(cc-defhost Just? (v -> boolean?)
            "Whether `v` is wrapped in a `Just` object.")
(cc-defhost Nothing? (v -> boolean?)
            "Whether `v` is the `Nothing` value.")

(cc-defhost Just (v -> Maybe?)
            "Wrap `v` in a `Just` object.")
(cc-defhost Nothing (-> Maybe?)
            "The `Nothing` value.")


(cc-def if-Just ([Result? v]
                 [ilist-of-possibly-source? then]
                 [ilist-of-possibly-source? else])
        "If `v` is a `Just` object, put its enclosed value on the
stack and evaluate the `then` branch; otherwise, evaluate the `else`
branch."
        (if-Just ((it v))
                 (==> (cc-interpreter.push* $cci it $word)
                      ;; ^ or use free push ?
                      (cc-interpreter.eval then))
                 (cc-interpreter.eval $cci else)))


(====cc-category (control-flow Maybe)
                 (lists Maybe))

(cc-defhost/try cat-Maybes ([(ilist-of Maybe?) l] -> ilist?)
                "Remove the items in l which are Nothing, and
return the others unwrapped.")


(TEST
 > (t (quote-source (c 'a Just 'b Just Nothing 'c Just slurp-stack cat-Maybes)))
 (Ok (list (list 'c 'b 'a))))


(====cc-category (control-flow Result)
                 "The 'Result' type is of the two cases 'Ok' and
'Error'.")


(cc-defhost Ok (v)
            "Wrap v in an Ok (Result type).")
(cc-defhost Error (v)
            "Wrap v in an Error (Result type).")

(cc-def if-Ok ([Result? v]
               [ilist-of-possibly-source? then]
               [ilist-of-possibly-source? else])
        (let (cont (lambda (it branch)
                    (==> (cc-interpreter.push* $cci it $word) ;; or use free push ?
                         (cc-interpreter.eval branch))))
          (if-Ok v (cont it then) (cont it else))))


(====cc-category (control-flow exceptions)
                 (control-flow Result exceptions)
                 "Switch between implicit (propagation) and explicit
error handling.")

(cc-def try ([ilist-of-possibly-source? prog]
             -> (Result-of ilist?
                           copycat-error?))
        "`eval` `prog`, catching exceptions, returning a `Result` --
either an `Ok`-wrapped stack, or an `Error`-wrapped copycat error
object."
        (if-Ok (cc-interpreter.eval $cci prog)
               ;; use new $cci, but set stack based on the original $s
               ;; (cc-return takes both variables from the scope)
               (let ($cci it)
                 (cc-return (Ok (cc-interpreter.stack $cci))))
               ;; XXX If it fails, we don't get to know how much fuel
               ;; it cost!!! Failing calculations are free currently!
               (cc-return it-Result)))

(cc-def raise ([copycat-error? e] -> !)
        "Raise an exception with `e` as the error value. Does not
return (continue evaluation of the current program)."
        ;; XX: re "does not return" type and docs: what about error
        ;; handlers in the future? "!" is tied to the implicit error
        ;; handler here.
        (Error e))

(cc-def unwrap ([(Result-of any? copycat-error?) result] -> (either any? !))
        "If result is an `Ok`, return its contained value. If result
is an `Error`, raise its value as an exception."
        (if-Ok result
               (cc-return it)
               result))

(cc-def generic-error ([string? msg] [ilist? args] -> copycat-generic-error?)
        "Wrap the given arguments in a copycat-generic-error object
which can be raised as an exception."
        (cc-return (copycat-generic-error $word msg args)))

(cc-def exit-repl-error (-> copycat-exit-repl?)
        "Create an exit-repl error object."
        (cc-return (copycat-exit-repl $word)))


(cc-def assertment-failure-error (-> copycat-assertment-failure?)
        "Create an assertment-failure error object."
        (cc-return (copycat-assertment-failure $word)))

(cc-defguest (: error [string? msg] [ilist? args] -> !
                "Raise a generic exception with the given message and
arguments."
                (generic-error raise))

             (: assert [boolean? t] ->
                "If `t` is #t, does nothing; if it is #f, raise an
assertment-failure exception."
                ((assertment-failure-error raise) unless))

             (: quit ->
                "Raise an exception of type , which, if uncaught,
instructs the current Copycat repl to end the loop (or the current
interpreter to return with the exception as an error)."
                (exit-repl-error raise)))

(cc-defhost/try .explanation (o -> string?)
                "Expected to return a string for copycat-error objects.")


(TEST
 > (t '("before"
            (je765etzc) try
            ("dead") (.explanation) if-Ok))
 (Ok (list "the given word is not defined" "before"))
 > (if-Ok (t* (quote-source ("hi" (1) error "there")))
          'huh
          ((dup copycat-generic-error? .msg .args) it))
 (#t "hi" (1))
 > (t '("before" "some" ("thing") generic-error Error unwrap "hi"))
 (Error (copycat-generic-error 'generic-error "some" (list "thing"))))


(====cc-category (control-flow Result exceptions)
                 (stack christian))

(cc-def set-stack ([ilist-of-possibly-source? stack])
        "Replace the stack contents with `stack`."
        (Ok (cc-interpreter.stack-set $cci (source-code stack))))

(cc-def get-stack (-> ilist?)
        "Put the current stack (as a list) on top of the current
stack."
        (cc-return $s))

(cc-def slurp-stack (-> ilist?)
        "Get the current stack (as a list) as a single item in a fresh
stack."
        (Ok (cc-interpreter.stack-set $cci (list $s))))


(TEST ;; Result
 > (t '(39 Ok ("yes") ("no") if-Ok))
 (Ok (list "yes" 39))
 >  (t '(40 Error ("yes") ("no") if-Ok))
 (Ok (list "no" 40))
 > (t '("before" ("yes") try))
 (Ok (list (Ok (list "yes" "before")) "before"))
 > (t '("before" ("hi" "yes" inc) try))
 ;; Errors drop the intermediate results (and don't retain them in the
 ;; exception value anymore, unlike in some earlier version of
 ;; copycat).
 (Ok (list (Error (copycat-type-error 'inc "(incrementable-fixnum? n)" "yes"))
           "before"))
 > (t '("before" ("yes") try (set-stack) ("bug") if-Ok))
 (Ok (list "yes" "before")))


