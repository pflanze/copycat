;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         (string-quote shell-quote)
         copycat-interpreter
         copycat-std-part2
         test)

(export)
;; XX offer them in an exported fashion instead of mutating the global
;; symbol table?

(include "lib/cj-standarddeclares.scm")

(TEST
 > (include "copycat-std-test--include.scm"))



(====cc-category (internal fuel)
                 (fuel))

(cc-def set-fuel ([fixnum-natural0? fuel])
        "Set amount of interpreter fuel to the given new value."
        (Ok (cc-interpreter.fuel-set $cci fuel)))

(cc-def add-fuel ([fixnum? fuel])
        "Add amount of interpreter fuel."
        (let (fuel* (+ (cc-interpreter.fuel $cci) fuel))
          (if (fixnum-natural0? fuel*)
              (Ok (cc-interpreter.fuel-set $cci fuel*))
              (Error (copycat-out-of-range $word "fixnum-natural0?" fuel*)))))



(cc-def eval ([ilist-of-possibly-source? prog])
        "Evaluate prog (a list of instructions)."
        (cc-interpreter.eval $cci prog))

(cc-def nop (->)
        "No operation."
        (cc-return))


(====cc-category (environment))

(cc-def set! ([(either ilist-of-possibly-source? ccproc?) prog]
              [(possibly-source-of symbol?) name] ->)
        "Set the word with the given name to prog, which must be
either a list of instructions, or a ccproc data structure as retrieved
from `ref`."
        (let (prog (xcond ((ccproc? prog)
                           prog)
                          ((ilist-of-possibly-source? prog)
                           (ccguestproc (or (maybe-source-location prog)
                                            (maybe-source-location name)
                                            (maybe-source-location $word))
                                        #f ;; docstring
                                        (cc-type-unknown #f)
                                        (copycat-interpreter:current-categories)
                                        prog))))
          (cc-word-set! (source-code name) prog))
        (cc-return))

(====cc-category (environment cc-categories)
                 "Categories (of the cc-category type) are used in
procedure definitions. They have a path which is a list of
symbols (which is shown by `help` as a single string with `/` between
the elements), and optionally a docstring describing their
intent. Each procedure holds a list of categories. To avoid having to
specify the categories on every procedure definition individually,
they can be set via `set-current-cc-categories`--subsequent procedure
definitions pick up the categories from there.")

(cc-defhost cc-category ([(list-of symbol?) path]
                         [(maybe string?) maybe-docstring] -> cc-category?)
            "Create a `cc-category` object for use with
`set-current-cc-categories`. The docstring is for the category itself,
not for subsequently created procedures.")

(cc-defhost/try .maybe-docstring (s))
(cc-defhost/try .path (s))

(TEST
 > (t '((some where) "somehelp" cc-category dup .path swap .maybe-docstring))
 (Ok (list "somehelp" (list 'some 'where)))
 > (t '((some where) "somehelp" cc-category 1 list set-current-cc-categories
        (: fun ()) 'fun help-string))
 (Ok (list "----------------------------------------------------------\nfun: \n\nCategories:\n  some/where\nNames:\n  fun\n----------------------------------------------------------\n")))


(cc-def current-cc-categories (-> (list-of cc-category?))
        "Get the current list of categories to be used to create
procedures via `:` or `set!`."
        (cc-return (copycat-interpreter:current-categories)))

(cc-def set-current-cc-categories ([(list-of cc-category?) categories] ->)
        "Set the current list of categories to be used to create
procedures via `:` or `set!`."
        (copycat-interpreter:current-categories categories)
        (cc-return))

(cc-def with-cc-categories ([(list-of cc-category?) categories]
                            [ilist-of-possibly-source? prog])
        "Set the current list of categories to be used to create
procedures via `:` or `set!` while `prog` is running (i.e. in `prog`'s
dynamic scope)."
        ;; Relies on Scheme's scoping, assumes that is tied to scoping
        ;; in the Guest language; which of course will cease to be the
        ;; case once first-class continuations are
        ;; implemented. (Well... could rely on Scheme's first-class
        ;; continuations... should it, would it be easier to compile
        ;; to? But, no good for compiling the interpreter to other
        ;; languages.)
        (parameterize ((copycat-interpreter:current-categories categories))
          (cc-interpreter.eval $cci prog)))

(cc-defhost cc-category-lookup ([(list-of symbol?) path] -> (maybe cc-category?))
            "Get the cc-category info globally registered for the
given category path.")

(cc-defhost get-cc-category-list (-> (list-of cc-category?))
            "Get all globally registered cc-categories (sorted by
their path).")

(cc-defhost get-cc-category-paths (-> (list-of (list-of symbol?)))
            "Get all globally registered cc-category paths (sorted).")

(cc-defguest (: category-path.maybe-docstring [(list-of symbol?) path] -> (maybe string?)
                "Look up docstring for given category path."
                (cc-category-lookup (.maybe-docstring) maybe->>=))

             (: maybe-category-docstring [cc-category? cat] -> (maybe string?)
                "Look up docstring for given cc-category (unlike
.maybe-docstring run directly on cat, this looks up the *registered*
cc-category associated with the contained path and gets the docstring
from there)."
                (.path category-path.maybe-docstring))

             (: .categories-string
                (.categories (.path (.string) list-map "/" strings-join)
                             list-map
                             "\n  " strings-join)))

(TEST
 > (t '('(@not-runtime-but-set-by-test) #f cc-category 1 list
        set-current-cc-categories))
 (Ok (list))
 > (equal? (t* '(
                 '(test-category) "category for testing" cc-category 1 list
                 ((: hi "test definition"('there))) with-cc-categories
                 (: hi2 "not there" (not))
                 'hi ref .categories (.path) list-map
                 'hi2 ref .categories (.path) list-map))
           (Ok '(((@runtime)) ((test-category)))))
 #t)


(====cc-category (environment symbols)
                 "Symbols are identifying items in an environment;
currently there's just one global environment.")

(cc-defhost string.symbol ([string? s] -> symbol?)
            "Turn `s` into a symbol with the same name.")

(cc-defhost/try .symbol (s -> symbol?)
                "Method to try to turn `s` into a symbol.")

(cc-defhost symbol.string ([symbol? s] -> string?)
            "Return the underlying name string of the given symbol.")

(cc-defhost symbol? ([any? v] -> boolean?)
            "Whether `v` is a symbol.")

(cc-def ref ([symbol? name] -> ccproc?)
        "Return the ccproc data structure associated with name (giving
an error if not bound)."
        (if-Just ((v (table.Maybe-ref cc-words name)))
                 (cc-return v)
                 (Error (copycat-unbound-symbol $word name))))

(cc-defguest (: alias [symbol? new] [symbol? old] ->
                "Make `new` the same as `old`."
                (ref swap set!)))

;; moved here so alias is defined
(cc-defguest 'c 'clear alias)


(TEST
 > (t (quote-source ('blabla 'help alias 'blabla ref .docstring source-code)))
 (Ok (list "Print help on the given word.")))


