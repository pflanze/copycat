;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         copycat-interpreter
         copycat-std-part4
         (copycat-transcript new-transcript)
         test)

(export)
;; XX offer them in an exported fashion instead of mutating the global
;; symbol table?

(include "lib/cj-standarddeclares.scm")

(TEST
 > (include "copycat-std-test--include.scm"))



(====cc-category (development)
                 "Development aids.")

(====cc-category (development files)
                 "Developing with files.")

(====cc-category (development files transcripts)
                 "Transcripts are files onto which the current repl
writes all successfully parsed user inputs, and in case of errors the
error message in a commet. Transcripts can be loaded via `load`,
although evaluation stops on the first failing statement; the idea is
to edit transcripts into a correct and useful shape.")

(cc-def new-transcript! (->)
        "Closes the current transcript file (if any), and opens a new
one. Note that if the stack isn't empty when running
`new-transcript!`, loading the transcript file later with an empty
stack may fail or not yield the same results."
        (when-just (.maybe-transcript-port $cci)
                   (close-port it))
        (Ok (.maybe-transcript-port-set $cci (new-transcript))))

(cc-def current-maybe-transcript-port (-> (maybe output-port?))
        "Return the current transcript port, if any."
        (cc-return (.maybe-transcript-port $cci)))


(====cc-category (development debugging)
                 "Debugging aids.")

(cc-def D ()
        "Enter a (nested) Copycat repl with the current stack /
machine state."
        ;; XX HACK: defined in copycat.scm which currently
        ;; depends on us
        (Ok (=> (.repl-level-inc $cci)
                _cc-repl
                .repl-level-dec)))

(cc-def DScheme ()
        "Print stack, enter a Scheme repl; enter `,(c something)` to
continue (where `something` could be `(Ok $cci)`--if `something` is a
`copycat-runtime-result?`, it is used as the new interpreter state or
error, otherwise the old one, `$cci`, is reused)."
        (mdo (copycat:try-Ok (pretty-print $s))
             (let (r (##repl))
               (if (copycat-runtime-result? r)
                   r
                   (begin
                     (warn "continuing with old interpreter state")
                     (Ok $cci))))))

(cc-def P (->)
        "Print the location of P and then the current stack (with
location info stripped)."
        (mdo (copycat:try-Ok
              (show-source-location $word)
              (pretty-print (cj-desourcify $s)))
             (cc-return)))

(cc-def PM ([string? msg] ->)
        "Print `msg` and then the current stack."
        (mdo (copycat:try-Ok (display msg)
                             (display ": ")
                             (pretty-print (cj-desourcify $s)))
             (cc-return)))

(cc-def PS (->)
        "Print the location of P and then the current stack, via
`.show` and with location info not stripped."
        (mdo (copycat:try-Ok
              (show-source-location $word)
              (pretty-print (.show $s)))
             (cc-return)))


(====cc-category (development benchmarking))

(cc-def time ([ilist-of-possibly-source? prog])
        "Runs `prog` then prints how long it took."
        (time-thunk (lambda ()
                      (cc-interpreter.eval $cci prog))
                    ;; Still showed in a Scheme-y way of course,
                    ;; though, `(time ,prog). Todo: improve?
                    (cj-desourcify prog)))

;; XX can't redirect time output, it's goint to console-port which
;; can't(?) be redirected. Sigh. Analyze/patch Gambit.
'(TEST
 > (t '((10 30 +) time))
 (Ok (list 40)))


(====cc-category (development help)
                 "Getting help about words (procedures).

Also see the category `(environment cc-categories)`.")

(cc-def dir (-> (ilist-of symbol?))
        "Returns the list of defined words."
        (cc-return (table.sorted-keys cc-words)))

;; on ccproc
(cc-defhost/try .docstring (s))
(cc-defhost/try .type (s))
(cc-defhost/try .categories (s))
;; on cc-type
(cc-defhost/try .maybe-original (s))

(cc-defguest 'categories 'get-cc-category-list alias
             
             (: proc-names [ccproc? v] -> (ilist-of symbol?)
                "Give the list of all names that map to `v`."
                (
                 ;; make program
                 (over ref eq? (Just) (drop Nothing) if) swap cons
                 dir
                 swap list-map
                 cat-Maybes))

             (: proc-names-string ;; similar to .categories-string
                (proc-names (pretty-string) list-map
                         "  " strings-join))

             (: help-string [symbol? word] -> string?
                "Give help string on the given word."
                (
                 dup .string ": " string.append ;; intro
                 swap ref
                 dup
                 .type .maybe-original (pretty-string string.chomp)
                 maybe->>= ;; maybe type
                 ("") or ;; still get a newline
                 swap
                 dup 
                 .docstring source-code
                 ("\n" swap string.append) maybe->>= ;; maybe docstring
                 swap
                 dup
                 .categories-string "\nCategories:\n  " swap string.append
                 swap
                 proc-names-string "Names:\n  " swap string.append
                 4 list cat-maybes "\n" strings-join
                 string.append
                 ;; add horizontal rulers
                 "----------------------------------------------------------\n"
                 swap over 3 list strings-append))

             (: help [symbol? word] ->
                "Print help on the given word."
                (help-string println)))

(TEST
 > (t '('help help-string))
 (Ok (list "----------------------------------------------------------\nhelp: ([symbol? word] ->)\n\nPrint help on the given word.\n\nCategories:\n  development/help\nNames:\n  blabla\n  help\n----------------------------------------------------------\n")))


(====cc-category (control-flow)
                 "Control flow.")

(cc-def thenelse ([boolean? val]
                  [ilist-of-possibly-source? truebranch]
                  [ilist-of-possibly-source? falsebranch])
        "Evaluates `truebranch` if val is #t, `falsebranch` if it is
#f."
        (cc-interpreter.eval $cci (if val truebranch falsebranch)))

(cc-defguest 'if 'thenelse alias)
;; (3 roll THENELSE (drop eval) (swap drop eval)) would be an
;; alternative definition, as long as the interpreter supports the
;; special THENELSE syntax.

(cc-def when ([boolean? val]
              [ilist-of-possibly-source? action])
        (if val
            (cc-interpreter.eval $cci action)
            (Ok $cci)))

(cc-def unless ([boolean? val]
                [ilist-of-possibly-source? action])
        (if val
            (Ok $cci)
            (cc-interpreter.eval $cci action)))

(TEST
 > (t '(#t ('yes) when))
 (Ok (list 'yes))
 > (t '(#f ('yes) when))
 (Ok (list))
 > (t '(#t ('yes) unless))
 (Ok (list))
 > (t '(#f ('yes) unless))
 (Ok (list 'yes)))


;; XX can't implement that properly in copycat, right? Really need
;; lexicals? Also, this drops all of the stack when failing at any
;; point; maybe this is correct though.
(cc-def repeat ([ilist-of-possibly-source? prog]
                [fixnum-natural0? n])
        "Repeat prog n times."
        (let lp ((n n)
                 (cci $cci))
          (if (zero? n)
              (Ok cci)
              (>>= (cc-interpreter.eval cci prog)
                   (C lp (dec n) _)))))

(TEST
 > (t '(10 (1 +) 5 repeat))
 (Ok (list 15)))

(cc-def loop ([ilist-of-possibly-source? prog])
        "Repeat prog forever."
        (let lp ((cci $cci))
          (==> (cc-interpreter.fuel-dec* cci $word)
               (cc-interpreter.eval prog)
               lp)))


(cc-defguest (: unergonomic-while
                [ilist-of-possibly-source? condition]
                [ilist-of-possibly-source? prog]
                "Evaluate `condition`, which must return a boolean;
when the returned value is #t, evaluate `prog` then repeat.

NOTE: `condition` and `prog` are still on the stack when `condition`
and `prog` are evaluated, and `condition` must put exactly 1 value
onto the stack and `prog` exactly 0. That's why it's called
\"unergonomic\"."
                (over eval (dup eval unergonomic-while) (drop2) if)))

(TEST
 > (t '(7 (pick2 zero? not) (rot 1 - -rot) unergonomic-while))
 (Ok (list 0)))

