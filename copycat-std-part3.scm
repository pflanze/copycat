;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         (cj-io-util putfile getfile)
         (string-quote shell-quote)
         (string-util-2 string-starts-with?)
         cj-path
         cj-source
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


(====cc-category (source)
                 "Source code representation (values and associated
location information).")

(cc-defhost source? ([possibly-source? s] -> boolean?)
            "Whether `s` is a value wrapped with location information.")

(cc-defhost source-code ([possibly-source? s] -> any?)
            "Strips location information from source (i.e. return the
code embedded in a source object); if s is not a source object, return
s (i.e. never fails).")

(cc-defhost maybe-source-location ([possibly-source? s] -> (maybe location?))
            "Strips location information from source (i.e. return the
code embedded in a source object); if s is not a source object, return
s (i.e. never fails).")
;; XX also add source-location but named source-maybe-location ?


(====cc-category (source location)
                 "Source code location information.")

(cc-defhost/try .maybe-location ([possibly-source? s] -> (maybe location?))
                "Try to call the maybe-location method on `s`, which
should return `s`'s location in the source code if available.")


(def location.container location-container)
;;(def location.position location-position)

(def (location.line loc) -> fixnum-natural?
     (=> (location-position loc)
         position-line))

(def (location.maybe-column loc) -> (maybe fixnum-natural?)
     (=> (location-position loc)
         position-maybe-column))

(cc-defhost location.container ([location? loc] -> (either path-string?
                                                           pair?))
            "Extract the container part of the location `loc`,
which is either the path to a file, or an alternative location
indication (like |string|, |console|) wrapped in a list.")

(cc-defhost location.line ([location? loc] -> fixnum-natural?)
            "Extract the line part of the location `loc`")
(cc-defhost location.maybe-column ([location? loc] -> (maybe fixnum-natural?))
            "Extract the column part of the location `loc`")

;; container->path ?


(def print-location show-location-location)

(cc-defhost/try print-location ([(maybe location?) loc] ->)
                "Print an also human-readable line that tells Emacs to
jump to the location represented by `loc`.")

(TEST
 > (=> (with-output-to-string
         (& (t '('help ref .maybe-location print-location))))
       fst
       (string-contains? "std-part3.scm\"@"))
 #t)


(====cc-category (I/O)
                 "Input and output.")


(cc-def print (v ->)
        ;; XX this hackery only needed due to -> not used for returns
        ;; yet
        (mdo (copycat:try-Ok (print v))
             (cc-return)))

(cc-def newline (->)
        (mdo (copycat:try-Ok (newline))
             (cc-return)))

(cc-def println (v ->)
        (mdo (copycat:try-Ok (println v))
             (cc-return)))

(def. (port.print p val)
  (print port: p val))

(cc-def port.print ([port? p] v ->)
        (mdo (copycat:try-Ok (port.print p v))
             (cc-return)))

(def. port.newline newline)
(cc-def port.newline ([port? p] ->)
        (mdo (copycat:try-Ok (port.newline p))
             (cc-return)))

(def. (port.println p val)
  (println port: p val))

(cc-def port.println ([port? p] v ->)
        (mdo (copycat:try-Ok (port.println p v))
             (cc-return)))
;; or:
;; (cc-defguest (: port.println [port? p] v ->
;;                 (over swap port.print port.newline)))

(def. port.flush force-output)
(cc-def port.flush ([port? p] ->)
        "Force output that was written to p but still buffered to be
sent to the OS."
        (mdo (copycat:try-Ok (port.flush p))
             (cc-return)))


(cc-defhost current-input-port (-> input-port?)
            "Returns the current input filehandle (on startup: stdin).")
(cc-defhost current-output-port (-> output-port?)
            "Returns the current output filehandle (on startup: stdout).")
(cc-defhost current-error-port (-> output-port?)
            "Returns the current error filehandle (on startup: stderr).")

(cc-defhost maybe-read-line ([input-port? port] -> string?)
            "Read a line from the given file handle, #f on EOF (ctl-d).")

(cc-def string.port ([string? s] -> input-port?)
        "Open the given string as a filehandle."
        (cc-return (call-with-input-string s identity)))


(====cc-category (I/O paths)
                 "Paths are strings with a few restrictions: they
can't be the empty string, and they can't contain the null character,
\"\\0\".")

;; XX lib
(def. path-string.add path-append)

(cc-defhost/try path-string.add ([path-string? base] [path-string? add]
                                 -> path-string?)
                "Build the path that results from following add when
the current-directory were base.")

(cc-defhost/try .add (a b -> a*)
                "Call the add method on a with b as additional
argument. The result is expected to be of the same type as the first
argument.")

;; XX lib
(def. path-string.absolute? path-absolute?)

(def. (path-string.relative? path)
  "Whether `path` does not start with a '/'. (TODO: portable
solution?)"
  (not (path-string.absolute? path)))


(cc-defhost path-string.absolute? ([path-string? path] -> boolean?)
            "Whether `path` starts with a '/'. (TODO: portable
solution?)")

(cc-defhost path-string.relative? ([path-string? path] -> boolean?)
            "Whether `path` does not start with a '/'. (TODO: portable
solution?)")


(====cc-category (I/O processes)
                 "Running processes.")

(cc-defhost exit ([uint8? code])
            "Exit the process running the Copycat interpreter with the
given exit code.")



(def (open-receiver-command [path-string? cmdpath] [(list-of string?) args])
     -> output-port? ;; odd, actually an #<input-output-port #9 (process "true")>
     "Simplified variant of open-output-process"
     (open-process (list path: cmdpath
                         arguments: args
                         stdin-redirection: #t
                         stdout-redirection: #f
                         stderr-redirection: #f)))

(cc-defhost/try open-receiver-command ([path-string? cmdpath]
                                       [(list-of string?) args]
                                       -> output-port?)
                "Start the given command/args and return an
output-port connected to it. When done, run `port.close`, and
`port.process-status` to get the exit status.")

(def. port.close close-port)

(cc-def port.close ([port? p] ->)
        "Close the given port (both input and output directions)."
        (mdo (copycat:try-Ok (port.close p))
             (cc-return)))

(def. port.process-status process-status)

(cc-defhost/try port.process-status ([port? p] -> uint16?)
                "Wait for the process on the other side of port `p` to
exit then return the exit status.")
;; (XX filehandles versus  ? (IRC discussion.))


(def run-shell shell-command) ;; for consistency

(cc-defhost/try run-shell ([string? code] -> uint16?)
                "Run `code` in a system shell (sh), and return the
exit code.")

(cc-defhost/try run-bash ([string? code] -> uint16?)
                "Run `code` in a bash shell, and return the exit
code.")

(cc-defguest (: xrun-bash [string? code] ->
                "Run `code` in a bash shell; raise an exception if
bash exits with a non-zero exit code."
                (run-bash dup zero?
                          (drop)
                          (1 list "bash exited with non-zero exit code"
                             swap error)
                          if)))

(cc-defhost/try xbacktick-bash ([string? code] -> string?)
                "Run `code` in a bash shell, and return the captured
output. If bash exited with a non-zero exit code, throw an
exception.")

(cc-def backtick-bash ([string? code] -> string? uint16?)
        "Run `code` in a bash shell, and return the captured
output as well as the exit code."
        (letv ((str res) (backtick-bash code))
              (cc-return str res)))

(TEST
 > (t '("true && true" run-shell))
 (Ok (list 0))
 > (t '("false" run-shell))
 (Ok (list 256))
 > (t '("true && true" run-bash))
 (Ok (list 0))
 > (t '("true && false" run-bash))
 (Ok (list 256))
 > (t '("echo Hi" xbacktick-bash))
 (Ok (list "Hi"))
 > (=> (t* '("false" xbacktick-bash)) Error.value copycat-host-error?)
 #t
 > (t '("echo Hi" backtick-bash))
 (Ok (list 0 "Hi"))
 > (t '("false" backtick-bash))
 (Ok (list 256 ""))
 > (t '("true" xrun-bash))
 (Ok (list))
 > (=> (t* '("false" xrun-bash)) Error.value copycat-generic-error.args)
 (256))


(cc-defhost/try putfile (bag [path-string? path] ->)
                "Write the contents of `bag`, which is a string or
list (of lists of) strings, to the file at `path`.")

(cc-defhost/try getfile ([path-string? path] -> string?)
                "Get the contents of the file at `path`.")

(cc-defhost shell-quote ([string? s] -> string?)
            "Turn s into a string constant in Bash syntax.")

(cc-def sleep ([nonnegative-real? seconds] ->)
        (thread-sleep! seconds)
        (cc-return))

(cc-def current-unixtime (-> inexact-real?)
        "The current time in unix time, with sub-second resolution."
        (cc-return (time->seconds (current-time))))

(====cc-category (I/O s-expressions)
                 "Reading and writing s-expressions.")

(cc-defhost read ([input-port? port] -> a)
            "Read one s-expression from the given filehandle.")

(cc-defhost read-all ([input-port? port] -> ilist?)
            "Read all s-expressions from the given filehandle.")

(TEST
 > (t '("hello 3 \"there\"" string.port read))
 (Ok (list 'hello))
 > (t '("hello 3 \"there\"" string.port read-all))
 (Ok (list (list 'hello 3 "there"))))


(cc-def path-string.read-source ([path-string? path] -> ilist?)
        "Read the contents of the file at path as a list of
s-expressions, enriched with location information."
        (>>= (copycat:try-Ok
              (call-with-input-file path read-all-source))
             (C cc-return _)))

(cc-defguest (: load [path-string? path]
                "Read and evaluate the given file."
                (path-string.read-source eval)))

;; This can't be guest code because I want to use the $word to get the
;; base location from, not the path argument. (Except could add syntax
;; (a macro!) to extract the location info onto the stack before the
;; call.)
(cc-def include ([path-string? path] ->)
        "Load the file at `path` resolved from the point of the call
to `include`, not the current-directory."
        (>>= (if (path-string.relative? path)
                 (if-let* ((loc (maybe-source-location $word))
                           (cont (location.container loc)))
                          (if (path-string? cont)
                              (Ok (path-string.add (dirname cont) path))
                              (Error (copycat-generic-error ;; ?
                                      $word
                                      "can't resolve relative path from source not coming from a file"
                                      (list cont))))
                          (Error (copycat-generic-error ;; better?
                                  $word
                                  "can't resolve relative path without source location information on the source code"
                                  '())))
                 (Ok path))
             (lambda (path)
               (>>= (copycat:try-Ok
                     (call-with-input-file path read-all-source))
                    (lambda (prog)
                      (cc-interpreter.eval $cci prog))))))

(cc-def write (v ->)
        (mdo (copycat:try-Ok (write v))
             (cc-return)))

(cc-def show (v ->)
        "Print the given value to the current-output-port as a Scheme
program that reconstructs v when evaluated."
        (mdo (copycat:try-Ok (pretty-print (try-show v)))
             (cc-return)))

(cc-defhost pretty-print (s ->)
            "Pretty-print s as a Scheme s-expression")

(cc-defhost pretty-string (s -> string?)
            "`pretty-print` `s` to a string.")


(====cc-category (I/O directories)
                 "Handling directories.")

(cc-defhost current-directory (-> string?)
            "The path to the current directory.")

(cc-def set-current-directory ([string? path] ->)
        "Set the path to the current directory."
        (>> (copycat:try-Ok (current-directory path))
            (cc-return)))

(cc-def directory-items ([string? path] -> (ilist-of string?))
        "The list of entries in the given directory (only the file
names without the parent directory path)."
        (>>= (copycat:try-Ok (stream->list (directory-item-stream path)))
            (C cc-return _)))

(cc-defguest 'cd 'set-current-directory alias
             'pwd 'current-directory alias
             (: ls -> (ilist-of string?)
                "List the files in the current directory."
                (current-directory directory-items))
             (: quit ->
                "Quit Copycat (currently also directly exits the process
running the interpreter, not just the interpreter)."
                ;; XX use the new repl stop thing instead?
                (0 exit)))


(====cc-category (development)
                 "Development aids.")

(====cc-category (development debugging)
                 "Debugging aids.")

(cc-def D ()
        "Enter a (nested) Copycat repl with the current stack /
machine state."
        ;; XX HACK: defined in copycat.scm which currently
        ;; depends on us
        (Ok (=> (.repl-level-inc $cci)
                cc-repl*
                .repl-level-dec)))

(cc-def DScheme ()
        "Print stack, enter a Scheme repl; enter ,(c (Ok $s)) to continue!"
        (mdo (copycat:try-Ok (pretty-print $s))
             (##repl)))

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

