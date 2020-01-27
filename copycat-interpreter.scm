;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         (oo-util-lazy ilist-of)
         table
         Maybe
         maybe
         Result
         srfi-1-Maybe
         error ;; to enable .show on Gambit exceptions
         (cj-typed typed-lambda-expand)
         posix/interrupts
         cc-type
         (copycat-interpreter-util possibly-source?
                                   copycat:predicate-accepts-source?)
         cc-category
         copycat-error)

(export (class cc-interpreter)
        (macros copycat:try-Ok
                copycat:try
                cc-def
                cc-return
                cc-defhost
                cc-defhost/try
                cc-defguest)
        possibly-source?
        copycat-init-sigint!)

(include "lib/cj-standarddeclares.scm")



;; --- Symbol table --------------------------------------

;; table to store the values of words (which are ccproc?s)
(def-once cc-words
  (make-table))
;; XX: Forth has a *tree* of binding maps? Context dependent? What kind
;; of context? Here it's just one map.

;; Note: currently the cc-words table is used for every word
;; reference at runtime. This could be made more efficient by moving
;; the lookups to a parsing step (symbol creation, store as part of
;; symbol data structure). ("Linker step")

(def (cc-word-set! [symbol? name] [ccproc? proc]) -> void?
     (table-set! cc-words name proc))


;; --- Errors --------------------------------------------

;; See copycat-error.scm

(def (cc-unwrap [copycat-runtime-result? v])
     (if-Ok v it (raise it)))

;; XX ilist? is still used in many places
(def copycat-stack? (ilist-of any?))

;; copycat-runtime-result? see further down


(def (copycat:rest v offending-code [fixnum-natural0? need])
     (if (pair? v)
         (Ok (rest v))
         (Error 
          (if (null? v)
              (copycat-missing-arguments offending-code
                                         'copycat:rest
                                         need
                                         0)
              ;; XX or should this be a host language error?
              (copycat-type-error offending-code
                                  "list?"
                                  v)))))

;; --- Procedures -----------------------------------------

;; Procedure values

(defclass ((ccproc #f)
           [(maybe (possibly-source-of string?)) docstring]
           [cc-type? type]
           [(list-of cc-category?) categories]))

;; Defined in guest language:

(defclass (ccguestproc [(possibly-source-of ilist?) code])
  extends: ccproc
  
  (defmethod (cc-apply s cci word/loc)
    ;; -> copycat-runtime-result?  don't break TCO!
    
    ;; XX loc from word/loc ?
    (cc-interpreter.eval cci code)))

;; Primitives:

;; Wich is (also) part of a foreign function interface (?), hence the
;; naming:

(defclass (ccforeigncall [natural0? numargs]
                         [procedure? op])
  extends: ccproc

  (defmethod (cc-apply s [cc-interpreter? cci] word/loc)
    ;; -> copycat-runtime-result?  TCO
    (let* ((cci/stack (lambda (stack)
                        (cc-interpreter.stack-set cci stack)))
           (stack (lambda ()
                    (cc-interpreter.stack cci)))
           (err
            (lambda ()
              (Error (copycat-missing-arguments word/loc
                                                s
                                                numargs
                                                (length (stack)))))))
      (case numargs
        ((0) (op word/loc cci))
        ((1) (if-let-pair
              ((a r) (stack))
              (op word/loc (cci/stack r) a)
              (err)))
        ((2) (let (stack (stack))
               (if (length->= stack 2)
                   (op word/loc (cci/stack (cddr stack))
                       (cadr stack) (car stack))
                   (err))))
        (else
         (let (stack (stack))
           (if-Just ((it (Maybe-split-at-reverse stack numargs)))
                    (letv ((rargs stack) it)
                          (apply op word/loc (cci/stack stack) rargs))
                    (err))))))))


;; like Result:try but converts non-|copycat-error|s
(defmacro (copycat:try-Ok . body)
  "Captures Scheme exceptions from body and returns them as (comp
Error copycat-host-error); the result from body is wrapped with |Ok|.
Needs `$word` in its context."
  `(with-exception-catcher (C copycat:Error $word _)
                           (lambda () (Ok (begin ,@body)))))

;; Without wrapping the non-exception path with Ok
(defmacro (copycat:try . body)
  "Captures Scheme exceptions from body and returns them as (comp
Error copycat-host-error); body should return a Result itself.
Needs `$word` in its context."
  `(with-exception-catcher (C copycat:Error $word _)
                           (lambda () ,@body)))


(def (copycat:Error $word e)
     (Error
      (cond ((copycat-error? e) e)
            ;;((type-exception? e) ...)
            ;; ^ XX but have to change copycat-type-error to take
            ;;   multiple values.
            (else
             (copycat-host-error $word e)))))


(def (copycat:_type-check-error $word
                                expr-str
                                pred-str
                                val)
     (Error (copycat-type-error $word
                                ($ "($pred-str $expr-str)")
                                val)))

;; Macro so that it can unhygienically catch $s and $word. Evil?
(defmacro (copycat:type-check-error use-source-error?
                                    expr-str
                                    pred-str
                                    pred
                                    val)
  `(copycat:_type-check-error $word
                              ,expr-str
                              ,pred-str
                              ,val))

(defmacro (copycat-lambda args . body)
  ;; have to remove location information *before* entering
  ;; typed-lambda, hence wrap the typed-lambda with an untyped one:
  (let* ((args* (source-code args))
         (vars (map perhaps-typed.var args*)))
    `(lambda ,vars
       ,(fold-right (lambda (arg expr)
                      (if (in-monad
                           maybe
                           (>>= (perhaps-typed.maybe-predicate arg)
                                copycat:predicate-accepts-source?))
                          expr
                          (let (var (perhaps-typed.var arg))
                            `(let (,var (copycat-interpreter-util:desourcify
                                         ,var))
                               ;; ^ source-code is not enough in the
                               ;; case of list (or vector, etc.)
                               ;; literals. cj-desourcify can't be
                               ;; used as it breaks objects.
                               ,expr))))
                    `(,(typed-lambda-expand stx args body
                                            '##begin
                                            `copycat:type-check-error)
                      ,@vars)
                    args*))))


(def (cc-parse-body perhaps-docstring+body)
     -> (values-of (maybe (possibly-source-of string?))
                   ilist?)
     (let (perhaps-docstring+body (source-code perhaps-docstring+body))
       (if-let-pair ((a r) perhaps-docstring+body)
                    (if (string? (source-code a))
                        (values a r)
                        (values #f perhaps-docstring+body))
                    (values #f perhaps-docstring+body))))


(defmacro (cc-def name args/type . perhaps-docstring+body)
  "Implement the word `name` via Scheme code. `args/type` is a list
following the same syntax as Copycat type definitions, i.e. can
contain a `->` within the list.

`perhaps-docstring+body` can optionally start with a string, used as
docstring. `body` needs to return a copycat-runtime-result?;
|cc-return| does that and should be used for normal returns.

Binds the following symbols in the scope of `body`:

$word  the word from the source code being evaluated, usually
       same as name but usually including location information
$cci   cc-interpreter state (includes stack and fuel)
$s     the stack (out of $cci)
"
  (assert* symbol? name)
  (if-Ok (cc-parse-type args/type)
         (let (inputs (.inputs it))
           (letv ((maybe-docstring body)
                  (cc-parse-body perhaps-docstring+body))
                 `(cc-word-set! ',name
                                (ccforeigncall
                                 ,maybe-docstring
                                 ,(.show it)      ;; type
                                 ,(cc-category:current-categories-symbol)
                                 ,(length inputs) ;; numargs
                                 (copycat-lambda
                                  ,(cons* '[possibly-source? $word]
                                          '[possibly-source? $cci]
                                          inputs)
                                  ;; ^ HEH that |source-code| is
                                  ;; required. otherwise gambit has a
                                  ;; problem, 'Identifier expected'

                                  ;; XX `(-> (Result-of ,(.maybe-results
                                  ;; it)) ..), no, it's not the result of
                                  ;; the copycat-lambda, those are inside
                                  ;; cc-return. TODO: verify anyway, don't
                                  ;; want unchecked "docs".
                             
                                  (let ($s (cc-interpreter.stack $cci))
                                    (in-monad Result
                                              ,@body)))))))
         (source-error args/type "args/type parsing error"
                       ;; XX .show ?
                       it)))

(def (copycat-interpreter:cc-return-stack cci stack)
     (Ok (cc-interpreter.stack-set cci stack)))

(defmacro (cc-return . es)
  `(copycat-interpreter:cc-return-stack $cci (cons* ,@(reverse es) $s)))

(defmacro (cc-defhost name args/type #!optional docstring)
  ;; argh, have to cc-parse-type twice? COPY-PASTE.
  (if-Ok (cc-parse-type args/type)
         `(cc-def ,name ,args/type
                  ,@(if docstring (list docstring) (list))
                  (cc-return ,(cons name
                                    (map perhaps-typed.var
                                         (.inputs it)))))
         (source-error args/type "args/type parsing error"
                       ;; XX .show ?
                       it)))

(defmacro (cc-defhost/try name args/type #!optional docstring)
  ;; ditto
  (if-Ok (cc-parse-type args/type)
         `(cc-def ,name ,args/type
                  ,@(if docstring (list docstring) (list))
                  (>>= (copycat:try-Ok ,(cons name
                                              (map perhaps-typed.var
                                                   (.inputs it))))
                       (C cc-return _)))
         (source-error args/type "args/type parsing error"
                       ;; XX .show ?
                       it)))


(defparameter copycat-interpreter:current-categories
  (list (cc-category '(@runtime) #f)))

(def (cc-defguest-run expr categories)
     (=> (parameterize ((copycat-interpreter:current-categories categories))
           (cc-interpreter.eval (fresh-cc-interpreter) expr))
         cc-unwrap
         cc-interpreter.stack
         ((lambda (stack)
            (unless (null? stack)
              (source-error expr "program does not return an empty stack"
                            (cj-desourcify stack)))))))

(defmacro (cc-defguest . prog)
  "Evaluates prog with an empty stack, throwing an exception if the
result is an Error or if there are any values left"
  `(cc-defguest-run (quote-source ,prog)
                    ,(cc-category:current-categories-symbol)))


;; --- Interpreter -------------------------------------------

;; from signal handler (or other thread, just globally, though)
(def *copycat-interpreter:interrupt* #f)


(defclass (cc-interpreter [copycat-stack? stack]
                          [fixnum-natural0? fuel]
                          [fixnum-natural0? repl-level])

  (defmethod (repl-level-inc s)
    (.repl-level-update s inc-function))

  (defmethod (repl-level-dec s)
    (.repl-level-update s dec-function))

  (defmethod (fuel-dec* s offending-code) -> copycat-runtime-result?
    (if (zero? fuel)
        (Error (copycat-out-of-fuel offending-code))
        (Ok (cc-interpreter stack (dec fuel) repl-level))))

  ;; For the other ...* methods, could just `==>`-chain fuel-dec* but
  ;; already wrote those and "are more optimized".


  (defmethod (push s val)
    (cc-interpreter (cons val stack) fuel repl-level))

  ;; (defmethod (push-vals s vals)
  ;;   (cc-interpreter (append vals stack) fuel repl-level))

  (defmethod (push* s val offending-code)
    (if (zero? fuel)
        (Error (copycat-out-of-fuel offending-code))
        (Ok (cc-interpreter (cons val stack) (dec fuel) repl-level))))

  
  (defmethod (drop s offending-code) -> copycat-runtime-result?
    "free drop operation"
    (in-monad Result
              (>>= (copycat:rest stack offending-code 1)
                   (lambda (stack*)
                     (Ok (cc-interpreter stack* fuel repl-level))))))

  ;; (defmethod (drop* s offending-code) -> copycat-runtime-result?
  ;;   "drop operation that takes fuel"
  ;;   (in-monad Result
  ;;             (if (zero? fuel)
  ;;                 (Error (copycat-out-of-fuel offending-code))
  ;;                 (>>= (copycat:rest stack offending-code 1)
  ;;                      (lambda (stack*)
  ;;                        (Ok (cc-interpreter stack* (dec fuel) repl-level)))))))


  (defmethod (apply s [symbol? word] word/loc)
    ;; -> copycat-runtime-result?  don't break TCO!
    (if-Just ((w (table.Maybe-ref cc-words word)))
             (.cc-apply w s word/loc)
             (Error (copycat-unbound-symbol word/loc
                                            word))))

  (defmethod (eval s prog/loc)
    ;; -> copycat-runtime-result? ;; XX don't break TCO!
    (in-monad
     Result

     (if *copycat-interpreter:interrupt*
         (begin
           (set! *copycat-interpreter:interrupt* #f)
           (Error (copycat-interrupted prog/loc)))

         (let (prog (source-code prog/loc))
           (if (null? prog)
               (Ok s)

               (if-let-pair
                ((item/loc prog*) prog)
                (mlet
                 ((s (cc-interpreter.fuel-dec* s item/loc)))
                 ;; ^ XX danger, out of sync with field accesses
                 (let (item (source-code item/loc))
                   (cond 

                    ((symbol? item)
                     ;; check for special syntax (XX should this be
                     ;; made extensible at runtime by using special
                     ;; word values?)
                     (case item
                       ((:)
                        ;; Simple (non-delimited) variant of |:|; takes 2
                        ;; arguments from program (name, prog), not stack
                        (let (missargerr
                              (lambda (notpair)
                                (Error
                                 (copycat-missing-arguments
                                  ;; XX not the same kind of
                                  ;; missing; missing syntax, not
                                  ;; runtime arguments
                                  ':
                                  notpair ;; XX?
                                  2
                                  (length prog*)))))
                          (if-let-pair
                           ((name/loc r) prog*)
                           (if-let-pair
                            ((subprog cont) r)
                            (if (list? (source-code subprog))
                                (begin
                                  ;; XX could retain name/loc
                                  ;; XX allow docstring still?
                                  (cc-word-set! (source-code name/loc)
                                                (ccguestproc #f ;;
                                                             (cc-type-unknown #f)
                                                             (copycat-interpreter:current-categories)
                                                             subprog))
                                  (cc-interpreter.eval s cont))
                                (Error
                                 (copycat-type-error item/loc
                                                     "list?"
                                                     subprog)))
                            (missargerr r))
                           (missargerr prog*))))
                       ((THENELSE)
                        ;; takes 2 arguments from program (truebranch,
                        ;; falsebranch), and 1 from stack (test value)
                        (let ((cont (cddr prog*)))
                          (==> (cc-interpreter.drop s item/loc)
                               (cc-interpreter.eval 
                                (if (car stack)
                                    (car prog*)
                                    (cadr prog*)))
                               (cc-interpreter.eval cont))))
                       (else
                        ;; Need to check prog* to get TCO!
                        ;; (cc-interpreter.eval '()) is a NOP but that's
                        ;; too late for TCO in *Scheme*.
                        (let (app (lambda ()
                                    (cc-interpreter.apply s item item/loc)))
                          (if (null? prog*)
                              (app)
                              (==> (app)
                                   (cc-interpreter.eval prog*)))))))

                    (else
                     (let (cont-literal
                           (lambda ()
                             (=> (cc-interpreter.push s item/loc)
                                 (cc-interpreter.eval prog*))))
                       (if-let-pair
                        ((a r) item)

                        (case (source-code a)
                          ((quote)
                           (if (and (pair? r)
                                    (null? (cdr r)))
                               (=> (cc-interpreter.push s (car r))
                                   (cc-interpreter.eval prog*))
                               (cont-literal)))

                          ((:)
                           ;; More featureful, delimited variant of |:|;
                           ;; expects a program as the last item in the
                           ;; list, optionally docstring before that, name
                           ;; as the first, and the part inbetween as type

                           (let (err-missing
                                 (lambda (msg ngotten)
                                   (Error (copycat-missing-arguments
                                           item/loc
                                           msg ;; proc, XX evil?
                                           2 ngotten))))
                             (if-let-pair
                              ((name r) r)
                              (if-let-pair
                               ((prog rr) (reverse r))

                               (let (cont-ccguestproc
                                     (lambda (maybe-docstring type)
                                       (if (list? (source-code prog))
                                           (begin
                                             (cc-word-set!
                                              (source-code name) ;; XX loc ?
                                              (ccguestproc maybe-docstring
                                                           type
                                                           (copycat-interpreter:current-categories)
                                                           prog))
                                             ;; Actually don't return with
                                             ;; Ok, but continue *here*
                                             ;; (restructure by looping
                                             ;; around outside? no?):
                                             (cc-interpreter.eval s prog*))
                                           (Error
                                            (copycat-invalid-type prog
                                                                  "not a list")))))
                                 (if-let-pair
                                  ((?docstring rr*) rr)
                                  (if (string? (source-code ?docstring))
                                      (>>= (cc-parse-type (reverse rr*))
                                           (lambda (type)
                                             (cont-ccguestproc ?docstring
                                                               type)))
                                      (>>= (cc-parse-type (reverse rr))
                                           (lambda (type)
                                             (cont-ccguestproc #f
                                                               type))))
                                  (cont-ccguestproc #f
                                                    (cc-type-unknown #f))))
                               (err-missing "missing program argument" 1))
                              (err-missing "missing name argument" 0))))
                    
                          (else
                           ;; "quoted" program
                           (cont-literal)))

                        ;; literal ("presumably")
                        (cont-literal)))))))

                ;; improper list (stupid)
                (Error (copycat-type-error prog/loc "list?" prog)))))))))


(defparameter copycat-default-fuel 100000)

(def (fresh-cc-interpreter)
     (cc-interpreter '() (copycat-default-fuel) 0))

(def copycat-runtime-result?
     (Result-of cc-interpreter?
                copycat-error?))



(def (copycat-init-sigint!)
     (interrupt-install-handler!
      SIGINT
      (lambda ()
        (set! *copycat-interpreter:interrupt* #t))))
