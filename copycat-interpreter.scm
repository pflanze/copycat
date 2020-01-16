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
         (cj-typed typed-lambda-expand))

(export (class cc-interpreter)
        (macros copycat:try-Ok
                copycat:try
                cc-def
                cc-return
                cc-defhost
                cc-defhost/try
                cc-defguest)
        possibly-source?)


(def (Maybe-find-deeply pred v) -> (Maybe any?)
     (cond ((pred v)
            (Just v))
           ((pair? v)
            (let-pair ((a r) v)
                      (Maybe:or (Maybe-find-deeply pred a)
                                (Maybe-find-deeply pred r))))
           (else
            (Nothing))))

(TEST
 > (Maybe-find-deeply string? '(a b "c" d e "f" g))
 [(Just) "c"]
 > (Maybe-find-deeply boolean? '(a b "c" d e "f" g))
 [(Nothing)]
 > (Maybe-find-deeply boolean? '(a b "c" d e "f" (#f) g))
 [(Just) #f])

(def (contains-deeply? v pred) -> boolean?
     ;; (yeah, optimize automatically? Rust will?)
     (Just? (Maybe-find-deeply pred v)))

(TEST
 > (map (C contains-deeply? '(a (("x" #f) (((b . c))))) _)
        (list (C eq? _ 'a)
              (C eq? _ 'c)
              not
              (C eq? _ #t)))
 (#t #t #t #f))


(def (possibly-source? v)
     "same as `any?`, but expresses intent to accept source code"
     #t)

(def (copycat:predicate-accepts-source? expr)
     ;; allocates, stupid, but only used at compile time
     (contains-deeply? (cj-desourcify expr)
                       (lambda (v)
                         (case v
                           ((possibly-source-of
                             possibly-source?
                             source-of
                             source?)
                            #t)
                           (else
                            #f)))))

(TEST
 > (map copycat:predicate-accepts-source?
        '((source-of list?)
          list?
          (values-of (possibly-source-of symbol?) boolean?)
          (list-of possibly-source?)
          (list-of string?)))
 (#t #f #t #t #f))


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

;; We represent guest language errors as (Error-of
;; copycat-runtime-error?):

(defclass ((copycat-runtime-error #f) [possibly-source? offending-code])
  (defclass (copycat-out-of-fuel))
  (defclass (copycat-unbound-symbol [symbol? name])
    (defmethod (explanation s)
      "the given word is not defined"))
  (defclass (copycat-missing-arguments proc
                                       [fixnum-natural0? need]
                                       [fixnum-natural0? got])
    (defmethod (explanation s)
      ($ "the given operation needs more arguments than are "
         "available (either on the stack, or (in the case of "
         "special syntax) in the program)")))
  (defclass (copycat-division-by-zero a b) ;; why capture b ?
    (defmethod (explanation s)
      "attempt to divide by exactly 0"))
  (defclass (copycat-out-of-bounds-access asked
                                          [fixnum-natural0? length])
    (defmethod (explanation s)
      "attempt to access an index outside the range of valid indices"))
  
  (defclass (copycat-type-error [string? predicate] value)
    (defmethod (explanation s)
      "value encountered does not match given type declaration"))
  (defclass (copycat-host-error exception)
    (defmethod (explanation s)
      "an exception was thrown from code implemented in the host language"))
  (defclass (copycat-invalid-type [string? reason])
    (defmethod (explanation s)
      "the given type declaration is syntactically invalid")))


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

;; Type parsing

(definterface cc-type
  
  (defclass (cc-type-unknown maybe-location)
    (defmethod (inputs _) '())
    (defmethod (maybe-results _) #f)
    (defmethod (maybe-original _) #f)

    (defclass (cc-type/unknown-results [ilist? inputs])
      (defmethod (maybe-original _)
        (possibly-sourcify inputs maybe-location))

      (defclass (cc-type/results [ilist? results])
        (defmethod (maybe-results _) results)
        (defmethod (maybe-original _)
          (possibly-sourcify (append inputs
                                     (list (possibly-sourcify
                                            '-> maybe-location))
                                     results)
                             maybe-location))))))

(def (cc-parse-type l)
     -> (Result-of cc-type?
                   copycat-runtime-error?)
     (let ((l* (cj-desourcify l))
           (loc (maybe-source-location l)))
       (if (list? l*)
           (if (null? l*)
               (Ok (cc-type-unknown loc))
               (let (parts (list-split l* '->))
                 (case (length parts)
                   ((1) (Ok (cc-type/unknown-results loc
                                                     (first parts))))
                   ((2) (Ok (cc-type/results loc
                                             (first parts)
                                             (second parts))))
                   (else
                    ;; hmm curried functions?
                    (Error (copycat-invalid-type
                            l "at most one '-> is allowed"))))))
           (Error (copycat-invalid-type l "not a list")))))

(TEST
 > (cc-parse-type '())
 [(Ok) [(cc-type-unknown) #f]]
 > (cc-parse-type '(a b -> b))
 [(Ok) [(cc-type/results) #f (a b) (b)]]
 > (cc-parse-type '(-> any?))
 [(Ok) [(cc-type/results) #f () (any?)]]
 > (cc-parse-type '([list? l] -> any?))
 [(Ok) [(cc-type/results) #f ([list? l]) (any?)]]
 > (cc-parse-type 'foo)
 [(Error) [(copycat-invalid-type) foo "not a list"]]
 > (cc-parse-type '([list? l] -> any? -> foo?))
 [(Error)
  [(copycat-invalid-type)
   ([list? l] -> any? -> foo?)
   "at most one '-> is allowed"]])


;; Procedure values

(defclass ((ccproc #f)
           [(maybe (possibly-source-of string?)) docstring]
           [cc-type? type]))

;; Defined in guest language:

(defclass (ccguestproc [(possibly-source-of ilist?) code])
  extends: ccproc
  
  (defmethod (cc-apply s stack word/loc)
    (cc-eval stack code)))

;; Primitives:

;; Wich is (also) part of a foreign function interface (?), hence the
;; naming:

(defclass (ccforeigncall [natural0? numargs]
                         [procedure? op])
  extends: ccproc

  (defmethod (cc-apply s stack word/loc)
    (let (err
          (lambda ()
            (Error (copycat-missing-arguments word/loc
                                              s
                                              numargs
                                              (length stack)))))
      (case numargs
        ((0) (op word/loc stack))
        ((1) (if-let-pair
              ((a r) stack)
              (op word/loc r a)
              (err)))
        ((2) (if (length->= stack 2)
                 (op word/loc (cddr stack)
                     (cadr stack) (car stack))
                 (err)))
        (else
         (if-Just ((it (Maybe-split-at-reverse stack numargs)))
                  (letv ((rargs stack) it)
                        (apply op word/loc stack rargs))
                  (err)))))))


;; like Result:try but converts non-|copycat-runtime-error|s
(defmacro (copycat:try-Ok . body)
  `(with-exception-catcher (C copycat:Error $word _)
                           (lambda () (Ok (begin ,@body)))))

;; Without wrapping the non-exception path with Ok
(defmacro (copycat:try . body)
  `(with-exception-catcher (C copycat:Error $word _)
                           (lambda () ,@body)))


(def (copycat:Error $word e)
     (Error
      (cond ((copycat-runtime-error? e) e)
            ;;((type-exception? e) ...)
            ;; ^ XX but have to change copycat-type-error to take
            ;;   multiple values.
            (else
             (copycat-host-error $word e)))))


(def (copycat:_type-check-error $s $word
                                use-source-error?
                                expr-str
                                pred-str
                                pred
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
  `(copycat:_type-check-error $s $word
                              ,use-source-error?
                              ,expr-str
                              ,pred-str
                              ,pred
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
                            `(let (,var (source-code ,var))
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

;; setting a word to a Scheme program (not translating Scheme
;; exceptions). body may start with a docstring.
(defmacro (cc-def name args/type . perhaps-docstring+body)
  (assert* symbol? name)
  (if-Ok (cc-parse-type args/type)
         (let (inputs (.inputs it))
           (letv ((maybe-docstring body)
                  (cc-parse-body perhaps-docstring+body))
                 `(cc-word-set! ',name
                                (ccforeigncall
                                 ,maybe-docstring
                                 ,(.show it) ;; type
                                 ,(length inputs) ;; numargs
                                 (copycat-lambda
                                  ,(cons* '[possibly-source? $word]
                                          '[possibly-source? $s]
                                          inputs)
                                  ;; ^ HEH that |source-code| is
                                  ;; required. otherwise gambit has a
                                  ;; problem, 'Identifier expected'

                                  ;; XX `(-> (Result-of ,(.maybe-results
                                  ;; it)) ..), no, it's not the result of
                                  ;; the copycat-lambda, those are inside
                                  ;; cc-return. TODO: verify anyway, don't
                                  ;; want unchecked "docs".
                             
                                  (in-monad Result
                                            ,@body))))))
         (source-error args/type "args/type parsing error"
                       ;; XX .show ?
                       it)))

(defmacro (cc-return . es)
  `(Ok (cons* ,@(reverse es) $s)))

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


(def (cc-unwrap [copycat-runtime-result? v])
     (if-Ok v it (raise it)))

(def (cc-defguest-run expr)
     (assert (null? (cc-unwrap (cc-interpreter.eval (fresh-cc-interpreter)
                                                    expr)))))

(defmacro (cc-defguest . prog)
  "cc-eval's prog with an empty stack, throwing an exception if the
result is an Error or if there are any values left"
  `(cc-defguest-run (quote-source ,prog)))


;; --- Interpreter -------------------------------------------

(defclass (cc-interpreter [copycat-stack? stack]
                          [fixnum-natural0? fuel])

  (defmethod (drop s offending-code) -> (Result-of cc-interpreter?)
    "free drop operation"
    (==> (copycat:rest stack offending-code 1)
         (cc-interpreter fuel)))

  (defmethod (drop* s offending-code) -> (Result-of cc-interpreter?)
    "drop operation that takes fuel"
    (if (zero? fuel)
        (Error (copycat-out-of-fuel offending-code))
        (==> (copycat:rest stack offending-code 1)
             (cc-interpreter (dec fuel)))))


  (defmethod (apply s [symbol? word] word/loc)
    -> copycat-runtime-result?
    (if-Just ((w (table.Maybe-ref cc-words word)))
             (.cc-apply w stack word/loc)
             (Error (copycat-unbound-symbol word/loc
                                            word))))

  (defmethod (eval s prog/loc)
    ;; -> copycat-runtime-result? ;; XX don't break TCO!
    (in-monad
     Result

     (let (prog (source-code prog/loc))
       (if (null? prog)
           (Ok s)

           (let-pair
            ((item/loc prog*) prog)
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
                     (==> (cc-interpreter.drop s)
                          (cc-interpreter.eval 
                           (if (car stack)
                               (car prog*)
                               (cadr prog*)))
                          (cc-interpreter.eval cont))))
                  ((QUOTE)
                   ;; takes 1 argument from program, puts it on
                   ;; the stack
                   (let ((cont (cdr prog*)))
                     (cc-interpreter.eval (cons (car prog*) stack) cont)))
                  (else
                   (let ((app (thunk (cc-apply stack item item/loc))))
                     (if (null? prog*)
                         (app)
                         (>>= (app)
                              (C cc-interpreter.eval _ prog*)))))))

               (else
                (let (cont-literal
                      (lambda ()
                        (cc-interpreter.eval (cons item/loc stack) prog*)))
                  (if-let-pair
                   ((a r) item)

                   (case (source-code a)
                     ((quote)
                      (if (and (pair? r)
                               (null? (cdr r)))
                          (cc-interpreter.eval (cons (car r) stack) prog*)
                          (cont-literal)))

                     ((:)
                      ;; More featureful, delimited variant of |:|;
                      ;; expects a program as the last item in the
                      ;; list, optionally docstring before that, name
                      ;; as the first, and the part inbetween as type

                      (let (err-missing
                            (lambda (msg mgotten)
                              (Error (copycat-missing-arguments
                                      item/loc
                                      msg ;; proc, XX evil?
                                      2 ngotten))))
                        (if-let-pair
                         ((name r) r)
                         (if-let-pair
                          ((prog rr) (reverse r))

                          (let (cont-ccguestproc
                                (lambda (?docstring type)
                                  (if (list? (source-code prog))
                                      (begin
                                        (cc-word-set! (source-code name)
                                                      ;; XX loc ?
                                                      (ccguestproc ?docstring
                                                                   type
                                                                   prog))
                                        ;; Actually don't return with
                                        ;; Ok, but continue *here*
                                        ;; (restructure by looping
                                        ;; around outside? no?):
                                        (cc-interpreter.eval stack prog*))
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
                   (cont-literal))))))))))))


(defparameter copycat-default-fuel 1000)

(def (fresh-cc-interpreter)
     (cc-interpreter '() (copycat-default-fuel)))

(def copycat-runtime-result?
     (Result-of cc-interpreter?
                copycat-runtime-error?))
