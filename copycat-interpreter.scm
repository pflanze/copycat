;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         (oo-util-lazy ilist-of)
         table
         Maybe
         Result
         srfi-1-Maybe
         error ;; for built-in exception .show
         (cj-typed typed-lambda-expand))

(export cc-eval
        (macros copycat:try-Ok
                copycat:try
                cc-def
                cc-return
                cc-defhost
                cc-defhost/try))


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

(defclass (copycat-runtime-error offending-code) ;; mb with location 
  (defclass (copycat-unbound-symbol [symbol? name]))
  (defclass (copycat-missing-arguments proc
                                       [fixnum-natural0? need]
                                       [fixnum-natural0? got]))
  (defclass (copycat-division-by-zero a b))
  (defclass (copycat-type-error [string? predicate] value))
  (defclass (copycat-host-error exception))
  (defclass (copycat-invalid-type [string? reason])))

(def copycat-stack? (ilist-of any?))
(def copycat-runtime-result?
     (Result-of copycat-stack?
                copycat-runtime-error?))


;; --- Procedures -----------------------------------------

;; Type parsing

(definterface cc-type
  (defclass (cc-type-unknown)
    (defmethod (inputs _) '())
    (defmethod (maybe-results _) #f)
    (defclass (cc-type/unknown-results [ilist? inputs])
      (defclass (cc-type/results [ilist? results])
        (defmethod (maybe-results _) results)))))

(def (cc-parse-type l)
     -> (Result-of cc-type?
                   copycat-runtime-error?)
     (let (l* (cj-desourcify l))
       (if (list? l*)
           (if (null? l*)
               (Ok (cc-type-unknown))
               (let (parts (list-split l* '->))
                 (case (length parts)
                   ((1) (Ok (cc-type/unknown-results
                             (first parts))))
                   ((2) (Ok (cc-type/results
                             (first parts)
                             (second parts))))
                   (else
                    ;; hmm curried functions?
                    (Error (copycat-invalid-type
                            l "at most one '-> is allowed"))))))
           (Error (copycat-invalid-type l "not a list")))))

(TEST
 > (cc-parse-type '())
 [(Ok) [(cc-type-unknown)]]
 > (cc-parse-type '(a b -> b))
 [(Ok) [(cc-type/results) (a b) (b)]]
 > (cc-parse-type '(-> any?))
 [(Ok) [(cc-type/results) () (any?)]]
 > (cc-parse-type '([list? l] -> any?))
 [(Ok) [(cc-type/results) ([list? l]) (any?)]]
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
            ;; ^ XX but have to change copycat-type-error to take multiple values.
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
  (typed-lambda-expand stx args body '##begin
                       `copycat:type-check-error))


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
                                  ,(cons* '$word '$s inputs)
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

(defmacro (cc-defhost name args/type)
  ;; argh, have to cc-parse-type twice? COPY-PASTE.
  (if-Ok (cc-parse-type args/type)
         `(cc-def ,name ,args/type
                  (cc-return ,(cons name
                                    (map perhaps-typed.var
                                         (.inputs it)))))
         (source-error args/type "args/type parsing error"
                       ;; XX .show ?
                       it)))

(defmacro (cc-defhost/try name args/type)
  ;; ditto
  (if-Ok (cc-parse-type args/type)
         `(cc-def ,name ,args/type
                  (>>= (copycat:try-Ok ,(cons name
                                              (map perhaps-typed.var
                                                   (.inputs it))))
                       (C cc-return _)))
         (source-error args/type "args/type parsing error"
                       ;; XX .show ?
                       it)))


;; --- Interpreter -------------------------------------------

(def (cc-apply [copycat-stack? stack] [symbol? word] word/loc)
     -> copycat-runtime-result?
     (if-Just ((w (table.Maybe-ref cc-words word)))
              (.cc-apply w stack word/loc)
              (Error (copycat-unbound-symbol word/loc
                                             word))))

(def (copycat-quoted? v)
     ;; which happens to be the same way Scheme quotes
     (if-let-pair ((a r) (source-code v))
                  (and (eq? (source-code a) 'quote)
                       (if-let-pair ((b r) r)
                                    (null? r)
                                    #f))
                  #f))

(def (cc-eval stack prog/loc) ;; -> copycat-runtime-result? ;; XX don't break TCO!
     (in-monad
      Result

      (let (prog (source-code prog/loc))
        (if (null? prog)
            (Ok stack)

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
                                                         (cc-type-unknown)
                                                         subprog))
                              (cc-eval stack cont))
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
                    (let ((app (thunk (cc-apply stack item item/loc))))
                      (if (null? prog*)
                          (app)
                          (>>= (app)
                               (C cc-eval _ prog*)))))))

                (else
                 (let (cont-literal
                       (lambda ()
                         (cc-eval (cons item stack) prog*)))
                   (if-let-pair
                    ((a r) item)

                    (case (source-code a)
                      ((quote)
                       (if (and (pair? r)
                                (null? (cdr r)))
                           ;; XX could retain location info on constants. But
                           ;; will want full context information anyway?
                           (cc-eval (cons (source-code (car r)) stack) prog*)
                           (cont-literal)))

                      ((:)
                       ;; More featureful, delimited variant of |:|;
                       ;; expects a program as the last item in the
                       ;; list, optionally docstring before that, name
                       ;; as the first, and the part inbetween as type

                       (if-let-pair
                        ((name r) r)
                        (if-let-pair
                         ((prog rr) (reverse r))

                         (let (cont-ccguestproc
                               (lambda (?docstring type)
                                 (cc-word-set! (source-code name) ;; XX loc ?
                                               (ccguestproc ?docstring
                                                            type
                                                            prog))
                                 ;; Actually don't return with Ok, but
                                 ;; continue *here* (restructure by looping
                                 ;; around outside? no?):
                                 (cc-eval stack prog*)))
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
                                              (cc-type-unknown))))
                         (Error (copycat-missing-arguments
                                 item/loc
                                 "missing program argument" ;; proc, XX evil?
                                 2 1)))
                        (Error (copycat-missing-arguments
                                item/loc
                                "missing name argument" ;; ditto
                                2 0))))
                    
                      (else
                       ;; "quoted" program
                       (cont-literal)))

                    ;; literal ("presumably")
                    (cont-literal)))))))))))


