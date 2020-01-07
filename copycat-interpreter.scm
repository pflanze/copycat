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
         (cj-typed typed-lambda-expand)
         )

(export cc-eval
        cc-repl
        (macros cc-def
                cc-return
                cc-defhost
                cc-defhost/try))


;; --- Symbol table --------------------------------------

;; table to store the values of words
(def cc-words
     (make-table))
;; XX: Forth has a *tree* of binding maps? Context dependent? What kind
;; of context? Here it's just one map.

;; Note: currently the cc-words table is used for every word
;; reference at runtime. This could be made more efficient by moving
;; the lookups to a parsing step (symbol creation, store as part of
;; symbol data structure). ("Linker step")

(def (cc-word-set! [symbol? name] val) -> void?
     (table-set! cc-words name val))


;; --- Errors --------------------------------------------

;; We represent guest language errors as (Error-of
;; copycat-runtime-error?):

;; XX Really bundle the stack with the error, or keep those separate?
(defclass (copycat-runtime-error [copycat-stack? stack])
  (defclass (copycat-runtime-error/symbol
             ;; Context, i.e. the word reporting the error, or the
             ;; word that wasn't found.
             [symbol? symbol])
    (defclass (copycat-unbound-symbol))
    (defclass (copycat-missing-arguments proc))
    (defclass (copycat-division-by-zero a b))
    (defclass (copycat-type-error predicate value))
    (defclass (copycat-host-error exception))))

(def copycat-stack? (ilist-of any?))
(def copycat-runtime-result?
     (Result-of copycat-stack?
                copycat-runtime-error?))


;; --- Primitives -------------------------------------------

;; Wich is (also) part of a foreign function interface (?), hence the
;; naming:

(defclass (ccforeigncall [natural0? numargs]
                         [procedure? op]))


;; like Result:try but converts non-|copycat-runtime-error|s
(defmacro (copycat:try/name name . body)
  `(with-exception-catcher (C copycat:Error $s ',name _)
                           (lambda () (Ok (begin ,@body)))))

(def (copycat:Error $s name e)
     (Error
      (cond ((copycat-runtime-error? e) e)
            ;;((type-exception? e) ...)
            ;; ^ XX but have to change copycat-type-error to take multiple values.
            (else
             (copycat-host-error $s name e)))))


(def (copycat:_type-check-error $s $word
                                use-source-error?
                                expr-str
                                pred-str
                                pred
                                val)
     (Error (copycat-type-error $s
                                $word
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

;; setting a word to a Scheme program (not translating Scheme
;; exceptions)
(defmacro (cc-def name args . body)
  (assert* symbol? name
           (lambda_
            `(cc-word-set! ',name
                           (ccforeigncall
                            ,(length (source-code args))
                            (let (($word ',name))
                              (copycat-lambda
                               ,(cons '$s (source-code args))
                               ;; ^ HEH that |source-code| is
                               ;; required. otherwise gambit has a
                               ;; problem, 'Identifier expected'
                               (in-monad Result
                                         ,@body))))))))

(defmacro (cc-return . es)
  `(Ok (cons* ,@(reverse es) $s)))

(defmacro (cc-defhost name args)
  `(cc-def ,name ,args
           (cc-return ,(cons name
                             (map perhaps-typed.var (source-code args))))))

(defmacro (cc-defhost/try name args)
  `(cc-def ,name ,args
           (>>= (copycat:try/name ,name
                                  ,(cons name (source-code args)))
                (C cc-return _))))


;; --- Interpreter -------------------------------------------

(def (cc-apply [copycat-stack? stack] [symbol? word])
     -> copycat-runtime-result?
     (if-Just ((w (table.Maybe-ref cc-words word)))
              (let (err
                    (lambda ()
                      (Error (copycat-missing-arguments stack
                                                        word
                                                        w))))
                (if (ccforeigncall? w)
                    (let-ccforeigncall
                     ((numargs op) w)
                     (case numargs
                       ((0) (op stack))
                       ((1) (if-let-pair
                             ((a r) stack)
                             (op r a)
                             (err)))
                       ((2) (if (length->= stack 2)
                                (op (cddr stack) (cadr stack) (car stack))
                                (err)))
                       (else
                        (if-Just ((it (Maybe-split-at-reverse stack numargs)))
                                 (letv ((rargs stack) it)
                                       (apply op stack rargs))
                                 (err)))))
                    (cc-eval stack w)))
              (Error (copycat-unbound-symbol stack word))))

(def (cc-eval stack prog) ;; -> copycat-runtime-result? ;; XX don't break TCO!
     (in-monad
      Result
      (if (null? prog)
          (Ok stack)
          (let-pair ((item prog*) prog)
                    (cond 
                     ((symbol? item)
                      ;; check for special syntax (XX should this be
                      ;; made extensible at runtime by using special
                      ;; word values?)
                      (case item
                        ((:)
                         ;; takes 2 arguments from program (name,
                         ;; prog), not stack
                         (let (err (lambda (notpair)
                                     (Error
                                      (copycat-missing-arguments
                                       stack ':
                                       ;; XX again, why "proc" argument?
                                       notpair))))
                           (if-let-pair
                            ((name r) prog*)
                            (if-let-pair
                             ((subprog cont) r)
                             (if (list? subprog)
                                 (begin (cc-word-set! name subprog)
                                        (cc-eval stack cont))
                                 (Error
                                  (copycat-type-error stack
                                                      ':
                                                      'list?
                                                      subprog)))
                             (err r))
                            (err prog*))))
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
                         (let ((app (thunk (cc-apply stack item))))
                           (if (null? prog*)
                               (app)
                               (>>= (app)
                                    (C cc-eval _ prog*)))))))
                     (else
                      (cc-eval (cons item stack) prog*)))))))


