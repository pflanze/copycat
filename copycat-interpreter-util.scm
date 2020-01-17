;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         test)

(export possibly-source?
        copycat:predicate-accepts-source?)


(include "lib/cj-standarddeclares.scm")


;; XX move to lib?

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

