;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export (macro ====cc-category)
        cc-category-lookup
        cc-category-list
        cc-category-paths)

(include "lib/cj-standarddeclares.scm")


(def (group-with-optional a? optional-b?
                          construct/a construct/a+b
                          err
                          items)
     (let lp ((r '())
              (items items))
       (if-let-pair ((a items*) items)
                    (if (a? a)
                        (if-let-pair ((b items**) items*)
                                     (if (optional-b? b)
                                         (lp (cons (construct/a+b a b) r)
                                             items**)
                                         (lp (cons (construct/a a) r)
                                             items*))
                                     (cons (construct/a a) r))
                        (err a))
                    r)))

(TEST
 > (def (t l) (group-with-optional
               ilist? string? (C list 'R _) (C list 'S _ _) (C list 'ERR _) l))
 > (t '("a" b))
 (ERR "a")
 > (t '(("a") b))
 (ERR b)
 > (t '(("a") "b"))
 ((S ("a") "b"))
 > (t '(("a") "b" "c"))
 (ERR "c")
 > (t '(("a") "b" ("c")))
 ((R ("c")) (S ("a") "b"))
 > (t '(("a") "b" ("c") ()))
 ((R ()) (R ("c")) (S ("a") "b")))





(defclass (cc-category [(list-of symbol?) path]
                       [(maybe string?) maybe-docstring])
  (defmethod (show s)
    ;; quote path better; should finally fix list.show / general show
    `(cc-category ',path ,maybe-docstring)))


(defparameter cc-category:current-categories-symbol #f)


(defmacro (====cc-category . categorypaths/docstrings)
  "set the path to the point in the category hierarchy that should be
recorded with subsequent definitions (via cc-def, cc-defguest,
cc-defhost*). Each path can be followed by a docstring for it."

  (let (scs
        (group-with-optional ilist?
                             string?
                             (C cc-category _ #f)
                             cc-category
                             (C source-error _ "")
                             (cj-desourcify categorypaths/docstrings)))
    (with-gensym
     CATEGORYPATHS
     (cc-category:current-categories-symbol CATEGORYPATHS)
     `(begin
        (def ,CATEGORYPATHS
             (list ,@(.map scs .show)))
        (cc-category:register! ,CATEGORYPATHS)))))


(def cc-category:register
     ;; path -> cc-category
     (make-table))

(def (cc-category:register! categories)
     (.for-each
      categories
      (lambda (c)
        (let (path (cc-category.path c))
          (let (reg! (lambda ()
                       (table-set! cc-category:register path c)))
            (cond ((table-ref cc-category:register path #f)
                   => (lambda (creg)
                        ((on cc-category.maybe-docstring
                             (lambda (sreg sc)
                               (if (and sreg sc)
                                   (if (string=? sreg sc)
                                       (void)
                                       (begin
                                         (warn "changing docstring of category, from, to:"
                                               path sreg sc)
                                         (reg!)))
                                   (if sreg
                                       (void)
                                       (reg!)))))
                         creg c)))
                  (else (reg!))))))))

(def (cc-category-lookup [(list-of symbol?) path])
     (table-ref cc-category:register path #f))

(def (cc-category-list) -> (list-of cc-category?)
     (cmp-sort (table.values cc-category:register)
               (on cc-category.path generic-cmp)))

(def (cc-category-paths) -> (list-of (list-of symbol?))
     (cmp-sort (table.keys cc-category:register)
               generic-cmp))

