;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         test)

(export (interface cc-type
          (class cc-type-unknown
            (class cc-type/unknown-results
              (class cc-type/results))))
        cc-parse-type)

"Parsing Copycat type declarations"

(include "lib/cj-standarddeclares.scm")


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
                   copycat-error?)
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


