;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         unixtime
         (posix/nice open-new))

(export new-transcript)

(include "lib/cj-standarddeclares.scm")


(def (localtime-filenamestring)
     (=> (current-localtime)
         .localtime-string
         (.filter-map (lambda (c)
                        (case c
                          ((#\, #\:) #f)
                          ((#\space) #\_)
                          (else c))))))


(def (new-transcript) -> output-port?
     (let (fns (localtime-filenamestring))
       (let lp (n 0)
         (if-Ok (open-new ($ "transcript-$fns"
                             (if (zero? n) "" ($ "-$n"))
                             ".scm"))
                it
                (if (< n 100)
                    (lp (inc n))
                    (raise it))))))


