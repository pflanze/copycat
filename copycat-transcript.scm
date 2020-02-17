;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         unixtime)

(export new-transcript)


(def (localtime-filenamestring)
     (=> (current-localtime)
         .localtime-string
         (.filter-map (lambda (c)
                        (case c
                          ((#\, #\:) #f)
                          ((#\space) #\_)
                          (else c))))))

(def (new-transcript-name)
     (let lp (n 0)
       (let (path ($ "transcript-"
                     (localtime-filenamestring)
                     (if (zero? n) "" ($ "-$n"))
                     ".scm"))
         (if (file-exists? path)
             (lp (inc n))
             path))))

(def (new-transcript)
     (open-output-file (new-transcript-name)))


