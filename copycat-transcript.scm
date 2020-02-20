;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         unixtime
         (posix/cj-posix _open
                         O_APPEND
                         O_CREAT
                         O_EXCL
                         O_WRONLY
                         fd->port)
         Result)

(export new-transcript)


;; XX lib

;; (def (intflags-has flags one-of-flags) -> boolean?
;;      (not (zero? (bitwise-and flags one-of-flags))))

(def (posix:open-flags->maybe-direction [uint32? flags]) -> symbol?
     (xcase (bitwise-and flags 3)
            ((0) 'input)
            ((1) 'output)
            ((2) 'input-output)))

(TEST
 > (map (lambda_ (%try (posix:open-flags->maybe-direction _)))
        (list O_WRONLY
              O_RDWR
              O_RDONLY
              (bitwise-or O_RDWR O_WRONLY)))
 ((value output)
  (value input-output)
  (value input)
  (exception text: "no match for: 3\n")))


(def (open [path-string? path]
           #!key
           [uint32? flags]
           (mode #o666)
           [(maybe list?) settings])
     -> (Result-of port?
                   posix-exception?)

     (let (res (posix:_open path flags mode))
       (if (posix-exception? res)
           (Error res)
           (Ok (fd->port res
                         (posix:open-flags->maybe-direction flags)
                         settings
                         (path-normalize path))))))

(def (open-new [path-string? path]
               #!key
               (flags (bitwise-or O_CREAT O_EXCL O_WRONLY O_APPEND))
               (mode #o600)
               [(maybe list?) settings])
     -> (Result-of output-port?
                   posix-exception?)
     (open path flags: flags mode: mode settings: settings))

;;/ lib


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


