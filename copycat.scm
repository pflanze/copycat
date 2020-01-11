;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         copycat-interpreter
         copycat-std)

(export cc-repl)


(def (_cc-repl stack past future) -> !
     (in-monad
      Result
      (pretty-print (cj-desourcify stack)) ;; XX display modes?
      (display "$ ")
      (let (err (lambda (e)
                  (warn "Error:" (try-show e))
                  (_cc-repl stack past future)))
        (if-Ok (>>= (let (($s stack)
                          ($word 'cc-repl))
                      (copycat:try-Ok
                       (with-input-from-string (read-line)
                         read-all-source)))
                    (lambda (prog)
                      (mcase prog
                             ;; handle pseudo commands
                             (`(undo)
                              (if-let-pair ((p past*) past)
                                           (_cc-repl p
                                                     past*
                                                     (cons stack future))
                                           (err (XXX))))
                             (`(redo)
                              XXX)
                             (else
                              (cc-eval stack prog)))))
               (_cc-repl it
                         (cons it (rappend future (cons stack past)))
                         '())
               (err it)))))

(def (cc-repl #!optional (stack '())) -> !
     (read-line)
     (_cc-repl stack '() '()))

