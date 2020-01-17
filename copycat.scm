;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         copycat-interpreter
         copycat-std)

(export cc-repl)


(include "lib/cj-standarddeclares.scm")



(def (_cc-repl cci past future last-commands) -> !
     (in-monad
      Result
      (pretty-print (cj-desourcify (.stack cci))) ;; XX display modes?
      (display ($ (.fuel cci) " \\$ "))
      (let (err (lambda (e) ;; takes old cci -- we don't have a new one
                       ;; anyway, currently
                  (warn "Error:" (try-show e))
                  (_cc-repl cci past future (turtle-commands))))
        (if-Ok (>>= (let (($word 'cc-repl))
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
                              (>>= (cc-interpreter.eval cci prog)
                                   ;; Hack: proper hook?
                                   (lambda (cci)
                                     (if (eq? (turtle-commands) last-commands)
                                         (Ok cci)
                                         (cc-interpreter.eval cci '(zeig)))))))))
               (_cc-repl it
                         (cons it (rappend future (cons cci past)))
                         ;; ^ XX undo/redo retain fuel, too, now.
                         '()
                         (turtle-commands))
               (err it)))))

(def (cc-repl #!optional (cci (fresh-cc-interpreter))) -> !
     (read-line)
     (_cc-repl cci '() '() (turtle-commands)))

