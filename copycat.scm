;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         unixtime
         copycat-interpreter
         copycat-std
         copycat-turtle;; XX
         )

(export cc-repl
        cc-repl*
        #!optional
        _cc-repl)


(include "lib/cj-standarddeclares.scm")


(def (localtime-filenamestring)
     (=> (current-localtime)
         .localtime-string
         (.filter-map (lambda (c)
                        (case c
                          ((#\, #\:) #f)
                          ((#\space) #\_)
                          (else c))))))


(def (_cc-repl [cc-interpreter? cci]) -> cc-interpreter?
     (let lp ((cci cci)
              (past '())
              (future '())
              (last-commands (turtle-commands)))
       (let (($word 'cc-repl)
             (maybe-transcript-port (.maybe-transcript-port cci)))
         (in-monad
          Result
          (pretty-print (cj-desourcify (.stack cci))) ;; XX display modes?
          (let (level (.repl-level cci))
            (display ($ (.fuel cci)
                        (if (zero? level) "" ($ " $level"))
                        " \\@ ")))
          (let* ((next (lambda (cci past future)
                         ;; Hack: proper hook?
                         (let (tcmds (turtle-commands))
                           (unless (eq? tcmds last-commands)
                             (unwrap (cc-interpreter.eval cci '(zeig))))
                           (lp cci past future tcmds))))
                 (err (lambda (e)
                        ;; takes old cci -- we don't have a new one
                        ;; anyway, currently
                        (if (copycat-exit-repl? e)
                            cci
                            (begin
                              (warn "Error:" (try-show e))
                              (next cci past future))))))
            (if-Ok
             (>>= (copycat:try-Ok
                   (in-monad
                    maybe
                    (mlet (line (maybe-read-line))
                          (return
                           (values line
                                   (with-input-from-string line
                                     read-all-source))))))
                  (lambda (maybe-line+prog)
                    ;; nothing on eof/ctl-d
                    (if-just
                     maybe-line+prog
                     (letv ((line prog) it)

                           ;; Now that it is clear that
                           ;; `line` is proper s-expression
                           ;; syntax, write it down:
                           (when-just maybe-transcript-port
                                      (displayln line it)
                                      (force-output it))

                           (mcase
                            prog
                            ;; handle pseudo commands
                            (`(undo)
                             (if-let-pair ((p past*) past)
                                          (lp p
                                              past*
                                              (cons stack future))
                                          (err (XXX))))
                            (`(redo)
                             XXX)
                            (else
                             ;; HACK:
                             (set! *copycat-interpreter:interrupt* #f)
                             ;; /HACK
                             (let (res (cc-interpreter.eval cci prog))
                               (when-just
                                maybe-transcript-port
                                (displayln
                                 (let (maybe-level
                                       (let (level (.repl-level cci))
                                         ;; ^ XX or (.value res) if Ok ?
                                         (and (> level 0)
                                              level)))
                                   (if-Ok res
                                          (if-just maybe-level
                                                   ($";; ^(level $it)")
                                                   "")
                                          (=> it
                                              try-show
                                              pretty-string
                                              (string-split #\newline)
                                              (.map (lambda (line)
                                                      ($ ";; $line\n")))
                                              (.cons ($ ";; ^"
                                                        (if-just
                                                         maybe-level
                                                         ($ "(level $it) ")
                                                         "")
                                                        "Error:\n"))
                                              strings-append)))
                                 it)
                                (force-output it))
                               res))))
                     (Error (copycat-exit-repl #f)))))
             (next it
                   (cons it (rappend future (cons cci past)))
                   ;; ^ XX undo/redo retain fuel, too, now.
                   '())
             (err it)))))))


(def (new-transcript-name)
     (let lp ((n 0))
       (let (path ($ "copycat-transcript-"
                     (localtime-filenamestring)
                     (if (zero? n) "" ($ "-$n"))
                     ".scm"))
         (if (file-exists? path)
             (lp (inc n))
             path))))


(def (cc-repl* #!key
               (cci (fresh-cc-interpreter))
               [(maybe path-string?) transcript])
     -> cc-interpreter?
     "for running from console"
     (let (cont (lambda (maybe-transcript-port)
                  (_cc-repl (if maybe-transcript-port
                                (.maybe-transcript-port-set
                                 cci maybe-transcript-port)
                                cci))))
       (if-just transcript
                (if (file-exists? it)
                    (error "transcript file already exists:" it)
                    (call-with-output-file it cont))
                (let (path (new-transcript-name))
                  (call-with-output-file path cont)))))

(def (cc-repl . args) -> cc-interpreter?
     "for running from Emacs inferior scheme mode"
     (read-line)
     (apply cc-repl* args))

