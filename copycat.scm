;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         copycat-interpreter
         copycat-std)

(export cc-repl)


(def (_cc-repl stack) -> !
     (in-monad
      Result
      (pp stack)
      (display "$ ")
      (if-Ok (>>= (let (($s stack)
                        ($word 'cc-repl))
                    (copycat:try
                     (with-input-from-string (read-line)
                       read-all-source)))
                  (C cc-eval stack _))
             (_cc-repl it)
             (begin
               (warn "Error:" (try-show it))
               (_cc-repl stack)))))

(def (cc-repl #!optional (stack '())) -> !
     (read-line)
     (_cc-repl stack))

