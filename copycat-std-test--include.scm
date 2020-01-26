
;; Test functions for running the test suite

(def (t* prog)
     (in-monad Result
               (==> (cc-interpreter.eval (cc-interpreter '() 10000 0)
                                         prog)
                    ((comp return cc-interpreter.stack)))))

(def (t prog)
     (=> (t* prog)
         .show))

