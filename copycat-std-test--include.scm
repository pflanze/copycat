
;; Test functions for running the test suite

(def (t* prog)
     (in-monad Result
               (==> (cc-interpreter.eval-with-dyn-boundary
                     (fresh-cc-interpreter 10000)
                     prog)
                    ((comp return cc-interpreter.stack)))))

(def (t prog)
     (=> (t* prog)
         .show))

