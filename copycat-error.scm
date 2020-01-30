;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         test)

(export possibly-source?
        copycat:predicate-accepts-source?)

"Representation for errors in Copycat (guest accessible) (used with
Result)"

(include "lib/cj-standarddeclares.scm")


(defclass ((copycat-error #f) [possibly-source? offending-code])

  (defmethod- (maybe-help s) #f)

  (defclass (copycat-exit-repl)
    (defmethod (explanation s)
      "not an error, but a signal for the current evaluator to stop
evaluation (and presumably continue evaluation of the program in the
outer context, if any)."))

  (defclass (copycat-out-of-fuel)
    (defmethod (explanation s)
      "no fuel left")
    (defmethod (maybe-help s)
      "Perhaps the program ran in an infinite loop. If
you think it isn't, try giving it more fuel via `add-fuel` or
`set-fuel`."))

  (defclass (copycat-interrupted)
    (defmethod (explanation s)
      "received interrupt signal (SIGINT, ctl-c)"))

  (defclass (copycat-generic-error [string? msg]
                                   [ilist? args])
    (defmethod (explanation s)
      "generic error"))

  (defclass (copycat-assertment-failure)
    (defmethod (explanation s)
      "`assert` was called with #f as the argument."))

  (defclass (copycat-unbound-symbol [symbol? name])
    (defmethod (explanation s)
      "the given word is not defined"))

  (defclass (copycat-missing-arguments proc
                                       [fixnum-natural0? need]
                                       [fixnum-natural0? got])
    (defmethod (explanation s)
      ($ "the given operation needs more arguments than are "
         "available (either on the stack, or (in the case of "
         "special syntax) in the program)")))

  (defclass (copycat-division-by-zero numerator)
    (defmethod (explanation s)
      "attempt to divide by exactly 0"))

  (defclass (copycat-out-of-bounds-access asked
                                          [fixnum-natural0? length])
    (defmethod (explanation s)
      "attempt to access an index outside the range of valid indices"))

  (defclass (copycat-out-of-range [string? predicate] value)
    (defmethod (explanation s)
      "value is not in range for given predicate"))

  (defclass (copycat-type-error [string? predicate] value)
    (defmethod (explanation s)
      "value encountered does not match given type declaration"))

  (defclass (copycat-host-error exception)
    (defmethod (explanation s)
      "an exception was thrown from code implemented in the host language"))

  (defclass (copycat-invalid-type [string? reason])
    (defmethod (explanation s)
      "the given type declaration is syntactically invalid")))

