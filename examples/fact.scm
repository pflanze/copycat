
;; The factorial, !n

: fact (dup zero? (drop 1) (dup 1 - fact *) thenelse)

;; Could also use extended definition syntax for allowing a docstring
;; and type declarations; and could use the |!| name; also, could use
;; the shorter |if| alias.

(: !.1 [natural0? n] -> natural?
   "The factorial, !n"
   (dup zero? (drop 1) (dup 1 - fact *) if))


;; Currently, type declarations are decorative only, so we have to
;; verify inputs ourselves via `assert` if we want to avoid the
;; infinite loop associated with negative inputs. Once we do that, if
;; we want to do the check only once for efficiency, we need a helper
;; function, and then we could as well also move to an iterative
;; algorithm at the same time:

(: !-iter start n -> start*
   "The iterative factorial function, which multiplies start*n then
recurses with that and the decremented n."
   (dup zero?
        (drop)
        (swap over * swap dec !-iter) if))

(: ! [natural0? n] -> natural?
   "The factorial, !n"
   (
    dup natural0? assert
    1 swap !-iter))

