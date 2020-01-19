
;; The factorial, !n

: fact (dup zero? (drop 1) (dup 1 - fact *) thenelse)

;; Could also use extended definition syntax for allowing a docstring
;; and type declarations; and could use the |!| name; also, could use
;; the shorter |if| alias.

(: ! [natural? n] -> natural?
   "The factorial, !n"
   (dup zero? (drop 1) (dup 1 - fact *) if))
