;; https://blog.plover.com/brain/hp-15c.html
;; (https://news.ycombinator.com/item?id=22222897)

;; "Then on (a) Friday night I was sitting around thinking about which
;; numbers n are such that 10n^2+9 (is) a perfect square, and I
;; couldn't think of any examples except for 0, 2, and 4. Normally I
;; would just run and ask the computer, which would take about two
;; minutes to write the program and one second to run it. But I was
;; out in the courtyard, it was a really nice evening, my favorite
;; time of the year, the fading light was beautiful, and I wasn't
;; going to squander it by going inside to brute-force some number
;; problem."

: formula (square 10 * 9 +)

: perfect-square? (sqrt integer?)

: matches? (formula perfect-square?)

(: hp100 -> list?
   (100 iota (dup matches? 2 vector) list-map))

(: hp [integer? from] -> !
   "Show the next solution, wait for the enter key to be pressed."
   (dup matches?
        (dup println
             "hit enter for the next result!" ask-string drop)
        when
        inc hp))

