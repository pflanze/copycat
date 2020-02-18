
-6 1 list 'star-num-rays set!
-360 star-num-rays / 1 list 'star-ray-total-angle set!
(16) 'star-ray-small-angle set!
star-ray-total-angle star-ray-small-angle + 1 list 'star-ray-big-angle set!


: ray (dup f
           star-ray-small-angle l
           b
           star-ray-small-angle r ;; why is this needed?
           star-ray-total-angle r)

: star ((dup ray) 6 repeat drop)

;; XX should allow to retrieve that from actual canvas size instead!
: canvas-width (200)
: canvas-height (200)
: canvas-x0 (-50)
: canvas-y0 (-50)


: sky (
        (
         ;; star size
         random-real square 4 *

            ;; X position
            canvas-width 20 + random-natural0 canvas-x0 + 10 -
            ;; Y position
            canvas-height 20 + random-natural0 canvas-y0 + 10 -

            ;; set position  XX rename this op
            start

            star)
        70
        repeat)
