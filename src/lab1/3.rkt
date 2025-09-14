#lang slideshow

(define bar-width 5)
(define bar-height 100)

(define (bar-lines digits)
  (apply hc-append
         (map (lambda (digit)
                (cond
                  [(= digit 1)
                   (colorize (filled-rectangle bar-width bar-height) "black")]
                  [else
                   (colorize (filled-rectangle bar-width bar-height) "white")]))
         digits)))

(bar-lines '(1 1 0 1 0 0 1))
