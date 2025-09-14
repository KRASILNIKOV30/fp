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

(define (l-code digit)
  (case digit
    [(0) '(0 0 0 1 1 0 1)]
    [(1) '(0 0 1 1 0 0 1)]
    [(2) '(0 0 1 0 0 1 1)]
    [(3) '(0 1 1 1 1 0 1)]
    [(4) '(0 1 0 0 0 1 1)]
    [(5) '(0 1 1 0 0 0 1)]
    [(6) '(0 1 0 1 1 1 1)]
    [(7) '(0 1 1 1 0 1 1)]
    [(8) '(0 1 1 0 1 1 1)]
    [(9) '(0 0 0 1 0 1 1)]
    [else (error "not a digit")]))

(define (r-code digit)
  (map (lambda (d)
         (case d
           [(1) (0)]
           [else (1)]))
       (l-code digit)))

(l-code 7)
