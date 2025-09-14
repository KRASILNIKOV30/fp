#lang slideshow

(define bar-width 2)
(define bar-height 100)
(define separator-height 115)

(define (bar-line digit height)
  (cond
    [(= digit 1)
     (colorize (filled-rectangle bar-width height) "black")]
    [else
     (colorize (filled-rectangle bar-width height) "white")]))

(define (bar-lines-with-height digits height)
  (apply hc-append
         (map (lambda (digit)
                (bar-line digit height))
              digits)))

(define (bar-lines digits)
  (bar-lines-with-height digits bar-height))

(define (bar-separator)
  (bar-lines-with-height '(0 1 0 1 0) separator-height))

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
           [(1) 0]
           [(0) 1]))
       (l-code digit)))

(define (g-code digit)
  (reverse (r-code digit)))

(define (bar-encode-with structure digits)
  (apply
   append
   (map
    (lambda (type digit)
      (case type
        [(L) (l-code digit)]
        [(G) (g-code digit)]
        [(R) (r-code digit)]
        [else (error "Unknown code type in structure:" type)]))
    structure
    digits)))

(define (bar-code-EAN-8 digits)
  (ht-append
   (bar-separator)
   (bar-lines (bar-encode-with '(L L L L) (take digits 4)))
   (bar-separator)
   (bar-lines (bar-encode-with '(R R R R) (drop digits 4)))
   (bar-separator)))

(define (ean13-structure first-digit)
  (case first-digit
    [(0) '(L L L L L L)]
    [(1) '(L L G L G G)]
    [(2) '(L L G G L G)]
    [(3) '(L L G G G L)]
    [(4) '(L G L L G G)]
    [(5) '(L G G L L G)]
    [(6) '(L G G G L L)]
    [(7) '(L G L G L G)]
    [(8) '(L G L G G L)]
    [(9) '(L G G L G L)]
    [else (error "Invalid first digit for EAN-13")]))

(define (bar-code-EAN-13 digits)
  (ht-append
   (bar-separator)
   (bar-lines (bar-encode-with
               (ean13-structure (first digits))
               (take (rest digits) 6)))
   (bar-separator)
   (bar-lines (bar-encode-with
               '(R R R R R R)
               (drop digits 7)))
   (bar-separator)))

(bar-code-EAN-13 '(5 9 0 1 2 3 4 1 2 3 4 5 7))
