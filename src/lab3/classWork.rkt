#lang slideshow
(require racket/generator)

(define (check-digits lst)
  (for/list ([i (in-naturals 0)]
             [item lst]
             #:unless (and (integer? item) (<= 0 item 9)))
    (cons i item)))

(define (fib-like? lst)
  (unless (> (length lst) 1) #t)
  
  (for/and ([pre-prev lst]
            [prev (rest lst)]
            [cur (rest (rest lst))])
    (= cur (+ pre-prev prev))))

(define (gen-fib n1 n2)
  (stream-cons n1 (gen-fib n2 (+ n1 n2))))

(define in-fib (gen-fib 1 1))

(define (fib-generator n1 n2)
    (yield n1)
    (fib-generator n2 (+ n1 n2)))

(define fib-gen
  (generator () (fib-generator 1 1)))

(define (ascending? lst)
  (for/and ([a lst]
            [b (rest lst)])
    (< a b)))

(define (moving-sum-3 lst)
  (for/fold ([result empty])
            ([n1 lst] [n2 (cdr lst)] [n3 (cddr lst)])
    (append result (list (+ n1 n2 n3)))))

;(moving-sum-3 '(1 2 3 4 5 6 7))

(define/match (choose-2 lst)
  [((cons head tail))
   (append
    (for/list ([x tail])
      (cons head x))
    (choose-2 tail))]
  [(empty) empty])

;(choose-2 '(a b c d))
; '((a . b) (a . c) (a . d) (b . c) (b . d) (c . d))

(define (coprime? lst)
  (for*/and ([n1 lst]
             [n2 lst]
             #:unless (= n1 n2))
    (= (gcd n1 n2) 1)))

; (coprime? '(2 3 5 7 9)) ; #f
; (coprime? '(8 9 35 11)) ; #t

(define/match (choose lst n)
  [(lst 2)
   (choose-2 lst)]
  [((cons head tail) n)
   (append
    (for/list ([x tail])
      (cons head x))
    (choose tail (- n 1)))])

(choose '(a b c d e) 3)
; '((a b c) (a b d) (a b e) (a c d) (a c e) (a d e)
;   (b c d) (b c e) (b d e) (c d e))