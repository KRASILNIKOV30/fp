#lang slideshow
(require racket/generator)

(define m 2147483648)
(define a 1103515245)
(define c 12345)

(struct rng (seed))

(define (make-rng n)
  (rng n))

(define (next-rng g)
  (rng  (modulo (+ (* a (rng-seed g)) c) m)))

;(rng-seed (next-rng (make-rng 123)))

(define (random-integer i j)
  (lambda (g)
    (let ([next-g (next-rng g)])
      (cons
       (+ i (round (* (- j i) (/ (rng-seed next-g) m))))
       next-g))))

((random-integer 7 77) (make-rng 19))

