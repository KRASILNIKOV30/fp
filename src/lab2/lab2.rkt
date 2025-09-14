#lang slideshow

(define (my-reverse lst)
  (foldl
   (lambda (x result) (cons x result))
   empty
   lst))

(define (my-flatten lst)
  (foldl
   (lambda (x result)
     (append result
             (cond
               [(cons? x) (my-flatten x)]
               [else (list x)])))
   empty
   lst))

(define (gen-sized-list size init generator)
  (cond
    [(= size 0) '()]
    [else (cons
           init
           (gen-sized-list
            (- size 1)
            (generator init)
            generator))]))

(define (gen-list init generator)
  (let ([next (generator init)])
    (cond
      [(not next) '()]
      [else (cons
             init
             (gen-list next generator))])))

(define (gen-range start end step)
  (cond
    [(> start end) '()]
    [else (cons
           start
           (gen-range (+ start step) end step))]))

(gen-range 0 10 3)
