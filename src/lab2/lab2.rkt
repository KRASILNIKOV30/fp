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

(define (my-encode lst))

(my-encode '(a a a a b c c a a d e e e e))