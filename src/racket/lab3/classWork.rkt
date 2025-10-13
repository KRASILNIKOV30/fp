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

; for/list (исправлено)
(define (moving-sum-3 lst)
  (for/list
      ([n1 lst] [n2 (cdr lst)] [n3 (cddr lst)])
    (+ n1 n2 n3)))

; (moving-sum-3 '(1 2 3 4 5 6 7))

(define/match (choose lst n)
  [(_ 0) (list empty)]
  [('() n) empty]
  [((cons head tail) n)
   (append
    (for/list ([x (choose tail (- n 1))])
      (cons head x))
    (choose tail n))])

; (choose '(a b c d e) 3)
; '((a b c) (a b d) (a b e) (a c d) (a c e) (a d e)
;   (b c d) (b c e) (b d e) (c d e))

(define (choose-2 lst)
  (choose lst 2))

; (choose-2 '(a b c d))
; '((a . b) (a . c) (a . d) (b . c) (b . d) (c . d))

; исправлено
(define (coprime? lst)
  (for/and ([pair (choose-2 lst)])
            (= (gcd
                (first pair)
                (second pair))
               1)))

(coprime? '(2 3 5 7 9)) ; #f
(coprime? '(8 9 35 11)) ; #t

; Упражнение 3.2

(define (trib-stream n1 n2 n3)
  (stream-cons n1 (trib-stream n2 n3 (+ n1 n2 n3))))

(define in-tribonacci (trib-stream 0 0 1))

; (for/list ([i 10] [f in-tribonacci]) f)
; '(0 0 1 1 2 4 7 13 24 44)

(define (stream-takef strm pred)
  (let ([n (stream-first strm)])
    (cond
      [(pred n) (cons
                 n
                 (stream-takef
                  (stream-rest strm)
                  pred))]
      [else empty])))

; (stream->list (stream-takef (in-naturals 1) (lambda (x) (< x 10))))
; '(1 2 3 4 5 6 7 8 9)

(define/match (stream-rest-n strm n)
  [(strm 0) strm]
  [(_ _) (stream-rest-n
          (stream-rest strm)
          (- n 1))])

(define (stream-chunks strm n)
  (define (get-chunk strm n)
    (for/fold ([result empty])
              ([i n]
               [el strm])
      (append result (list el))))
  (stream-cons
   (get-chunk strm n)
   (stream-chunks (stream-rest-n strm n) n)))

; (for/list ([i 3] [chunk (stream-chunks (in-naturals 1) 4)]) chunk)
; '((1 2 3 4) (5 6 7 8) (9 10 11 12))

(define (stream-lists lst1 lst2)
  (for/stream ([n1 lst1]
               [n2 (reverse lst2)])
    (cons n1 n2)))

; (for/list ([x (stream-lists '(1 2 3 4) '(1 2 3 4))]) x)

(define (stream-zigzag strm1 strm2)
  (define (helper strm1 strm2 lst1 lst2 strm)
    (cond
      [(stream-empty? strm)
       (let ([new-lst1 (append lst1 (list (stream-first strm1)))]
             [new-lst2 (append lst2 (list (stream-first strm2)))])
         (helper
          (stream-rest strm1)
          (stream-rest strm2)
          new-lst1
          new-lst2
          (stream-lists new-lst1 new-lst2)))]
      [else
       (stream-cons
        (stream-first strm)
        (helper strm1 strm2 lst1 lst2 (stream-rest strm)))]))
  (helper strm1 strm2 empty empty empty-stream))

;(for/list ([i 10] [x (stream-zigzag (in-naturals 1) (in-naturals 1))]) x)

; '((1 . 1)
;  (1 . 2) (2 . 1)
;  (1 . 3) (2 . 2) (3 . 1))

(define (my-stream-map f s)
  (stream-cons
   (f (stream-first s))
   (my-stream-map f (stream-rest s))))

(define (my-stream-take s n)
  (cond
    [(= n 0) empty-stream]
    [else
     (stream-cons
      (stream-first s)
      (my-stream-take (stream-rest s) (- n 1)))]))

;(stream->list
; (my-stream-take (my-stream-map (lambda (x) (* x x))
;                          (in-naturals 1))
;              10))
; '(1 4 9 16 25 36 49 64 81 100)

; Доказательство

; Предположение индукции:
; (stream-take (stream-map f s) n) = (stream-map f (stream-take s n))

; Базовый случай: n = 0
; 1. (stream-take (stream-map f s) 0)= empty-stream
;
; 2. (stream-map f (stream-take s 0))
; = (stream-map f empty-stream) = empty-stream
; => empty-stream = empty-stream

; Левая часть для n = k+1:
; (stream-take (stream-map f s) (+ k 1))
; = (stream-cons (f (stream-first s)) 
;                (stream-take (stream-map f (stream-rest s)) k))

; Правая часть для n = k+1:
; (stream-map f (stream-take s (+ k 1)))
; = (stream-map f (stream-cons (stream-first s)   
;                             (stream-take (stream-rest s) k)))
; = (stream-cons (f (stream-first s))              
;                (stream-map f (stream-take (stream-rest s) k)))

; По предположению индукции:
; (stream-take (stream-map f (stream-rest s)) k) = (stream-map f (stream-take (stream-rest s) k))

; Следовательно, левая и правая части равны 

; Сделать через генератор (исправлено)
(define m 2147483648)
(define a 1103515245)
(define c 12345)     

(define (rand-gen init)
  (begin
    (let ([next (modulo (+ (* a init) c) m)])
      (begin
        (yield next)
        (rand-gen next)))))

(define rand
  (generator () (rand-gen 1)))

;(rand) ; 472202153
;(rand) ; 1639847214
;(rand) ; 402077903
;(rand) ; 111964764
;(rand) ; 829221221
;(rand) ; 1671059002
;(rand) ; 1507365611
;(rand) ; 180115528
;(rand) ; 80197345
;(rand) ; 1976015878
