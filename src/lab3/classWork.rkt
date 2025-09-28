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

(define (coprime? lst)
  (for*/and ([n1 lst]
             [n2 lst]
             #:unless (= n1 n2))
    (= (gcd n1 n2) 1)))

; (coprime? '(2 3 5 7 9)) ; #f
; (coprime? '(8 9 35 11)) ; #t

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

;(for/list ([x (stream-lists '(1 2 3 4) '(1 2 3 4))]) x)

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

(for/list ([i 10] [x (stream-zigzag (in-naturals 1) (in-naturals 1))]) x)

; '((1 . 1)
;  (1 . 2) (2 . 1)
;  (1 . 3) (2 . 2) (3 . 1))

