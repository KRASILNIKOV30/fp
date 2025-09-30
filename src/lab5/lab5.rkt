#lang slideshow

(define/match (palindrome? lst)
  [((list a b ... c))
   (and (eq? a c) (palindrome? b))]
  [(_) #t])

; (palindrome? '(b b a b a b b))
; #t

(define cart-total
  (match-lambda
    [(list (list _ price count) ...)
     (apply + (map * price count))]))

; (cart-total '((milk 119.99 3) (bread 89.99 2) (butter 189.99 1)))
; 729.9399999999999

(define-syntax implies
  (syntax-rules ()
    [(implies) #t]
    [(implies x) #t]
    [(implies a b xs ...)
     (and
      (or (not a) b)
      (implies xs ...))]))

; (implies (< 1 5) (< 1 10) (< 0 10))
; #t

(define-syntax lazy-product
  (syntax-rules ()
    [(lazy-product) 1]
    [(lazy-product x xs ...)
     (cond
       [(= x 0) 0]
       [else (* x (lazy-product xs ...))])]))

; (lazy-product 1 2 3 4 5)
; 120
; (lazy-product 1 2 3 (- 1 1) 4 5 (error "не вызывается"))
; 0

; Упражнение 5.2 (Честное ветвление).

(define-syntax cond-all
  (syntax-rules ()
    [(cond-all) (void)]
    [(cond-all [condition branch] branches ...)
     (begin
       (if condition branch (void))
       (cond-all branches ...))]))

;(let ([n 15])
;  (cond-all
;   [(= 0 (modulo n 3)) (displayln 'fizz)]
;   [(= 0 (modulo n 5)) (displayln 'buzz)]
;   [(= 1 (modulo n 3)) (displayln 'error)]))

(define-syntax cond-all/list
  (syntax-rules ()
    [(cond-all/list) empty]
    [(cond-all/list [condition branch] branches ...)
     (cond
       [condition (cons branch (cond-all/list branches ...))]
       [else (cond-all/list branches ...)])]))

;(let ([n 15])
;  (cond-all/list
;   [(= 0 (modulo n 3)) 'fizz]
;   [(= 0 (modulo n 5)) 'buzz]
;   [(= 1 (modulo n 5)) 'error]))
; '(fizz buzz)

(define-syntax match-all
  (syntax-rules ()
    [(match-all lst) empty]
    [(match-all lst [patt branch] branches ...)
     (match lst
       [patt (cons branch (match-all lst branches ...))]
       [_ (match-all lst branches ...)])]))

(define-syntax-rule (match-all/append lst branches ...)
  (apply append (match-all lst branches ...)))

(let ([lst '(a b b a b b b a a)])
  (match-all/append lst
                    [(list l ... x x x r ...) (list (cons 3 x))]
                    [(list l ... x x r ...)
                     (list (cons 2 x))]))

; '((3 . b) (2 . b) (2 . a))

; Упражнение 5.3 (Цикл while)

;(define-syntax while/list
;  (syntax-rules ()
;    []))

;(while/list (current 1)
;            (< current 100)
;            (* current 2))
; '(1 2 4 8 16 32 64)
