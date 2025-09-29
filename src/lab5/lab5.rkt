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
    [(lazy-product 0 _ ...) 0]
    [(lazy-product x xs ...)
     (* x (lazy-product xs ...))]))

; (lazy-product 1 2 3 4 5)
; 120
; (lazy-product 1 2 3 0 4 5 (error "не вызывается"))
; 0
