#lang slideshow
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)

(defrel (membero x xs)
  (matche xs
          [(,y . ,ys)
           (conde
            [(== x y)]
            [(membero x ys)])]))

(define sample-context
  '((x . Int)
    (f . (Int -> Bool))
    (y . Int)
    (b . Bool)
    (+ . (Int -> (Int -> Int)))))

(defrel (context-lookupo var type context)
  (fresh (x t rest)
         (== `((,x . ,t) . ,rest) context)
         (conde
          [(== x var) (== t type)]
          [(=/= x var) (context-lookupo var type rest)])))

(displayln "---context-lookupo---")
(run* (type) (context-lookupo 'x type sample-context)) 
(run* (type) (context-lookupo 'z type sample-context))
(run* (var) (context-lookupo var 'Int sample-context))
(run* (var type) (context-lookupo var type sample-context))

(defrel (context-varso context vars)
  (conde
   [(== context '()) (== vars '())]
   [(fresh (x t rest xs)
           (== `((,x . ,t) . ,rest) context)
           (== vars `(,x . ,xs))
           (context-varso rest xs))]))

(displayln "---context-varso---")
(run* (vars) (context-varso sample-context vars))
(run* (context) (context-varso context '(x y z)))

(define (untyped-expro vars expr)
  (conde
   [(fresh (x)
           (== expr x)
           (symbolo x)
           (membero x vars))]
   [(fresh (n)
           (== expr n)
           (numbero n)
           (membero n vars))]
   [(fresh (e1 e2)
           (== expr `(,e1 + ,e2))
           (untyped-expro vars e1)
           (untyped-expro vars e2))]
   [(fresh (e1 e2)
           (== expr `(,e1 * ,e2))
           (untyped-expro vars e1)
           (untyped-expro vars e2))]
   [(fresh (f e)
           (== expr `(,f ,e))
           (untyped-expro vars f)
           (untyped-expro vars e))]
   [(fresh (e1 e2 e3)
           (== expr `(if ,e1 then ,e2 else ,e3))
           (untyped-expro vars e1)
           (untyped-expro vars e2)
           (untyped-expro vars e3))]))

(displayln "---untyped-expro---")
(run 6 (expr) (untyped-expro '(x) expr))
(run 14 (expr) (untyped-expro '(x y) expr))
(run 1 (vars) (untyped-expro vars
                             '(if (p x) then (x * (f (x + 1))) else (f (f x)))))

(define (bounded-untyped-expro max-depth vars expr)
  (fresh (d)
         (pluso '(1) d max-depth)
         (conde
          [(fresh (x)
                  (== expr x)
                  (symbolo x)
                  (membero x vars))]
          [(fresh (n)
                  (== expr n)
                  (numbero n)
                  (membero n vars))]
          [(fresh (e1 e2)
                  (== expr `(,e1 + ,e2))
                  (bounded-untyped-expro d vars e1)
                  (bounded-untyped-expro d vars e2))]
          [(fresh (e1 e2)
                  (== expr `(,e1 * ,e2))
                  (bounded-untyped-expro d vars e1)
                  (bounded-untyped-expro d vars e2))]
          [(fresh (f e)
                  (== expr `(,f ,e))
                  (bounded-untyped-expro d vars f)
                  (bounded-untyped-expro d vars e))]
          [(fresh (e1 e2 e3)
                  (== expr `(if ,e1 then ,e2 else ,e3))
                  (bounded-untyped-expro d vars e1)
                  (bounded-untyped-expro d vars e2)
                  (bounded-untyped-expro d vars e3))])))

(displayln "---bounded-untyped-extro---")
(run* (expr) (bounded-untyped-expro (build-num 2) '(x) expr))
; '(x (x x) (x + x) (x * x) (if x then x else x))
(run 1 (vars) (bounded-untyped-expro (build-num 5) vars
                                     '(if (p x) then (x * (f (x + 1))) else (f (f x)))))
; '((p x f 1 . _.0))

(define (typed-expro context expr type)
  (conde
   ; переменная/константа
   [(fresh (var)
           (== expr var)
           (symbolo var)
           (context-lookupo var type context))]
   [(fresh (n)
           (== expr n)
           (numbero n)
           (== type 'Int)
           (context-lookupo n 'Int context))]  ; числа в контексте как (5 . Int) и т.п.
   ; сложение
   [(fresh (e1 e2)
           (== expr `(,e1 + ,e2))
           (== type 'Int)
           (typed-expro context e1 'Int)
           (typed-expro context e2 'Int))]
   ; умножение — аналогично
   [(fresh (e1 e2)
           (== expr `(,e1 * ,e2))
           (== type 'Int)
           (typed-expro context e1 'Int)
           (typed-expro context e2 'Int))]
   ; применение f e
   [(fresh (f e X)
           (== expr `(,f ,e))
           (typed-expro context f `(,X -> ,type))
           (typed-expro context e X))]
   ; условное выражение
   [(fresh (e1 e2 e3 T)
           (== expr `(if ,e1 then ,e2 else ,e3))
           (== type T)
           (typed-expro context e1 'Bool)
           (typed-expro context e2 T)
           (typed-expro context e3 T))]))

(displayln "---typed-expro---")
(run* (q) (typed-expro sample-context '(if (f x) then (x + y) else y) 'Int))
; '(_.0)
(run* (q) (typed-expro sample-context '(if (f x) then (x + b) else y) 'Int))
; '()
(run* (type) (typed-expro
              `((1 . Int) . ,sample-context)
              '(if (f x) then (x + 1) else y)
              type))

(displayln "")
(run 1 (context) (typed-expro context '(if (f x) then (x + b) else y) 'Int))
; '(((f Int -> Bool) (x . Int) (b . Int) (y . Int) . _.0))
(run 1 (context) (typed-expro context '((f (g x)) + (g (f x))) 'Int))
; '(((f Int -> Int) (g Int -> Int) (x . Int) . _.0))
(run 1 (context type) (typed-expro context '(if (p x) then (f x) else x) type))
; '((((p _.0 -> Bool) (x . _.0) (f _.0 -> _.0) . _.1) _.0))


(displayln "")
(run 6 (expr) (typed-expro sample-context expr 'Int))
; '(x y (x + x) (x * x) (x + y) (if b then x else x))
(run 6 (expr) (typed-expro sample-context expr 'Bool))
; '(b (f x) (f y) (f (x + x)) (if b then b else b) (f (x * x)))
(run 10 (expr) (typed-expro sample-context expr '(Int -> Int)))
; '((+ x) (+ y) (+ (x + x)) (+ (x * x)) (+ (x + y)) (+ (if b then x else x)))

