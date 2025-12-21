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


