#lang slideshow
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)

(defrel (appendo xs ys xys)
  (matche xs
          [() (== ys xys)]
          [(,z . ,zs)
           (fresh (zys)
                  (== (cons z zys) xys)
                  (appendo zs ys zys))]))

(defrel (membero x xs)
  (matche xs
          [(,y . ,ys)
           (conde
            [(== x y)]
            [(membero x ys)])]))

(define (replicateo n one many)
  (conde
   [(zeroo n) (== many '())]
   [(poso n)
    (fresh (n-1 rest)
           (pluso n-1 '(1) n)
           (appendo one rest many)
           (replicateo n-1 one rest))]))

(run* (q) (replicateo (build-num 3) '(a b c) q))
; '((a b c a b c a b c))

(run* (q) (replicateo (build-num 3) q '(a b c a b c a b c)))
; '((a b c))

(run 1 (q) (replicateo q '(a b c) '(a b c a b c a b c)))
; '((1 1))


(define (wordso sentence words)
  (conde
   ;; Базовый случай: предложение закончилось
   [(== sentence '())]
    
   ;; Рекурсивный шаг
   [(fresh (word rest)
           ;; Пытаемся разбить sentence на префикс (word) и остаток (rest)
           (appendo word rest sentence)
           ;; Префикс должен быть одним из допустимых слов
           (membero word words)
           ;; Остаток должен быть также составлен из слов
           (wordso rest words))]))


(run* (q) (wordso '(a h a t h e h a d) '((h e) (h a d) (a) (h a t))))
; '(_.0)

(run* (q) (wordso '(a h a t h e h a d) '((h e) (h a d) (h a t))))
; '()
