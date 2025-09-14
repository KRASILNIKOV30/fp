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

; Упражнение 2.2

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
    [(> start end) empty]
    [else (cons
           start
           (gen-range (+ start step) end step))]))

; Упражнение 2.3

(define (length-via-foldl lst)
  (foldl
   (lambda (x result) (+ result 1))
   0
   lst))

(define (reverse-via-foldl lst)
  (foldl cons empty lst))

(define (map-via-foldl fn lst)
  (foldl
   (lambda (x result) (append result (list (fn x))))
   empty
   lst))

; Упражнение 2.4

(define (my-map f lst)
  (cond
    [(empty? lst) empty]
    [else (cons
           (f (first lst))
           (my-map f (rest lst)))]))

(define (my-filter pred lst)
  (cond
    [(empty? lst) empty]
    [else
     (if (pred (first lst))
         (cons (first lst) (my-filter pred (rest lst)))
         (my-filter pred (rest lst)))]))

(define (my-andmap pred lst)
  (cond
    [(empty? lst) #t]
    [(not (pred (first lst))) #f]
    [else (my-andmap pred (cdr lst))]))

(define (my-ormap pred lst)
  (cond
    [(empty? lst) #f]
    [(pred (first lst)) #t]
    [else (my-ormap pred (rest lst))]))

(define (my-foldl f init lst)
  (cond
    [(empty? lst) init]
    [else (my-foldl
           f
           (f (first lst) init)
           (rest lst))]))

; Докажем, что (map (lambda (x) x) lst) = lst
; 1. Для пустого списка
;    (my-map (lambda (x) x) '()) = '()
;    empty = '()
;    '() = '()
;
; 2. Для непустого списка
;    обозначим (lambda (x) x) = id
;    (my-map id '(a b)) = lst
;    (cons (id 'a) my-map id '(b)) = '(a, b)
;    (cons 'a (cons (id 'b) (my-map id '()))) = '(a, b)
;    (cons 'a (cons 'b '())) = '(a, b)
;    (cons 'a '(b)) = '(a, b)
;    '(a, b) = '(a, b)


; Докажем, что (map f (map g lst)) = (map (lambda (x) (f (g x))) lst)
; 1. Для пустого списка lst = '()
; Левая часть (ЛЧ): (my-map f (my-map g '()))
;   (my-map g '()) = '()
;   (my-map f '()) = '()
;   Следовательно, ЛЧ = '()
; Правая часть (ПЧ): (my-map (lambda (x) (f (g x))) '())
;   (my-map (lambda (x) (f (g x))) '()) = '()
;   Следовательно, ПЧ = '()
; Вывод: ЛЧ = ПЧ = '()
;
; 2. Для непустого списка lst = '(a b)
; Обозначим (lambda (x) (f (g x))) = comp
; Левая часть (ЛЧ): (my-map f (my-map g '(a b)))
;   (my-map g '(a b)) = (cons (g 'a) (my-map g '(b)))
;   = (cons (g 'a) (cons (g 'b) (my-map g '())))
;   = (cons (g 'a) (cons (g 'b) '()))
;   = '( (g a) (g b) )
;   (my-map f '( (g a) (g b) )) = (cons (f '(g a)) (my-map f '( (g b) )))
;   = (cons (f '(g a)) (cons (f '(g b)) (my-map f '())))
;   = (cons (f '(g a)) (cons (f '(g b)) '()))
;   = '( (f (g a)) (f (g b)) )
;   Следовательно, ЛЧ = '((f (g a)) (f (g b)))
; Правая часть (ПЧ): (my-map comp '(a b))
;   (my-map comp '(a b)) = (cons (comp 'a) (my-map comp '(b)))
;   = (cons (f (g 'a)) (my-map comp '(b)))
;   (my-map comp '(b)) = (cons (comp 'b) (my-map comp '()))
;   = (cons (f (g 'b)) '())
;   Подставляем обратно:
;   (cons (f (g 'a)) (cons (f (g 'b)) '()))
;   = '((f (g a)) (f (g b)))
;   Следовательно, ПЧ = '((f (g a)) (f (g b)))
; Вывод: ЛЧ = ПЧ = '((f (g a)) (f (g b)))
