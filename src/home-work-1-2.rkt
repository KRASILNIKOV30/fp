#lang slideshow

; Упражнение 1.3

(define bar-width 2)
(define bar-height 100)
(define separator-height 115)

(define (bar-line digit height)
  (cond
    [(= digit 1)
     (colorize (filled-rectangle bar-width height) "black")]
    [else
     (colorize (filled-rectangle bar-width height) "white")]))

(define (bar-lines-with-height digits height)
  (apply hc-append
         (map (lambda (digit)
                (bar-line digit height))
              digits)))

(define (bar-lines digits)
  (bar-lines-with-height digits bar-height))

(define (bar-separator)
  (bar-lines-with-height '(0 1 0 1 0) separator-height))

(define (l-code digit)
  (case digit
    [(0) '(0 0 0 1 1 0 1)]
    [(1) '(0 0 1 1 0 0 1)]
    [(2) '(0 0 1 0 0 1 1)]
    [(3) '(0 1 1 1 1 0 1)]
    [(4) '(0 1 0 0 0 1 1)]
    [(5) '(0 1 1 0 0 0 1)]
    [(6) '(0 1 0 1 1 1 1)]
    [(7) '(0 1 1 1 0 1 1)]
    [(8) '(0 1 1 0 1 1 1)]
    [(9) '(0 0 0 1 0 1 1)]
    [else (error "not a digit")]))

(define (r-code digit)
  (map (lambda (d)
         (- 1 d))
       (l-code digit)))

(define (g-code digit)
  (reverse (r-code digit)))

; append-map

(define (bar-encode-with structure digits)
  (apply
   append
   (map
    (lambda (type digit)
      (case type
        [(L) (l-code digit)]
        [(G) (g-code digit)]
        [(R) (r-code digit)]
        [else (error "Unknown code type in structure:" type)]))
    structure
    digits)))

(define (bar-code-EAN-8 digits)
  (ht-append
   (bar-separator)
   (bar-lines (bar-encode-with '(L L L L) (take digits 4)))
   (bar-separator)
   (bar-lines (bar-encode-with '(R R R R) (drop digits 4)))
   (bar-separator)))

(define (ean13-structure first-digit)
  (case first-digit
    [(0) '(L L L L L L)]
    [(1) '(L L G L G G)]
    [(2) '(L L G G L G)]
    [(3) '(L L G G G L)]
    [(4) '(L G L L G G)]
    [(5) '(L G G L L G)]
    [(6) '(L G G G L L)]
    [(7) '(L G L G L G)]
    [(8) '(L G L G G L)]
    [(9) '(L G G L G L)]
    [else (error "Invalid first digit for EAN-13")]))

(define (bar-code-EAN-13 digits)
  (ht-append
   (bar-separator)
   (bar-lines (bar-encode-with
               (ean13-structure (first digits))
               (take (rest digits) 6)))
   (bar-separator)
   (bar-lines (bar-encode-with
               '(R R R R R R)
               (drop digits 7)))
   (bar-separator)))

(bar-code-EAN-13 '(5 9 0 1 2 3 4 1 2 3 4 5 7))

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

;
(define (gen-list init generator)
  (let ([next (generator init)])
    (cond
      [(not next) '()]
      [else (cons
             init
             (gen-list next generator))])))

(gen-list 10 (lambda (x) (if (< x 0) #f (- x 1))))
; '(10 9 8 7 6 5 4 3 2 1 0)


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
;    обозначим (lambda (x) x) = id, lst = (cons x xs)
;    (my-map id (cons x xs)) = lst
;    (cons (id x) (my-map id xs)) = lst
;    по предположению индукции:
;    (cons x xs) = lst
;    lst = lst


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
; 2. Для непустого списка lst = (cons x xs)
; Предположение индукции:
; (my-map f (my-map g xs)) = (my-map (lambda (y) (f (g y))) xs)
; Обозначим (lambda (x) (f (g x))) = comp
; Левая часть (ЛЧ): (my-map f (my-map g (cons x xs)))
; (my-map g (cons x xs)) = (cons (g x) (my-map g xs))
; Подставим это обратно в ЛЧ:
; ЛЧ = (my-map f (cons (g x) (my-map g xs))) =
; = (cons (f (g x)) (my-map f (my-map g xs)))
; По предположению индукции:
; ЛЧ = (cons (f (g x)) (my-map comp xs))
;
; Правая часть (ПЧ): (my-map comp (cons x xs))
; ПЧ = (cons (comp x) (my-map comp xs))
; (comp x) = ((lambda (y) (f (g y))) x) = (f (g x))
; ПЧ = (cons (f (g x)) (my-map comp xs))
; Вывод:
; ЛЧ = (cons (f (g x)) (my-map comp xs)) и ПЧ = (cons (f (g x)) (my-map comp xs)).
; Следовательно, ЛЧ = ПЧ.
