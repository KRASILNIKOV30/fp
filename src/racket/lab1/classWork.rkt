#lang slideshow

(define (second-to-last lst)
  (define (helper lst current)
    (cond
      [(empty? current) (first lst)]
      [else (helper (rest lst) (rest current))]))
  (helper lst (rest (rest lst))))

(define (my-reverse lst)
  (define (helper lst current)
    (cond
      [(empty? lst) current]
      [else (helper
             (rest lst)
             (cons (first lst) current))]))
  (helper lst empty))

(define (my-flatten lst)
  (define (helper lst current)
    (cond
      [(empty? lst) current]
      [else (cond
              [(cons? (first lst)) (helper (append (first lst) (rest lst)) current)]
              [else (helper (rest lst) (append current (list (first lst))))])]))
  (helper lst empty))

(define (my-encode lst)
  (define (helper lst ch counter result)
    (cond
      [(empty? lst) (append result (list (cons ch (list counter))))]
      [else
       (cond 
         [(eq? ch (first lst)) (helper (rest lst) ch (+ 1 counter) result)]
         [else (helper (rest lst) (first lst) 1 (append result (list (cons ch (list counter)))))])]))
  (helper (rest lst) (first lst) 1 empty))

(define (my-decode lst)
  (define (helper lst ch counter result)
    (cond
      [(empty? lst)
       (cond
         [(= counter 0) result]
         [else
          (helper lst ch (- counter 1) (append result (list ch)))])]
      [else
       (cond 
         [(= counter 0)
          (helper (rest lst) (first (first lst)) (second (first lst)) result)]
         [else
          (helper lst ch (- counter 1) (append result (list ch)))])]))
  (helper (rest lst) (first (first lst)) (second (first lst)) empty))

(define (my-split lst index)
  (define (helper lst fst index)
    (cond
      [(= index 0) (append (list fst) (list lst))]
      [else (helper (rest lst) (append fst (list (first lst))) (- index 1))]))
  (helper lst empty index))

(my-split '(a b c d e f g h i k) 3)
