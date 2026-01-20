#lang slideshow

;; Вспомогательная функция для безопасного получения n элементов
;; Возвращает два значения: список элементов (chunk) и остаток потока (rest)
(define (take-n stream count acc)
  (if (or (zero? count) (stream-empty? stream))
      (values (reverse acc) stream)
      (take-n (stream-rest stream)
              (sub1 count)
              (cons (stream-first stream) acc))))

(define (stream-chunks st n)
  (if (stream-empty? st)
      empty-stream
      (let-values ([(chunk rest) (take-n st n '())])
        (stream-cons chunk (stream-chunks rest n)))))

(for/list ([i 3] [chunk (stream-chunks (in-naturals 1) 4)]) chunk)
; '((1 2 3 4) (5 6 7 8) (9 10 11 12))

(define (stream-splits lst)
  (define (helper prefix suffix)
    (stream-cons
     (cons prefix suffix)   
     (if (null? suffix)
         empty-stream           ; Условие выхода
         (helper (append prefix (list (car suffix))) ; Рекурсивный вызов
                 (cdr suffix)))))
  (helper '() lst))

(stream->list (stream-splits '(a b c)))


