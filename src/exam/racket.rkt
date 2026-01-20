#lang slideshow

(define (take-n stream count acc)
  (cond
    [(or (= count 0) (stream-empty? stream))
     (values acc stream)]
    [else
     (take-n (stream-rest stream)
             (- count 1)
             (append acc (list (stream-first stream))))]))

(define (stream-chunks st n)
  (cond
    [(stream-empty? st) empty-stream]
    [else
     (let-values ([(chunk rest) (take-n st n '())])
       (stream-cons chunk (stream-chunks rest n)))]))

(for/list ([i 3] [chunk (stream-chunks (in-naturals 1) 4)]) chunk)


(define (stream-splits lst)
  (define (helper prefix suffix)
    (stream-cons
     (cons prefix suffix)
     (cond
       [(null? suffix) empty-stream]
       [else
        (helper (append prefix (list (first suffix)))
                (rest suffix))])))
  (helper '() lst))


(stream->list (stream-splits '(a b c)))





