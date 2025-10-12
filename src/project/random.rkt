#lang slideshow
(require racket/generator)

(define m 2147483648)
(define a 1103515245)
(define c 12345)

(struct rng (seed))

(define (make-rng n)
  (rng n))

(define (next-rng g)
  (rng (modulo (+ (* a (rng-seed g)) c) m)))

;(rng-seed (next-rng (make-rng 123)))

(define (random-integer i j)
  (lambda (g)
    (let ([next-g (next-rng g)])
      (cons
       (+ i (floor (* (- j i) (/ (rng-seed next-g) m))))
       next-g))))

(define (stream-random fn g)
  (let ([res (fn g)])
    (stream-cons
     (car res)
     (stream-random fn (cdr res)))))

(define (sample fn [n 5] [g (make-rng 709)])
  (let ([res (fn g)])
    (cond
      [(zero? n) empty]
      [else
       (cons
        (car res)
        (sample fn (sub1 n) (cdr res)))])))                    

(define (map-random pred fn)
  (lambda (g)
    (let ([res (fn g)])
      (cons
       (pred (car res))
       (cdr res)))))

(define random-bool
  (map-random even? (random-integer 1 100)))

(define random-coin-flip
  (map-random
   (lambda (p)
     (cond
       [p 'opёл]
       [else 'решка]))
   random-bool))

(define random-dice-roll
  (random-integer 1 7))

(define (frequency-count lst)
  (map (lambda (group)
         (cons (car group) (length group)))
       (group-by identity lst)))

(define (random-cons random-fn1 random-fn2)
  (lambda (g)
    (let* ([res1 (random-fn1 g)]
           [val1 (car res1)]
           [g1 (cdr res1)])
      (let* ([res2 (random-fn2 g1)]
             [val2 (car res2)]
             [g2 (cdr res2)])
        (cons (cons val1 val2) g2)))))

(define (random-if random-bool-fn random-then-fn random-else-fn)
  (lambda (g)
    (let* ([res-bool (random-bool-fn g)]
           [bool-val (car res-bool)]
           [g1 (cdr res-bool)])
      (cond
        [bool-val (random-then-fn g1)]
        [else (random-else-fn g1)]))))

(define (random-constant value)
  (lambda (g)
    (cons value g)))

(define (random-choice random-then-fn random-else-fn)
  (random-if random-bool random-then-fn random-else-fn))

(define (random-item lst)
  (when (empty? lst)
    (error 'random-item "нельзя использовать с пустым списком"))
  (map-random
   (lambda (index) (list-ref lst index))
   (random-integer 0 (length lst))))

(define (random-list-of-size random-fn size)
  (lambda (g)
    (let-values ([(final-acc final-g)
                  (for/fold ([acc '()] [current-g g])
                            ([i (in-range size)])
                    (let* ([res (random-fn current-g)]
                           [val (car res)]
                           [next-g (cdr res)])
                      (values (cons val acc) next-g)))])
      (cons final-acc final-g))))

(define (random-bind random-val-fn func)
  (lambda (g)
    (let* ([res1 (random-val-fn g)]
           [val1 (car res1)]     
           [g1 (cdr res1)])       
      ((func val1) g1))))

(define (random-list fn)
  (random-bind
   (random-integer 0 10)
   (lambda (n)
     (random-list-of-size fn n))))

(define-syntax let*/random
  (syntax-rules ()
    [(_ () body)
     (random-constant body)]
    [(_ ([id expr] bindings ...) body)
     (random-bind
      expr
      (lambda (id)
        (let*/random (bindings ...) body)))]))

(define (round-to dp num)
  (let ([factor (expt 10 dp)])
    (/ (round (* num factor))
       (exact->inexact factor))))

(define (random-real i j)
  (map-random
   (lambda (n)
     (round-to 3 (+ i (* (- j i) (/ n m)))))
   (random-integer 0 m)))

(define random-point-in-unit-square
  (random-cons
   (random-real -1 1)
   (random-real -1 1)))

(define (in-circle? point)
  (let ([x (car point)]
        [y (cdr point)])
    (<= (+ (* x x) (* y y)) 1)))

(define (monte-carlo-pi n)
  (let ([points (sample random-point-in-unit-square n)])
    (let ([inside-count (length (filter in-circle? points))])
      (* 4.0 (/ inside-count n)))))

(struct property (random-value predicate))

(define reverse-reverse-is-identity
  (property
   (random-list (random-integer 0 100))
   (lambda (xs) (equal? xs (reverse (reverse xs))))))

(define real-addition-is-associative
  (property
   (random-list-of-size (random-real -1000 1000) 3)
   (lambda (nums)
     (let ([x (first nums)]
           [y (second nums)]
           [z (third nums)])
       (= (+ x (+ y z))
          (+ (+ x y) z))))))

(define (find-counterexample prop [n 1000])
  (let ([generator (property-random-value prop)]
        [predicate (property-predicate prop)])
    (let loop ([tries-left n]
               [current-g (make-rng 709)])
      (cond
        [(zero? tries-left) '()]
        [else
         (let* ([res (generator current-g)] 
                [val (car res)]
                [next-g (cdr res)])
           (cond
             [(predicate val)
              (loop (sub1 tries-left) next-g)]               
             [else (list val)]))]))))

(define-syntax (forall/property stx)
  (syntax-case stx ()
    [(_ ([var gen] ...) body ...)
     #`(property
        (let*/random ([var gen] ...)
                     (list (list 'var '= var) ...))
        (lambda (generated-bindings)
          (let ([var (caddr (assoc 'var generated-bindings))] ...)
            body ...)))]))

(find-counterexample
 (forall/property ([x (random-real 0 1)]
                    [y (random-real 0 x)]
                    [z (random-real 0 y)])
                   (> x (+ y z))))