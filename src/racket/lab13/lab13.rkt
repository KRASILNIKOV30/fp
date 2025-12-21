#lang slideshow
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)

(defrel (studento name group)
  (matche (cons name group)
    [(Дарья . 2)]
    [(Максим . 1)]
    [(Кирилл . 2)]
    [(Александр . 1)]
    [(Владимир . 2)]))

(defrel (friendo name1 name2)
  (matche (cons name1 name2)
    [(Дарья . Максим)]
    [(Дарья . Александр)]
    [(Максим . Кирилл)]
    [(Максим . Владимир)]
    [(Кирилл . Александр)]
    [(Александр . Владимир)]))

(defrel (parento parent-name child-name)
  (matche (cons parent-name child-name)
    [(Мардж . Барт)]
    [(Мардж . Лиза)]
    [(Мардж . Мэгги)]
    [(Гомер . Барт)]
    [(Гомер . Лиза)]
    [(Гомер . Мэгги)]
    [(Абрахам . Гомер)]
    [(Мона . Гомер)]
    [(Жаклин . Мардж)]
    [(Жаклин . Пэтти)]
    [(Жаклин . Сельма)]
    [(Клэнси . Мардж)]
    [(Клэнси . Пэтти)]
    [(Клэнси . Сельма)]
    [(Сельма . Линг)]))

(defrel (unaryo n)
  (conde
    [(== 'z n)]
    [(fresh (m)
       (== `(s ,m) n)
       (unaryo m))]))

(defrel (direct-traino from to)
  (let [(ft (cons from to))]
    (matche ft
      [(Мытищи . Химки)]
      [(Люберцы . Мытищи)]
      [(Одинцово . Люберцы)]
      [(Красногорск . Одинцово)]
      [(Балашиха . Красногорск)]
      [(Видное . Балашиха)]
      [(Коломна . Видное)])))

(defrel (membero x xs)
  (fresh (a d)
    (== `(,a . ,d) xs)
    (conde
      [(== x a)]
      [(membero x d)])))

(defrel (groupmateso name1 name2)
  (=/= name1 name2)
  (fresh (g)
    (studento name1 g)
    (studento name2 g)))

(displayln "---groupmateso---")
(run 1 (q) (groupmateso 'Дарья 'Максим))
(run 1 (q) (groupmateso 'Дарья 'Владимир))

(defrel (ancestoro a d)
  (conde
    [(parento a d)]
    [(fresh (i)
       (parento a i)
       (ancestoro i d))]))

(defrel (relativeo x y)
  (=/= x y)
  (fresh (a)
    (ancestoro a x)
    (ancestoro a y)))

(displayln "---relativeo---")
(run 1 (q) (relativeo 'Сельма 'Пэтти))
(run 1 (q) (relativeo 'Лиза 'Линг))
(run 1 (q) (relativeo 'Лиза 'Сельма))
(run 1 (q) (relativeo 'Гомер 'Сельма))

(defrel (binaryo xs)
  (conde
    [(== xs '())]
    [(fresh (b bs)
       (== xs `(,b . ,bs))
       (conde
         [(== b 0)]
         [(== b 1)])
       (binaryo bs))]))

(displayln "---binaryo---")
(run* (x) (binaryo `(1 ,x 0)))
(run 6 (xs) (binaryo xs) (fresh (ys) (== `(1 . ,ys) xs)))
(run* (q) (fresh (d1 d2) (== q `(,d1 ,d2)) (binaryo q)))

(defrel (wordo alpha xs)
  (conde
    [(== xs '())]
    [(fresh (a as)
       (== xs `(,a . ,as))
       (membero a alpha)
       (wordo alpha as))]))

(displayln "---wordo---")
(run* (x) (wordo '(a b c) `(a ,x c)))
(run 1 (x) (wordo x '(a b c)))
(run* (x y z) (wordo '(a b) `(,x ,y ,z)))

(defrel (addo a b c)
  (conde
    [(== 'z a) (== b c)]
    [(fresh (d e)
       (== `(s ,d) a)
       (== `(s ,e) c)
       (addo d b e))]))

(defrel (doubleo n m)
  (addo n n m))

(displayln "---doubleo---")
(run 1 (q) (doubleo '(s (s z)) '(s (s (s (s z))))))
(run 1 (x) (doubleo '(s (s z)) x))
(run 1 (x) (doubleo x '(s (s (s (s z))))))
(run 1 (x) (doubleo x '(s (s (s z)))))

(defrel (leqo a b)
  (conde
    [(== a b)]
    [(fresh (c)
       (== `(s ,c) b)
       (leqo a c))]))

(displayln "---leqo---")
(run 1 (q) (leqo '(s (s z)) '(s (s (s z)))))
(run 1 (q) (leqo '(s (s (s (s z)))) '(s (s (s z)))))
(run 1 (q) (leqo q '(s (s (s z)))))

(defrel (multo a b c)
  (conde
    [(== 'z a) (== 'z c)]
    [(fresh (d e)
       (== `(s ,d) a)
       (addo b e c)
       (multo d b e))]))

(displayln "---multo---")
(run 1 (x) (multo '(s (s z)) '(s (s (s z))) x))
(run 1 (x) (multo x '(s (s (s z))) '(s (s (s (s (s (s z))))))))

(defrel (power-of-2o n m)
  (conde
    [(== 'z n) (== '(s z) m)]
    [(fresh (n-1 m-1)
       (== `(s ,n-1) n)
       (doubleo m-1 m)
       (power-of-2o n-1 m-1))]))

(displayln "---power-of-2o---")
(run 1 (q) (power-of-2o '(s (s z)) '(s (s (s (s z))))))
(run 1 (x) (power-of-2o x '(s (s (s z)))))
(run 1 (x) (power-of-2o '(s z) x))
(run 3 (x y) (power-of-2o x y))


(defrel (traino from to)
  (conde
    [(direct-traino from to)]
    [(fresh (mid)
       (direct-traino from mid)
       (traino mid to))]))

(displayln "---traino---")
(run 1 (q) (traino 'Люберцы 'Химки))
(run* (q) (traino 'Люберцы q))
(run* (q) (traino q 'Люберцы))
(run* (from to) (traino from to))

(defrel (train-patho from to path)
  (conde
    [(direct-traino from to)
     (== path `(,from ,to))]
    [(fresh (mid rest)
       (direct-traino from mid)
       (train-patho mid to rest)
       (== path `(,from . ,rest)))]))

(displayln "---train-patho---")
(run 1 (path) (train-patho 'Люберцы 'Химки path))
(run* (path) (fresh (to) (train-patho 'Люберцы to path)))
(run* (q path) (train-patho q 'Люберцы path))
(run* (from to path) (train-patho from to path))