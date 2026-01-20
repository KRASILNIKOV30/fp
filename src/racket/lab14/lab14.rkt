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

(defrel (appendo xs ys xys)
  (matche xs
          [() (== ys xys)]
          [(,z . ,zs)
           (fresh (zys)
                  (== (cons z zys) xys)
                  (appendo zs ys zys))]))

(defrel (lengtho xs n)
  (matche xs
          [() (== n (build-num 0))]
          [(,y . ,ys)
           (fresh (m)
                  (lengtho ys m)
                  (pluso (build-num 1) m n))]))

(defrel (inserto x before after)
  (conde
   [(== before '()) (== after `(,x))]
   [(fresh (b rest-a)
           (== before `(,b . ,rest-a))
           (== after `(,x ,b . ,rest-a)))]
   [(fresh (b rest-b rest-a)
           (== before `(,b . ,rest-b))
           (== after `(,b . ,rest-a))
           (inserto x rest-b rest-a))]))

(displayln "---inserto---")
(run* (x xs) (inserto x xs '(1 2 3)))
(run* (x xs) (inserto x xs '(a b a c)))

(defrel (anagramo xs ys)
  (conde
   [(== xs '()) (== ys '())]
   [(fresh (x xs-rest ys-rest)
           (== xs `(,x . ,xs-rest))
           (membero x ys)
           (inserto x ys-rest ys)
           (anagramo xs-rest ys-rest))]))

(displayln "---anagramo---")
(run 1 (q) (anagramo '(d o r m i t o r y) '(d i r t y r o o m)))

(defrel (subseqo sub whole)
  (conde
   [(== sub '())]
   [(fresh (x xs rest)
           (== sub `(,x . ,xs))
           (== whole `(,x . ,rest))
           (subseqo xs rest))]
   [(fresh (x rest)
           (== whole `(,x . ,rest))
           (subseqo sub rest))]))

(displayln "---subseqo---")
(run* (q) (subseqo '(2 4 5) '(1 2 3 4 5 6)))
(run* (xs) (subseqo xs '(1 2 3)))
(run* (xs)
      (fresh (a b c)
             (== xs `(,a ,b ,c))
             (subseqo '(1 2) xs)))

(defrel (prefixo needle lst)
  (conde
   [(== needle '())]
   [(fresh (x xs y ys)
           (== needle `(,x . ,xs))
           (== lst `(,x . ,ys))
           (prefixo xs ys))]))

(defrel (searcho needle haystack pos)
  (fresh (pre rest)
         (appendo pre rest haystack)
         (prefixo needle rest)
         (lengtho pre pos)))


(displayln "---searcho---")
(run* (pos) (searcho '(a b a) '(c a b a b a d) pos))
(run* (needle) (searcho needle '(c a b a b a d) (build-num 5)))
(run* (needle pos)
      (fresh (x)
             (== needle `(a ,x a))
             (searcho needle '(c a b a b a d) pos)))

(defrel (not-prefixo prefix lst)
  (=/= prefix '())
  (conde
   [(== lst '())]
   [(fresh (p h ps hs)
           (== prefix `(,p . ,ps))
           (== lst `(,h . ,hs))
           (=/= p h))]
   [(fresh (p h ps hs)
           (== prefix `(,p . ,ps))
           (== lst `(,p . ,hs))
           (not-prefixo ps hs))]))

(displayln "---not-prefixo---")
(run* (xs) (not-prefixo '(a b) '(a b r a b a)))
(run* (xs) (not-prefixo '(a b a) '(a b r a b a)))

(defrel (not-sublisto sub lst)
  (conde
   [(== sub '()) (=/= lst '())]
   [(fresh () (not-prefixo sub lst)) (== lst '())]
   [(fresh (h t)
           (== lst `(,h . ,t))
           (not-prefixo sub lst)
           (not-sublisto sub t))]))

(displayln "---not-sublisto---")
(run* (xs) (not-sublisto '(a b a) '(a b r a b a)))
(run* (xs) (not-sublisto '(a b c) '(a b r a b a)))

(defrel (replaceo old new old-whole new-whole)
  (conde
   [(== old-whole '()) (== new-whole '())]
   [(fresh (suffix rest-new)
           (appendo old suffix old-whole)
           (appendo new rest-new new-whole)
           (replaceo old new suffix rest-new))]
   [(fresh (x xs ys)
           (== old-whole `(,x . ,xs))
           (== new-whole `(,x . ,ys))
           (replaceo old new xs ys))]))

(displayln "---replaceo---")
(run* (new-whole) (replaceo '(a b) '(N E W) '(a b r a b a) new-whole))
(run* (new-whole) (replaceo '(a a) '(x y) '(a a a a) new-whole))

(defrel (replace-allo old new old-whole new-whole)
  (conde
   [(== old-whole '()) (== new-whole '())]
   [(fresh (suffix rest-new rest)
           (appendo old suffix old-whole)
           (appendo new rest-new new-whole)
           (replace-allo old new suffix rest-new))]
   [(fresh (x xs ys)
           (== old-whole `(,x . ,xs))
           (== new-whole `(,x . ,ys))
           (not-prefixo old old-whole)
           (replace-allo old new xs ys))]))

(displayln "---replace-allo---")
(run* (new-whole) (replace-allo '(a b) '(N E W) '(a b r a b a) new-whole))
(run* (new-whole) (replace-allo '(a a) '(x y) '(a a a a) new-whole))