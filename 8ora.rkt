#lang racket
(define/contract (összead A B)
  (-> number? number? number?)
  (+ A B))

(writeln (összead 5 3))

(define/contract (összead-sztring-szám s x)
  (-> string? number? number?)
  (+ (string-length s) x))

(writeln (összead-sztring-szám "alma" 3))

(define/contract (inc x)
  (-> (and/c integer? (>/c 0)) number?)
  (+ x 1))

;(define/contract (inc x)
;  (-> (and/c integer? (>/c 0)) any/c)
;  (+ x 1))

(writeln (inc 5))

(define/contract (dec-v1 x)
  (-> (and/c number? (>/c 0)) number?)
  (- x 1))

(define/contract (dec-v2 x)
  (->i ([x (and/c number? (>/c 0))])
       [result (x) (and/c number? (</c x))])
  (- x 1))

;összead-kicsi-nagy a b , az a-nak kissebnek kell lennie, mint a b-nek

(define/contract (ad-k-n a b)
  (->i ([a number?]
        [b (a) (>/c a)])
       [result number?])
  (+ a b))

(writeln (ad-k-n 3 8))

(define/contract (ki-n-k a b)
  (->i ([a number?]
        [b (a) (</c a)])
       [result number?])
  (- a b))

(writeln (ki-n-k 8 3))

;Magasabb rendű függvények

(writeln (filter number? (list 1 "alma" 2 "körte" 5)))
(writeln (filter even? (list 1 2 3 4 5 6)))
(writeln (filter even? (filter number? (list 1 "alma" 2 3 4 5 6))))

(writeln (map sqrt (list 1 4 9 121 22)))
(writeln (map inc (list 1 2 3 5)))
(writeln (map sqrt (map string-length (list "alma" "körte" "barack"))))

(define (my-filter f lst)
  (cond
    [(empty? lst) (list )]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (my-filter f farok))
     (if (f fej)
         (cons fej farok-ertek)
         farok-ertek)]))

(writeln (my-filter number? (list 1 "alma" 2 "körte" 5)))


(define (my-map f lst)
  (cond
    [(empty? lst) (list )]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (my-map f farok))
     (define uj-fej (f fej))
     (cons uj-fej farok-ertek)]))

(writeln (my-map sqrt (list 1 4 9 121 22)))


;my-map contract

(define/contract (ma-mapc f lst)
  (-> (-> any/c any/c) list? list?)
  (cond
    [(empty? lst) (list )]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (my-map f farok))
     (define uj-fej (f fej))
     (cons uj-fej farok-ertek)]))


;my-filter contract

(define/contract (my-filterc f lst)
  (-> (-> any/c boolean?) list? list?)
  (cond
    [(empty? lst) (list )]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (my-filter f farok))
     (if (f fej)
         (cons fej farok-ertek)
         farok-ertek)]))


