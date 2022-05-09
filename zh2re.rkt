#lang racket
(define/contract (inc n)
    (-> (and/c integer? (>/c 0)) integer?)
    (+ n 1))
(writeln (inc 5))

;(->i ([param1 előfeltétel]
;      [param2 (param1) előfeltétel]) 
;      [result utófeltétel])

(define/contract (inc2 n)
    (->i ([n (>/c 0)]) [result integer?])
    (+ n 1))
(writeln (inc2 5))

(define/contract (add-nagy-kicsi a b)
    (->i ([a (b) (>/c b)] [b integer?]) [result integer?])
    (+ a b))
(writeln (add-nagy-kicsi 6 5))

; ird be meg az add kicsi nagy
(define/contract (add-kicsi-nagy a b)
    (->i ([a (b) (</c b)] [b integer?]) [result integer?])
    (+ a b))
(writeln (add-kicsi-nagy 2 3))

(define (nagyobbmint5 x)
    (and (integer? x) (> x 5)))

; magasabb rendű függvények. def: azt mondjuk hogy az f függvény magasabb rendű akor és csak akkor ha van függvény típusú paramétere

(writeln (filter even? (filter number? (list 1 "alma" 2 6))))
(writeln (filter (and integer? (>/c 5)) (list 1 "alma" 2 6)))
(writeln (filter nagyobbmint5 (list 1 "alma" 2 6)))

(writeln (map sqrt (list 1 4 9)))
(writeln (map (lambda (x) (+ 1 x)) (list 1 4 9)))
(writeln (map inc (list 1 4 9)))

(define/contract (map2 f lista)
    (-> (-> any/c any/c) list? list?)
    (match lista
        [(list ) (list )]
        [(cons fej farok)
            (define farokertek (map2 f farok))
            (cons (f fej) farokertek)]))

(writeln (map2 inc (list 1 4 9)))

(define/contract (my-filter f lst)
    (-> (-> any/c boolean?) list? list?)
    (match lst
        [(list ) (list )]
        [(cons fej farok) (define farokertek  (my-filter f farok))
        (if (f fej) (cons fej farokertek) farokertek)]))
(writeln (my-filter even? (list 1 2 3)))

;részleges függvény, currying

;(require relation/function)

; szeretnék létrehozni egy olyan függvényt ami használható részleges függvényként
; azaz nem muszáj megmondanom az összes paraméterét
;(define/function (add a b) (+ a b))

;(define paradd (add 5)) létrejön egy add5 nevű függvény ami 1 paraméteres
;(writeln (add 5 3))
;(writeln (paradd 3))

; (define (szor1 a b) (* a b)) - int x int -> int
; (define/function (szor2 a b) (* a b)) - int -> (int -> int)
