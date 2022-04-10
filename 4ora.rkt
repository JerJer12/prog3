#lang racket
(define lista1 (cons 1(cons 2 (cons 3 empty))))
(define lista2 (list 1 2 3))
(define lista3 '(1 2 3))

(writeln lista1)
(writeln lista2)
(writeln lista3)
(writeln lista1)
(writeln (list? lista1))
(writeln (empty? lista2))
(writeln (null? lista2))

;a lista rekurzív adatszerkezet , mert a lista fejből és farokból áll és a lista farka az mindig lista
;kivéve az üres lista, az üres listának nincs se feje se farka

;(first (list 1 2 3)) = 1
;(rest (list 1 2 3)) = (list 2 3)
;(first (list 2 3)) = 2
;(rest (list 2 3)) = (list 3)
;(first (list 3)) = 3
;(rest (list 3)) = (list )
;(1 2 3) -> (2 3) -> (3) ->()
;(3 2 1) -> (2 1) -> (1) ->()
;(a b c) -> (b c) -> (c) ->()
;(alma banán) (banán) ()

;a kutya az egy kutya        -rossz
;a lista farka egy lista     -jó
;ez egy jó def, mert a lista farka egy eggyel rövidebb lista ezért ezt a rekurziót megállítja az üres lista

;rekurzív adatszerkezetet rekurzívan dolgozom fel
;azaz: "meghívom önmagam a farokra"!

;minden rekurzív függvényben kell egy bázis feltétel ami megállítja a rekurziót
;mivel 30 és mivel 27(.sor), ezért a listát feldolgozó rekurzív fg. bázis feltétela
;az, hogy a paraméterként megkapott lista üres-e?

(define (length lst)
  (cond
    ;bázis feltétel
    [(empty? lst) 0]
    ;rekurzív ágon alistát szétszedem fejre és farokra define-nal
    ;meghívom önmagam rekurzívan a farokra, ezt elnevezem farok-ertek-nek
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (length farok))
     ;konkrét példák:
     ;(length (list 5 7 6)) = 3 = 1 + 2 = (+ 1 2) = (+ 1 farok-ertek)
     ;(length (list 7 6)) = 2
     (+ 1 farok-ertek)]))

(writeln (length (list 5 7 6)))

;sum

(define (sum lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (sum farok))
     ;példa
     ;(sum (list 10 9 8)) = 27 =10 +17 = (+ fej farok-ertek)
     ;(sum (list 9 8)) =17
     (+ fej farok-ertek)]))

(writeln (sum(list 10 8 9)))

;prod lst, szorzat

(define (prod lst)
  (cond
    [(empty? lst) 1]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (prod farok))
     (* fej farok-ertek)]))

(writeln (prod(list 5 2 4)))

(writeln (even? 4))

;sum-páros lst, lst-ban lévő páros számok összege

(define (sum-páros lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (sum-páros farok))
     ;példa1
     ;(sum-páros (list 2 3 6)) = 8 = ha a fej páros akkor        fej+ farok-ertek
     ;                                              egyébként    farok-ertek
     ;(sum-páros (list 3 6)) = 6
     ;példa2:
     ;(sum-páros (list 3 3 6)) = 6 = (if (even? fej) (+ fej farok-ertek) farok-ertek)
     ;(sum-páros (list 3 6)) = 6
     (if (even? fej) (+ fej farok-ertek) farok-ertek)]))

(writeln (sum-páros (list 2 3 6)))
(writeln (sum-páros (list 3 3 6)))

;sum-páratlan

(writeln (odd? 5))

(define (sum-páratlan lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (sum-páratlan farok))
     (if (odd? fej) (+ fej farok-ertek) farok-ertek)]))

(writeln (sum-páratlan (list 2 3 6)))
(writeln (sum-páratlan (list 3 3 6)))

;max-v2 lst, az lst-ben lévő legnaygobb szám
;üres listának nincs legnagyobb értéke, ott a not a number-t adunk vissza
;egy elemű lista esetén: (max-v2 (list 5)) = 5

;hint1: not a number: +nan.0
;hint2: legyen 3 ág: ha üres, ha egy elemű és else
;hint3: (= 1 (length lst))

(define (max-v2 lst)
  (cond
    [(empty? lst) +nan.0]
    [(= 1 (length lst)) (first lst)]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (max-v2 farok))
     (if (> fej farok-ertek) fej farok-ertek)]))

(writeln (max-v2 (list 2 8 5)))

;min-v2 lst, visszaadja a lst-ben lévő legkisebb számot

(define (min-v2 lst)
  (cond
    [(empty? lst) +nan.0]
    [(= 1 (length lst)) (first lst)]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (min-v2 farok))
     (if (< fej farok-ertek) fej farok-ertek)]))

(writeln (min-v2 (list 6 2 8 5)))

;append-v2 lst1 lst2, egymás után fűzi a 2 listát
;példa: (append-v2 (list 2 5 1) (list 1 8 10)) = (list 2 5 1 1 8 10)
; (append-v2 (list ) (list  1 8 10)) = (list 1 8 10)
;hint: cons-ot használni kell

;cons szintaxisa: (cons fej farok), ahol a farok egy lista
