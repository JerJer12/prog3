#lang racket
;ismetles listat rekurzivan dolgozok fel!
;meghivom onmagam a lista farkara!!!
(define (my-sum param1)
  (cond
    [(empty? param1) 0]
    [else
     (define fej (first param1))
     (define farok (rest param1))
     (define farok-ertek (my-sum farok))
     (+ fej farok-ertek)]
    ))
(println (my-sum (list 1 2 3)))
;szures, kapok egy listat es abbol egyelore kiszurom a parosokat egy uj listaba
;paros szuro
;paros?(4) = true
;paros?(3) = false
(define (paros? ertek)(= 0 (modulo ertek 2))) ;return ertek % 2 == 0
(println (paros? 3))
;paros szuro egy parameteres, ami egy lista visszaterese egy lista
;ismetles:
;a cons segitsegevel rakom ossze a fejet es a farkat egy listava
;pl (cons 1 (list 2 3)) = (1 2 3)
;hint1: ha ures a lista akkor a kimenet is ures
;hint2: ha nem ures, akkor megnezem a fejet hogy paros e ha igen megtartom azaz marad a fej ha nem eldobom a fejet azaz csak a farok-erteket adom vissza
(define (szuro feltetel? lista)
  (cond
    [(empty? lista) empty]
    [else
     (define fej (first lista))
     (define farok (rest lista))
     (define farok-ertek (szuro feltetel? farok))
     (if (feltetel? fej) (cons fej farok-ertek) farok-ertek)]))
;szurjuk le a parosokat
(println (szuro paros? (list 1 2 3 4 5 6 7 8)))
;szurjuk le a 3 es 5 kozti szamokat
;szurjuk le az ottel oszthatokat
;vegyuk eszre hogy a feltetel is parameter
;a fuggveny is ertek akar atadhato parameter kent
(define (inter35? x)(and (>= x 3) (<= x 5)))
(println (szuro inter35? (list 32 4 56 5)))
(define (div5? x) (= 0 (modulo x 5)))
(println (szuro div5? (list 35 40 121 5 10)))
;member - benne van-e
;member - eleme-e
;(member x lista) -igazat ad ha x benne van a listaban
;                 -egyebkent hamisat
;racket-ben ami nem hamis az igaz!!!!
(println (member 25 (list 1 5 8 10)))
;lista->halmaz kap egy listat visszaad egy listat de abban mar nem lehet ismetlodes viszont a sorrend mindegy
;hint1: lista->halmaz(ures) = ures
;hint2: ha a fej benne van a farokban, akkor a fejet eldobom egyebkent megtartom
;pl: lista->halmaz((1 2 1 2 3 4 1)) = (1 2 3 4) = (2 3 4 1) = ...
;hint3: eldob=farok-ertek
;hint4: megtart = ....
(define (halmaz lst)
  (cond
    [(empty? lst) empty]
    [else
    (define fej (first lst))
    (define farok (rest lst))
    (define farok-ertek (halmaz farok))
    (if (member fej farok) farok-ertek (cons fej farok-ertek))
     ]))
(println (halmaz (list 1 2 1 2 3 4 4 4 5 1 2 3)))
;unio = append majd lista->halmaz hivas
;unio(a,b) = append(a,b)
;append pelda
(println (halmaz(append (list 1 2 3) (list 2 3 4))))
;unio((list 1 2 3) (list 2 3 4)) = (1 2 3 4)
(define (unio param1 param2) (halmaz (append param1 param2)))
(println (unio (list 1 2 3) (list 2 3 4)))
;unio 2 rekurzivan
;otlet vagy a lista1-et, vagy a lista2-ot rekurzivan elfogyasztom
;elfogyasztom a lista1
;unio2(ures,lista2) = lista2
;ha az elso lista nem ures, akkor megnezem hogy a fej szerepel e a lista2-ben
;ha igen akkor eldobom
;ha nem akkor megtart
(define (unio2 param1 param2)
  (cond
    [(empty? param1) param2]
    [else
     (define fej (first param1))
     (define farok (rest param1))
     (if(member fej param2) (unio2 farok param2)(cons fej (unio2 farok param2)))]))
(println (unio2 (list 4 5 6) (list 5 6 7)))
;metszet
(define (intersect param1 param2)
  (cond
    [(empty? param1) '()]
    [else
     (define fej (first param1))
     (define farok (rest param1))
     (if (member fej param2) (cons fej (intersect farok param2)) (intersect farok param2))
     ]))
(println (intersect (list 1 2 3 69) (list 2 3 4 69)))