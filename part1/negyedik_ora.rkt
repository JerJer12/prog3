#lang racket
;a lista rekurziv adatszerkezet, mert a lista farka lista
;a lista farkat a (rest x) hivassal kapom meg
;rekurziv adatszerkezetet rekurzivan dolgozok fel
;ez a lista eseten azt jelenti, hogy meghivom onmagamat a lista farkara
;pelda:(my-length (list  12 8 5)) = 3 = 2 + 1
;(my-length (8 5)) = 2
(define (my-length szkk)
  (cond
    [(empty? szkk) 0]
    [else (+ 1 (my-length (rest szkk)))]))
(println (my-length (list 12 8 5)))
;(my-sum (list 12 8 2)) = 22 = 12 + (my-sum(list 8 2))
(define (my-sum lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst) (my-sum (rest lst)))]))
(println (my-sum (list 12 8 2)))
;feladat: my-prod a listaban levo elemek szorzata
;megegyezes szerint az ures listara 1-et ad vissza
(define (my-prod lst)
  (cond
    [(empty? lst) 1]
    [else (* (first lst) (my-prod (rest lst)))]))
(println (my-prod (list 3 4 5)))
;my-last viszaadja az utolsot
;ha ures listara hivom, akkor +nan.0-t adja, ami azt jelenti, hogy not a number
;hint: ugy nezem meg,hogy egy elemu a lista , hogy a farka ures-e
(define (my-last lst)
  (cond
    [(empty? lst) +nan.0]
    [(empty? (rest lst)) (first lst)]
    [else (my-last (rest lst))]))
(println (my-last (list 5 15 25)))
;feladat: last-but-one utolso elotti
;uresre +nan.0
;egy elemre +nan.0
(define (last-but-one lst)
  (cond
    [(empty? lst) +nan.0]
    [(empty? (rest lst)) +nan.0]
    [(empty? (rest(rest lst))) (first lst)]
    [else (last-but-one (rest lst))]))
(println (last-but-one (list 5 15 25)))
;my-max: visszadja a listaban levo legnagyobb szamot
;ha ures a lista, akkor +nan.0
;ha egy elemu akkor az az egy elem, pl: (my-max (list 5)) = 5
;egyebkent: pl: (my-max (list 5 15 25)) if(> 5 (my-max (rest lst)))...
(define (my-max lst)
  (cond
    [(empty? lst) +nan.0]
    [(empty? (rest lst)) (first lst)]
    [else
     (define fej (first lst))
     (define farok-ertek (my-max (rest lst)))
     (if(> fej farok-ertek) fej farok-ertek)]))
(println (my-max (list 234 232 123 2111 34242)))
;dont repeat yourself (dry)
;my-sum2 irja meg ugy hogy legyen define fej es farok-ertek
(define (my-sum2 lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok-ertek (my-sum2 (rest lst)))
     (+ fej farok-ertek)]))
(println (my-sum2 (list 324 2342 131 3131)))