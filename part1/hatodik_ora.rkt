#lang racket

(define (sum lst)
  (cond
    [(empty? lst) 0] ;nem reku ag
    [else             ;reku ag
     (define fej (first lst)) 
     (define farok (rest lst))
     (define farok-ertek (sum farok))
     (+ fej farok-ertek)]))
(println (sum (list 1 2 3)))
;a sum reku agan mi az utolso hivas?
; +
; mivel az utolso hivas nem rekurziv hivas ezert ez nem farok rekurzio
;akkor mondjuk egy fg-re hogy farok reku
;ha a reku ag utolso hivasa a rekurziv hivas
;ahhoz hogy atirjam farok rekurzivre ahhoz AKKUMULÁTORT kell hasznalni
;az AKKU-ban gyujtom a reszeredmenyt!!!
;ha vege a rekunak akkor visszaadom az AKKU-t
;azert erdemes farok reku fg-t irni, mert abbol a rendszer ciklust fordit, azzaz nem kell verem
;Ez egy deklarativ nyelv, azaz a keretrendszer talalja ki, hogy hogyan hajtsa vegre
;ciklust forditson belole?
;elos lepes: csinalok egy 1 parameteres sum-ot, ami meghivja a 2 parameterest
(define (sum1 lst) (sum2 0 lst))
;az akku kezdo erteke az az amit a sima fg vissza ures listara!
(define (sum2 akku lst)
  (cond
    ;ha ures a lista, innen vesszuk eszre hogy vege a rekunak akkor az akkuban van a veg eredmeny tehat az akkut kell visszaadni
    [(empty? lst) akku]
    [else
     ;ugyan ugy fej farok, de nem lehet farok ertek mert az utolso hivasnak kell lennie a rekurzio hivasnak
     ;e helyett lesz az uj-akku
     (define fej (first lst))
     (define farok (rest lst))
     ;okolszabaly ugy epitem az akkut mint sima esetben a visszateresi erteket azaz lenyegeben a farok-ertek helyett hasznalom
     ;sokszor erdemes peldat gyartani hogy meggyozd magad mit kell ide irni sokszor olyan peldat kell csinalni hogy a lista egy elemu
     ;hogy lassuk mit kell a fejjel csinal
     ;pl: (sum2 0 (list 10)) = 10 = 10 + 0 = fej + akku
     (define uj-akku (+ fej akku))
     ;szerencsere ez konnyu mindig ezt kell irni:
     ;hivd meg magad a farokra csak uj-akku-val
     (sum2 uj-akku farok)
     ])) ;2 param az akku es a lista
;mi az utolso hivas? sum2 azaz ez farok rekurziv
(println (sum1 (list 4 6 5)))
(define (prod lst)
  (cond
    [(empty? lst) 1]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (prod farok))
     (* fej farok-ertek)]))
;feladat prod1,prod2
(define (prod1 lst) (prod2 1 lst))
(define (prod2 akku lst)
  (cond
    [(empty? lst) akku]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define uj-akku (* fej akku))
     (prod2 uj-akku farok)]))
(println (prod1 (list 1 2 3)))
(define (my-max lst)
  (cond
    [(empty? lst) +nan.0]
    [(empty? (rest lst)) (first lst)]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (my-max farok))
     (if (> fej farok-ertek) fej farok-ertek)
     ]))
(println (my-max (list 1 2 3 69)))
(define (max1 lst) (if (empty? lst) +nan.0 (max2 (first lst) lst)))
(define (max2 akku lst)
  (cond
    [(empty? (rest lst)) (first lst)]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define uj-akku (if(> fej akku) fej akku))
     (max2 uj-akku farok)]))
(println (max1 (list 1 2 3 4 5)))
(define (lista->halmaz lst)
  (cond
    [(empty? lst) empty]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (lista->halmaz farok))
     (if (member fej farok) farok-ertek (cons fej farok-ertek))
     ]))
(println (lista->halmaz (list 1 1 2 2 3 3)))
(define (lth1 lst) (lth2 empty lst))
(define (lth2 akku lst)
  (cond
    [(empty? lst) akku]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define uj-akku (if (member fej akku) akku (cons fej akku)))
     (lth2 uj-akku farok)
     ]))
(println (lth1 (list 1 1 22 22 22 33 44 55 44 66)))
(define (myinter A B)
  (cond
    [(empty? B) empty]
    [else
     (define Bfej (first B))
     (define Bfarok (rest B))
     (define farok-ertek (myinter A Bfarok))
     (if (xor (member Bfej A) (member Bfej Bfarok)) (cons Bfej farok-ertek) farok-ertek)]))
(println (myinter (list 1 2 3) (list 2 3 4)))
(define (inter1 A B) (inter2 empty A B))
(define (inter2 akku A B)
  (cond
    [(empty? B) akku]
    [else
     (define Bfej (first B))
     (define Bfarok (rest B))
     (define uj-akku (if (member Bfej A) (cons Bfej akku) akku))
     (inter2 uj-akku Bfarok A)
     ]))
(println (inter1 (list 1 2 3) (list 3 4 5)))
;teszt:
;string-append,number->string
(define k 5)
(define kk 15)
(println (string-append (number->string k) " < " (number->string kk)))
;if,max,min
;listakezeles,sum,prod
;halmazok,member,uni,intersection
;farok reku, akkumulator hasznalata