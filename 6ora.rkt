#lang racket
;6.óra

;Rekurzió vs ciklusok

;a két programozói módszer ugyanolyan erősségű,
;azaz amit meg tudok oldani rekurzióval azt meg tudom oldani ciklusasl is
;és fordítva
;de a rekurzió magasabb absztrakciós szinten áll,
;ezért gyakran könnyebb megérteni
;de a rekurzióhoz kell verem, mert minden fg. hívás előtt kell
;menteni a lokális vátozokat és a regiszterek értékét
;ezért a ciklus gyorsabb lesz és keveebb memoriát használ
;van egy arany középút
;olyan rekurzió, amelyből ciklust lehet fordítani
;ezt farok reurziónak hívják

;FAROK REKURZIÓ
;def (farok reku fg): Azt mondjuk, hogy az f fg. farok rekurzív <>
;1. az f fg rekurzív és
;2. az fg törzsében a rekurzív hívás az utolsó helyen áll

;f(x) = if (x==1) 1 else g(x) + f(x-1);
;mivel az utolsó hívás az összeadás, ezért nem farok rekurzív

;f(x,y) = if (x==1) 1+y else f(x-1,g(x));
;mivel az utolsó hívás a rekurzív hívás,
;ezért ez farok rekurzív

;ahhoz, hogy farok rku fg-t tudjuk írni
;gyakran akkumulátort kell használni
; - az akkumulátor egy plusz paraméter
; - az akku-ban gyűjtőm a részeredményekért, ezért
; - a nem reku ágon az akku-ban lesz a végeredmény
; - az akku kezdő értéke az a sima reku fg
;   nem reku ágának visszatérési értéke
; - az akku2-t úgy számolom, hogy a sima reku fg
; kiszámítási szabályát átmásolom és a faro-érték helyére,
;azt írom, hogy akku


;sima reku sum
(define (sum lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (sum farok))
     (+ fej farok-ertek)]))

(writeln (sum(list 1 5 4)))

;átírás akkumulátorosan

(define (sum2 lst) (farok-reku-sum 0 lst))

(define (farok-reku-sum akku lst)
  (cond
    [(empty? lst) akku]     ;ez nem rekurzív ág)
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define akku2 (+ fej akku))
     (farok-reku-sum akku2 farok)]))

(writeln (sum2 (list 1 5 4)))

;sima sum
;(+ 1 (+ 5 (+4 0)))

;sum2
;(+ 4 (+ 5 (+ 1 0)))

;prod
(define (prod lst)
  (cond
    [(empty? lst) 1]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (prod farok))
     (* fej farok-ertek)]))

(writeln (prod (list 1 5 4)))


(define (prod2 lst) (farok-reku-prod 1 lst))

(define (farok-reku-prod akku lst)
  (cond
    [(empty? lst) akku]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define akku2 (* fej akku))
     (farok-reku-prod akku2 farok)]))

(writeln (prod2 (list 1 5 4)))


;sima unio
;minden halmaz müveletet az eleme-e fg-re vezetek vissza
;ami angolul member
;(member x lista) visszaadja, hogy x benne van-e a listában

(define (unio A B)
  (cond
    [(empty? A) B]
    [else
     (define fejA (first A))
     (define farokA (rest A))
     (define farok-ertek (unio farokA B))
     (if (member fejA B) farok-ertek
                            (cons fejA farok-ertek))]))

(writeln (unio (list 1 2 3) (list 2 3 4)))

;farok-reku unio megírása
;a titok az, az hogy akku2-t úgy számolom ki, hogy farok-ertek helyett azt írom, hogy akku
;1. lépés: írok egy olyan 2 paraméterest ami, hív egy 3 paraméterest
;2. lépés: akku a kezdő értéke

(define (unio2 A B) (farok-reku-unio B A B))

(define (farok-reku-unio akku A B)
  (cond
    [(empty? A) akku]
    [else
     (define fejA (first A))
     (define farokA (rest A))
     (define akku2 
     (if (member fejA B) akku
                            (cons fejA akku)))
       (farok-reku-unio akku2 farokA B)]))

(writeln (unio2 (list 1 2 3) (list 2 3 4)))
 
