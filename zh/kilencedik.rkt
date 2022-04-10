#lang racket
;szerzodes alapu programozas
;Design by Contract / DoC
;OOP vonatkozasok:
;http://aries.ektf.hu/~gkusper/ProgTechKonyv.v.1.3.2.pdf
;ebben a szemleletben a hivo es a hivott kozott van egy szerzodes:
;ha a metodus elofelteltetelet a hivo betartja, akkor a hivott metodus garantalja, hogy az utofeltetele igaz lesz
;elofeltetel, pre-condition: mi a binaris kereses elofeltetele?
;-a bemeneti listanak rendezettnek kell lennie
;-mi az sqrt elofeltetele? a bemeneti szam nem negativ
;-string->szam elofeltetel? a bemeneti stringnek szamnak kell lennie
;az elofeltetel a bemenetrol beszel
;input - bemenet - parameter - formalis parameter lista
;az elofeltetel megadja hogy a fg parametereinek milyennek kell lennie!
;utofeltetel, post-condition:
;-mi a binaris kereses utofeltetele? ha benne van akkor az index ha nincs benne akkor a visszateres: -1
;-mi az utofeltele annak hogy berakok 100ft-ot a bankszamlara
; uj-balansz = regi-balansz + 100 es mondjuk ha sikeres akkor true egyebkent false a visszateres
;mirol beszel az utofeltetel?
;A visszateresi ertekrol
;Ebbol a szempontbol egy fg-t ugy kell specifikalni hogy megmondom az elofeltetelet es az utofeltetelet
;Invarians: az aminek mindig igaznak kell lennie
;pl. bankszamla eseten a balansz (mennyi penzem van a szamlan) az nem lehet negativ
;az invarians az ami a fg hivas elott es utan is igaz kozben lehet hamis de elotte-utana igaz mert ebben szemleletben a fg atominak tekintjuk

;konkret pelda:
;bank account
;ket fuggveny, egy invarians
;withdraw - kivesz
;deposit - betesz

;szintaxis:
;(define/contract (fg-nev param) (-> mibol mibe) fg-torzs)
;mibol: alapvetoen egy tipus,de barmilyen feltetel is lehet, ami bemenetre vonatkozik
;mibol: elofeltetel
;mibe: utofeltetel
;sqrt: N+ -> R+
;a -> jelentese mibol mibe kepez a fg.

(define/contract (my-length lst) (-> list? number?)
  (cond
    [(empty? lst) 0]
    [else
     (define farok (rest lst))
     (define farok-ertek (my-length farok))
     (+ 1 farok-ertek)]))
(println (my-length (list 1 2 3)))

(define balance 1000) ;kezdetben van 0 ft
;(withdraw 100) 100-al csokkenti a balancet
;elofeltetel1: osszeg <= balance
;elofeltetel2: osszeg > 0
;utofeltetel: a visszaadott ertek egyenlo a balance-val
;ha kell es vagy vagy akkor az and es az or nem jo helyette and/c or/c
;kell egy seged logikai fugveny:
(define (pre-withdraw? a) (and (number? a) (integer? a) (exact? a) (> a 0) (<= a balance)))
(define (post-withdraw? return-value) (= return-value balance))
(define/contract (withdraw amount) (-> pre-withdraw? post-withdraw?)
  (set! balance (- balance amount))
  balance)
(println (withdraw 100))
(println (withdraw 100))
;deposit szerzodese:
;bemenet: egy nem negativ szam
;kimenet az uj balansz
(define (pre-deposit? a) (and (number? a) (exact? a) (integer? a) (> a 0)))
(define (post-deposit? kk) (= kk balance))
(define/contract (deposit amount) (-> pre-deposit? post-deposit?)
  (set! balance (+ balance amount))
  balance)
(println (deposit 1000))
;elofeltetel: a bemenetre es az allapotra vonatkozik
;utofeltetel: a kimenetre es az allapot atmenetre vonatkozik
;OOP-ben a belso allapot az objektum mezoinek pillanatnyi erteke
;Kutya k1 = new Kutya("buksi","fekete")
;k1 belso allapota, a nev="buksi" szin="fekete"
;k1.setSzin("zold")
;k1 belso allapota, a nev="buksi" szin="zold"2
;hf irj singletont-t Javaban vagy C#-ban
;irj multitont
;irj proxy-t
;irj prototype-ot