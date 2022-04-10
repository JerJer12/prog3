#lang racket
;szerzodes alapu programozas
;a fuggvenynek van elo- es utofeltetele
;pre- and post- condition
;az elofeltetel a bemenetre vonatkozik azaz a parameterekre
;az utofeltetel a kimenetre vonatkozik azaz a visszateresi ertekre
(define account 1000000)
(define (pre-withdraw? amount) (and (exact? amount) (number? amount) (> amount 0) (<= amount account)))
;utofeltetel: account-t a bemeneten kapott amount-tal kell megnovelni
;uj-acc == regi-acc + amount
;meglehet csinalni de sajnos nem egyszeru
;1. megoldas felveszek 2 globalist, a 2 globalisba beleirom es hasznalom
;gond: globalis valtozoba irni az mellekhatas (side-effect)
;2. megoldas nem csak a accountot adom vissza, hanem a masik kettot is
;egyreszrol jo mert nincs mellekhatas masreszrol nem egy szamot adok vissza hanem egy listat
(define (post-withdraw? lst) (match lst [(list account regi-acc amount) (= account (- regi-acc amount))]
                               [_ #f]))
(define/contract (withdraw amount)
  (-> pre-withdraw? post-withdraw?)
  (define regi-acc account)
  (set! account (- account amount))
  (list account regi-acc amount))
(println (first (withdraw 100)))

;pattern matching mintaillesztes
;szintaxisa: (match ertek ([minta1 kif1][minta2 kif2][minta3 kif3] ...))
;szemantikaja: az elso olyan kif erteket adja vissza ahol a minta illeszkedik az ertekre
;FONTOS1 : a mintaban lehet valtozo
;FONTOS2 : az ertekben nem lehet valtozo
;FONTOS3 : a mintaillesztesnek van mellekhatasa a valtozok erteket kapnak
; (5,3) = (5,b) ahol = jelentse azt hogy mintaillesztes b megkapja a 3-mat ertek
; (5,3) = (a,b) lehetseges? igen, mellekhatas a := 5, b:= 3
;(a, 3) = (5,b)
;(1 ,2 ,3) = (a, _,b) lehetseges, a := 1 b := 3
;(1,2) = (a,a) lehetseges? nem
;(1,1) = (a,a) lehetseges? igen
;(1,2) = (_,_) lehetseges? igen mellekhatas nincs

;terjunk at racketre
(println (match (list 5 3) [(list 5 b) b]))
(println (match (list 5 3) [(list a b) (list a b)]))
(println (match (list 1 2 3) [(list a _ b) (list a b)]))
(println (match (list 1 2) [(list a a) a][_ "nem jou"]))
(println (match (list 1 1) [(list a a) a]))

;hasznaljuk valami hasznosra
;szedjuk szet a listat fejre es farokra
(define (length-v1 lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (length-v1 farok))
     (+ farok-ertek 1)]))
(define (length-v2 lst)
  (match lst
    [(list ) 0]
    [(cons fej farok)
     (define farok-ertek (length-v2 farok))
     (+ farok-ertek 1)]))

(println (length-v1 (list 1 2 3)))
(println (length-v2 (list 1 2 3)))

;ird meg a sum-v2-t match-csel
(define (sum-v2 lst)
  (match lst
    [(list ) 0]
    [(cons fej farok)
     (define farok-ertek (sum-v2 farok))
     (+ farok-ertek fej)]))
(println (sum-v2 (list 1 2 3)))
;a szerzodes helye a dokumentalo megjegyzes: java:/** c#:///
;@param amount , ide kell leirnom az elofeltetelt
;@return ide kell irni az utofeltetelt
;az ondokumentalo resz az api resze
;asser kenyszerito ereju
;az assert ki illetve bekapcsolhato