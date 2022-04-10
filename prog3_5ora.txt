#lang racket

;pattern matching = minta illesztés
;deklaratív oldalon a = 1; 1 = a, mindkettő ugyanazt jelenti
;Mert '=' jel nem az értékadás, hanem a intaillesztés
;kivéve LISP, RACKET, ezekben a nyelvekben: match
;szintaxisa:
;(match kif [minta1 viszsatérési-érték1] [minta2 vé2] ...)
;szemantikája: megnézi, hogy kif illeszkedik-e minta1-re, ha igen
;akkor vé1-et adaja vissza, ha nem, nézi a következőt

(define x 2)   ;x = 2

(writeln
 (match x
   [1 "egy"]
   [2 "kettő"]
   [3 "hátom"]))

(writeln
 (cond
   [(= x 1) "egy"]
   [(= x 2) "kettő"]
   [(= x 3) "három"]))

;a mintaillesztésnek van mellékhatása

(writeln (match (list 1 2 1) [(list a b a) b]))
;(writeln (match (list 1 2 1) [(list a b _) b]))
;a név nélküli változó: _ű

(writeln (match (list 1 2 1) [(cons fej farok) fej]))

(writeln (match (list 1 2 1) [(cons fej farok) farok]))    ;(2 1)

;(writeln (match (list ) [(cons fej farok) farok]))

(writeln
 (match(list )
   [(cons fej farok) farok]
   [(list ) "alma"]))

;sum cond-dal (ismétlés)

(define (sum lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (sum farok))
     (+ farok-ertek fej)]))

(writeln (sum (list 2 3 4)))

(define (sum2 lst)
  (match lst
    [(list ) 0]
    ;ha nem üres akkor a konstruktor segítségével szétszedem fejre és farokra
    [(cons fej farok)
     (define farok-ertek (sum2 farok))
     (+ farok-ertek fej)]))

(writeln (sum2 (list 2 3 4)))

;(myappend A B) egymás után füzi a két listát
;(myappend (list 1 2 3) (list 2 3 4)) = (list 1 2 3 2 3 4)
;hint: zrdjük szét az első listát
;mire vezet ez?
;előbb utóbb a lista üres lesz
;(myappend (list ) (list 2 3 4)) = (list 2 3 4)

(define (myappend A B)
  (match A
    [(list ) B]
    [(cons fejA farokA)
     (define farok-ertek (myappend farokA B))
     ;hogyan lesz a farok-ertekből érték?
     ;példa:
     ;(myappend (list 1 2 3) (list 2 3 4)) = (list 1 2 3 2 3 4)
     ;fatok-ertek = (myappend (list 2 3) (list 2 3 4)) = (list 2 3 2 3 4)
     ;tehát fejA + farok-ertek
     ;mivel lehet összerakni listát?
     ;cons szintaxisa: (cons fej farok)
     (cons fejA farok-ertek)]))

(writeln (myappend (list 1 2 3) (list 2 3 4)))

(writeln (match (list 1 2 3) [(list a ___)a]))
;;ez egy olyan lista aminke legalább 1 eleme van

(define nevek (list "Éva" "Ádám" "Káin" "Ábel"))

(writeln (match nevek [(list _ a ___ _) a]))