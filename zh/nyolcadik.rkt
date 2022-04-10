#lang racket
;sima for: szintaxis:
;(for([ciklus-valtozo-1 range1][ciklus-valtozo-2 range2]...)
;utasitasok, itt nincs olyan, hogy utolso kituntetett lenne
;ezert set!-et kell hasznalni ami nagyon csunyanak szamit)
(define x (for ([i (in-range 0 5)]) i ))
(println x)
;ismetles szumma for ciklussal
(define (my-sum lst)
  (define ki 0)
  (for ([x (in-list lst)]) ;imperativan ki = ki + x az x-szel megyek vegig a listan ugyanaz mint c#-ban foreach(int x : lst)
    (set! ki (+ ki x)))
  ki)
(println (my-sum '(1 2 3 4)))
;vissza egy veletlen lista
(define (random-list )
  (define ki empty)
  (for ([i (in-range 1 4)])
    (set! ki (append ki (list (random 10)))))
  ki)
(println (random-list))
;irjuk meg ugyanezt a for/list segitsegevel
;szintakszisa ugyanaz mint a sima fornak de a szemantikaja mas
;egy nev nelkuli listat bovit a for/list utolso kif ertekevel
;mindig a vegen boviti
(define (feltolt-list )
  (for/list ([i (in-range 0 10)])
    (random 10)
    ))
(println (feltolt-list))
;feladat szorzotabla listaba
;hint1: ket ciklus valtozo kell i,j
;hint2 : szambol->szoveg: (number->string i)
;hint3: string osszefuzes: (string-append s1 s2 s3 ...)
(define (szorzotabla )
  (for*/list ([i (in-range 0 10)] [j (in-range 0 10)])
    (define si (number->string i))
    (define sj (number->string j))
    (define szorzat (number->string (* i j)))
    (string-append si "*" sj "=" szorzat)))
(println (szorzotabla))
;for* egymasba agyazott for ciklus
;for egyszerre lepteti a valtozokat
;for/and , for/or
;for/and osszeés-eli a for/and utolso kif erteket
;for/or ossze or-ol
;for/and megegyezik a logikabana minden kvantorral
;for/or megegyezik a logikabana létezik kvantorral
;letezik-paros-elem lista
(define (letezik-paros-elem lst)
  (for/or ([x lst])
    (even? x)))
(println (letezik-paros-elem (list 1 2 3)))
(define (mind-paros-elem lst)
  (for/and ([x lst])
    (even? x)))
(println (mind-paros-elem (list 1 2 3)))
;minden x in lst letezik y in lst (x >= y)
(define (valami lst)
  (for/and ([x lst])
    (for/or ([y lst])
      (>= x y))))
(println (valami (list 1 2 3)))
;legosibb osztaly object%
;(define classnev% (class ososztaly-neve
 ;                   (init resz)
  ;                  (mezok)
   ;                 (szuper konstruktor hivas)
    ;                 
     ;               (metodusok)))
;feladat:ember osztaly nev es eletkor
(define ember%
  (class object%
    (init nev eletkor)       ;konstruktor
    (define current-nev nev) ;a konstruktor hivasnal megadod nev a kezdoerteke
    (define current-eletkor eletkor)
    (super-new)
    ))
(define ember1 (new ember% [nev "Ádám"][eletkor 22]))
(define ember2 (new ember% [nev "Milán"][eletkor 21]))
;macska%-nak van neve,szine
(define macska%
  (class object%
    (init nev szin)
    (define current-nev nev)
    (define current-szin szin)
    (super-new)
    (define/public (get-nev) current-nev)
    ))
(define macska1 (new macska% [nev "cirmi"][szin "barna"]))
(println (send macska1 get-nev))
;Java: macska1.getNev() mert ez metodus hivas
;a metodus hivas es az uzenet kuldes ugyanolyan erosegu programozoi ezskoz
;amit meg lehet csinalni metodus hivassal azt meg lehet csinalni uzenet kuldessel (forditva is)
;aszinkron dolgot uzenet kuldessel
;szinkron dolgot metodus hivassal
;metodus hivas az ugy nevezett ip (instraction pointer) atadom a metodusnak majd visszakapom az elerest
;addig en megallok amig az altalam hivott uzenet
;az uzenet kuldes aszinkron modon elkuldom az uzenetet egyszer majd megkapom a valaszt es felhasznalom
;uzenet kuldesnel mindig kell postas broker (belenez virust keres)
;metodus hivas eseten ha tavoli geprol kell rpc (remote procedure call) szimulalja
