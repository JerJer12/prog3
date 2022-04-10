#lang racket
(define alma 5)
(println alma)
(define (f x) (+ x 1))
(println (f 5))
;a lista rekurziv adatszerkezet, hiszen a lista farka lista.
;a fa rekurziv adatszerkezet, hiszen a fa ágai fák
;a bináris fa rekurziv adatszerkezet, hiszen a bináris fa bal és jobb ága bináris fák
;a listát a list kulcsszó segítségével hozok létre
(define lista1 (list 1 2 3))
(define lista2 '("kocsi" "auto" "szg"))
(println lista1)
(println lista2)
;sajnos a (list 1 2 3) az csak egy érzéki csalódás mert azt jelenti hogy:
;ahol a cons az a listát létrehozó konstruktor neve
;szintaxisa: (cons fej farok)
;szemantikája: létrehozza a fej fejű és farok farkú listát
;(cons 1 (list 2 3))
(println(cons 1(cons 2 (cons 3 empty))))
(println (list 1 #t "hello")) ;inhomogén
(println (list (list 1 2) (list 3 4 5))) ;'((1 2)(3 4 5))
(println (list 1 2 '(3 4 5)));'(1 2 (3 4 5))
(println (list (list "kocsi" "auto") (list "kék" "zöld"))) ;'(("kocsi" "auto") ("kék" "zöld"))
(println (cons "R" (list "G" "B"))) ;'("R" "G" "B")
;mivel a listát szét lehet szedni fejre és farokra a lista fejét a first adja vissza, minden más nyelvben head van
;a lista farkát a rest adja vissza, minden más nyelvben tail van
(define my-list (list 1 2 3))
(println (first my-list))
;a lista feje: 1
(println (string-append "A lista feje: "(number->string(first my-list))))
(println (rest (rest (rest  my-list)))) ; '()
;a lista fejbol és farokbol all kiveve az ures listat mivel nincs se feje se farka
;racket nyelvben a fuggvenyeknek adhatunk elo es uto feltetelt amit egyutt ugy hivunk szerzodes az elofeltetel a bemenetre az uto feltetel a kimenetre (visszateresi ertekre)
(define y '())
(println (empty? y))
;a lista adatszerkezetet rekurzivan dolgozom fel hiszen a lista adatszerkezet rekurziv
;egy fg akkor rekurziv ha meghivja onmagat
; pl.: f(x) = f(x + 1) vegtelen rekurzio
;ha fg.-t hivsz ,akkor regiszterek tartalmat meg a hivas elott el kell menteni a verembe (stack) mert a fg. ezeket atalithatja.
;az ip-t biztosan atalitja
;ezert ha vegtelen rekurziot irsz elob utob elfogy a stack es kapsz egy stack overflow exception-ot
;racket mint mas funkcionalis pny rekurziv fg. atalakitja ciklussa egyszeruen csak vegtelen sokaig fog futni
;szivargo absztrakcio  Joel on software kedvenc cikk 1 oldal essze +the law of leaky abstration 1 oldal essze
;ahoz hogy ne legyen vegtelen rekurzio kell egy kilepesi feltetel vagy mas neven bazis feltetel ami a fg. nem rekurziv agat irja le
;pl.: fact(x) = ha(x==0) akkor return 1 else x*fact(x-1)
(define (fact x) ;rekurziv mert fact hivja fact-ot
  (if (= x 0) ;bazis feltetel
      1       ; nem rekurziv ag
      (* x (fact(- x 1))))) ;rekurziv ag
(println (fact 32))

;a cond kif a tobb agu elagazas
;szintaxisa: (cond [teszt1 kif] [teszt2 kif] ... [else kif])
;szemantikaja: az elso igaz teszt utani kifejezes erteket adja vissza ha egyik sem igaz , akkor else ag kif erteket

(define z empty)
(cond
  [(number? z) "ez egy szam"]
  [(string? z) "ez egy szoveg"]
  [(empty? z) "ez egy ures lista"]
  [(list? z) "ez egy lista"]
  )