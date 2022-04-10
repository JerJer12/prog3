#lang racket

;LISTA KEZELÉS

;adatszerkezetet vizsgáló kérdéssor

;egyszerű vagy összetett?
;egyszerű ha csak egy adatot tud tárrolni, ha többet akkor összetett

;homogén vagy inthomogén?
;egyféle vagy többféle adattípust tárolhat

;soros(hozzáférésű) vagy direkt (hozzáférésű)
;soros, ha a 3. elemet csak úgy érjük el ha előtte az első 2-t is el kell érnünk

;folytonos vagy szétszórt
;folytonos, ha az elemek egymás után vannak, szétszórt ha összevissza

;rekurzív vagy nem rekurzív?

;tömb: össztett, homogén, direkt, folytonos, nem rekurzív
;bifa: összetett, homogén, soros, szétszórt, rekurzív
;lista: összetett, homogén, soros, szétszórt, rekurzív

;a lista két részből áll, fejből és farokból ahol a farok mindig lista (rekurív)

;a kutya az egy kutya ((ez rekurzív) (önmagát definiálja))

;rekurzív adatszerkezetet rekrzívan kell feldolgozni

(define lista1(list "alma" "körte" "barack"))    ;a list kulcsszót kell haszanálni
(writeln lista1)

;fej      -head   -first
;farok    -tail   -reat
;                 a racketben így van

(writeln (first lista1))    ;ez a lista feje
(writeln (rest lista1))     ;elsőn kívül minden más

(define lista2(list 1 2 3))
(writeln (rest lista2))                  ;(2 3)
(writeln (rest(rest lista2)))            ;(3)
(writeln (rest(rest(rest lista2))))      ;()

;;(writeln (rest(rest(rest(rest lista2)))))
;az üres listának nincs se feje se farka

;az 1 elemű listának pl: (3), feje 3, farka ()
;üres listánka eljárásának módja: (list ), empty, null, '()
;(list 1 2 3) helyett: '(1 2 3)

;(writeln (list? lista2))    ;list? igazat ad vissza, ha a paramétere lista
;(writeln (list? "alma"))    ;#t igaz, #f hamis

;(writeln (empty? lista2))
;(writeln (null? lista2))

(define lista3 (list "alma" 11 #t))
(writeln lista3)

;ez csak érzéki csalódás: (list 1)
;mert ez azt jelenti, hogy: (cons 1 empty)
;ahol a cons a lista konstruktor, aminek  a szintaxisa:  (cons fej farok)
;szemantikája: létrehoz egy listát fej fejjel és farok farokkal
;(list 1 2) = (cons 1 (cons 2 empty))

;feladat: hozd létre a (list 1 2 3) listát cons és empty használatával

(define listae(cons 1(cons 2(cons 3 empty))))
(writeln listae)

;feladat: hozza létre ezt (1 (2 3) 4) cons és empty segítségével

(define segéd(cons 2(cons 3 empty)))
(define listae2(cons 1(cons segéd(cons 4 empty))))
(writeln listae2)

(define listae3(cons 1(cons (cons 2(cons 3 empty))(cons 4 empty))))
(writeln listae3)


;lista feldolgozó függvények
;length
(define(length lst)
  (cond
    [(empty? lst)0]     ;nem rekurzív ág
    [else ;ha nem üres a lista akkor szétszedem fejre és farokra
     (define fej (first lst))
     (define farok (rest lst))
     ;recept: meghívom önmagam rekurzívan a farokra
     ;ez visszaad egy értéket amiből számolom a saját visszatérési értéket
     ;példa:
     ;(length (list 5 2 8))  =3
     ;(length (list 2 8))  =2
     (+ 1 (length farok))]))
(writeln (length (list 5 2 8)))

(define (sum lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     ;példa:
     ;(sum (list 5 2 8))   =15
     ;(sum (list 2 8))   =10
     (+ fej (sum farok))]))
(writeln (sum (list 5 2 8)))

;feladat: szorzat megírása

(define (szor lst)
  (cond
    [(empty? lst) 1]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     ;példa:
     ;(szor (list 5 2 8))  
     ;(szor (list 2 8))  
     (* fej (szor farok))]))
(writeln (szor (list 5 2 8)))

;házi min és max megírása, üreslistára nem értelmezett, egy elemű listára a legnagyobb szám az egyetlen szám

(define (min lst)
  (cond
    [(empty? lst) 50]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     ;példa:
     ; 
     (if (> fej (min farok)) (min farok) fej)]))
(writeln (min (list 5 2 8 3)))

(define (max lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     ;példa:
     ; 
     (if (< fej (max farok)) (max farok) fej)]))
(writeln (max (list 5 2 8 3)))


;házi még az avg, átlag

(define (avg lst)
  (cond
    [(empty? lst) 0]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     ;példa:
     ; 
     (/ (sum lst) (length lst))]))
(writeln (avg (list 5 2 8)))

;házi a páros számok summája

