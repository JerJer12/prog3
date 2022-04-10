#lang racket
;for szintakszisa
;(for ([ciklus_valtozo interval]+)(utasitasok))
;sum forral:
(define (my-sum lst)
  (define x 0)      ;lokalis valtozo ebben szamolok ezt adom vissza ugyanaz mint az akku volt
  (for ([akt (in-list lst)])    ;az i segitsegevel vegigmegyek a listan i-ben lesz az aktualis elem
    (set! x (+ x akt)))          ;x = x + 5 <=> (set! x (+ x 5)) x = x + akt <=> (set! x (x . + . akt))
    x                   ;az egesz fuggveny viszzateresi erteke
    )              ;sima set erteket ad olyannak, aminek meg nincs 
                     ;set! erteket ad olyannak, aminek van es mutable
;javaban c# a string immutable barmit csinalsz egy stringel a string nem valtozik csak egy uj jon letre
;a mellekhatas legveszelyesebb valtozata amikor egy globalis valtozoba irok ez gyakran nehez megtalalhato hibat eredmenyez a set! a ! nem tisztan funkcionalis
;a tiszta funkcionalis nyelvekben nem lehet mellekhatast csinalni neha jo a mellekhatas ez a !
(println (my-sum (list 3 1 8)))
;count x lst -megszamolja a listaban hanyszor van benne az x ertek
(define (my-count x lst)
  (define db 0)
  (for ([akt (in-list lst)])
    (if (= x akt) (set! db (+ db 1)) void))
  db)
(println (my-count 2 (list 2 3 4 5 2 2)))
;paros-szuro, kap egy listat visszadja a paros elemek listajat, for-al
(define (paros-szuro lst)
  (define uj-lista (list ))
  (for ([akt (in-list lst)])
    (if (= 0 (modulo akt 2)) (set! uj-lista (append uj-lista (list akt))) uj-lista)
    )
  uj-lista)
(println (paros-szuro (list 1 2 3 4 5)))
;paros-sum, kap egy listat visszaadja a listaban levo paros szamok osszeget
(define (paros-sum lst)
  (define x 0)
  (for ([akt (in-list lst)])
    (if (= 0 (modulo akt 2)) (set! x (+ x akt)) void))
  x)
(println (paros-sum (list 1 2 3 4 5 6)))

;regularis kifejezes
;regular expression
;a regularis kifejezes vagy #rx kezdodik, vagy #px
(define s "hello aawd AACCGTA")
(define rx #rx"[a-z]+") ;. pontosan egy karakter [abc]+ , a.c , [A-Z] ,#px \\d{2} ,#px"(abc)+" .* -akarmi akarhanyszor
;mindig egy olyan maximalis hosszu resz stringet amire illeszkedik a minta
;valami? = valami-bol 0..1 db
;valami+ = valami-bol 1..n db
;valami* = valami-bol 0..n db
(regexp-match? rx s)
(define poz (regexp-match-positions rx s)) ; regexp-match-positions* minden
(define x (car (first poz))) ;car - par elso eleme
(define y (cdr (first poz))) ;cdr - par masodik eleme
(println (substring s x y))
;irj egy mintat ,ami igazat ad, ha szerepel benne az AAGGTA minta
;irj egy regexp-et, ami azt fejezi ki, hogy ez egy DNS szekvencia
(define rxdns #rx"[ACGT]+")
;irj egy regexp-et,ami azt fejezi ki, hogy a DNS-ben van egy AAA es utana valahol egy AGT,tehat eloszor (de nem biztos, hogy az elejen) AAA es utana valahol egy AGT
(define rxagt #rx"^[ACGT]*AAA[ACGT]*AGT[ACGT]*$")
;hint {0,5} jelentese: 0 vagy 1 vagy 2 ... vagy 5-szor fordul elo egymas utan
;legalabb 3 A, de max 8 A es utana valahol GGG
;8szamjegy-1szamjegy-2szamjegy adoszam
;2nagybetu8dbszam
;4szam.2.szam.2szam