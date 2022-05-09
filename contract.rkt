#lang racket
; szerződés alapú programozás design by contract
;minden függvénynek van elő és utófeltétele
; pre- and post-condition 
; pl bináris keresés előfeltétele hogy a bemeneti lista rendezett legyen
; pl gyökvonás bemenete egy szám ami nagyobb egyenlő mint 0
; osztás(a, b) esetén b nem lehet 0
; az előfeltétel a bemenetre vonatkozik
; kimenet: x = max(a,b) x>=a, x>=b
; i = keresindex(k, tömb) i==-1 vagy tömb[i] == k
; rövidzár kiértékelés: 
;hamis és drágafg(), mivel ugyis hamis lesz ezért a drágafg() nem fut le
; igaz vagy drágafg(), mivel ugyis igaz ezért a drágafg() nem fut le
; a visszatérési értékről az utófeltétel beszél
; előfeltétel egy megszorítás a bemenetekre
; utófeltétel egy megszorítás a kimenetre
; előfeltétel adhat megszorítást az állapotra is
; utófeltétel adhat megszorítást az állapotra is
; OOP-ben ráadásul még további veltületek vannak: history constraint
; Class Kutya{
;   String név = "Bodri" ;
;   public void setNév(String ujnev) { név = ujnev; }
; utófeltétele: a this belső állapoti megváltozik a név az az ujnév lesz
; belső állapot, inner state: a mezők pillanatnyi értéke 
; viselkedés, behavior: a futó programkód, a viselekedés függ attól, hogy melyik ágra fut rá a vezérlés, ami pedig a belső állapottól függ
; immutable: nincs belső állapot átmenet, a belső állapotot sehogy se lehet megváltozatni, ilyen pl a String: a.substring(2,3) nem változtatja meg a stringet, hanem visszaad egy ujat.
; stateless: nincs belső állapota, mert nincs egy mezője se
; class Kutya{
;   boolean mérges = false;
;   ...
;   public String ugat(){
;       if(mérges) return "GR... VAU VAU";
;       else return "VAU VAU"; 
;   }
; }
;
; szerződés alapú prgogramozás szintaxisa:
; (define/contract (név paramlista) (-> előfeltétel utófeltétel) (törzs))

(define/contract (osszead a b) 
    (-> number? number? number?) 
    (+ a b))
(writeln (osszead 2 3))

(define/contract (stradd a b)
    (-> string? number? number?)
    (+ b (string-length a)))

(writeln (stradd "alma" 3))

(define/contract (lsum lista)
    (-> list? (and/c number? integer?))
    (match lista
        [(list ) 0]
        [(cons fej farok) (if (string? fej) 
                                (+ (string-length fej) (lsum farok))
                                (+ fej (lsum farok)))]))
        
(writeln (lsum (list 1 2 3)))
(writeln (lsum (list 1 "ab" 3)))
