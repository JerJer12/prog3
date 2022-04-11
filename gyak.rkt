#lang racket

;1

(define (paratlan-szuro lst)
    (cond
        [(empty? lst) (list )]
        [else
            (define fej (first lst))
            (define farok (rest lst))
            (define farok-ertek(paratlan-szuro farok))
            (if(number? fej)(if(odd? fej)(cons fej farok-ertek) (paratlan-szuro farok)) farok-ertek)
        ]
    )
)

(writeln (paratlan-szuro (list 2 5 4 7 "alma")))

;2

(define (paros? ertek)
  (= 0 (modulo ertek 2)))

(define (my-paros-max lst)
  (cond
    [(empty? lst) +nan.0]
    [(empty? (rest lst)) (if (paros? (first lst)) (first lst) 0)]
    [else
       (define fej (first lst))
       (define farok (rest lst))
       (define farok-ertek (my-paros-max farok))
       (if (and (paros? fej) (> fej farok-ertek))
              fej farok-ertek)]))
(writeln (string-append "A legnagyobb páros szám: " (number->string (my-paros-max (list -2 1 4 8)))))

;3

(define (setminus A B)
    (cond
        [(empty? A) (list )]
        [else
            (define fejA (first A))
            (define farokA (rest A))
            (define farok-ertek (setminus farokA B))
            (if (not (member fejA B)) (cons fejA farok-ertek)
                farok-ertek)]))
(writeln (setminus (list 1 2 3 4) (list 2 4 2)))


;4

(define lista1 (list 1 2 3 4 5 6 7))
(define (sum2 lista)
    (
        farok-reku-sum-paros 0 lista
    )
)

(define (farok-reku-sum-paros akku lista)
    (cond
        [(empty? lista) akku] ;ez a nem rekurzív ág
        [else
            (define fej (first lista));ezek  recept
            (define farok (rest lista))
            (define akku2 (+ fej akku))
            (if(even? fej)(farok-reku-sum-paros akku2 farok) (farok-reku-sum-paros akku farok))
        ]
    )
)
(writeln (sum2 (list 1 2 3 4 5 8)))