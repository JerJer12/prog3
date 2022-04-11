#lang racket

;saját próba

;1 paratlan szuro lst

(define (szuro lst)
  (cond
    [(empty? lst) (list)]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define farok-ertek (szuro farok))
     (if (number? fej) (if (odd? fej) (cons fej farok-ertek) (szuro farok)) farok-ertek)]))

(writeln (szuro (list "alma" 3 5 6 8 3)))

;2 paros-max lst

;3 setminus

(define (setminus A B)
  (cond
    [(empty? A) (list )]
    [else
     (define fejA (first A))
     (define farokA (rest A))
     (define farok-ertek (setminus farokA B))
     (if (not (member fejA B)) (cons fejA farok-ertek) farok-ertek)]))

(writeln (setminus (list 1 2 3 4 5) (list 2 4 2 1)))

;4 páros sum akkuval

(define (parsum lst) (farok-reku-parsum 0 lst))

(define (farok-reku-parsum akku lst)
  (cond
    [(empty? lst) akku]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define akku2 (+ fej akku))
     (if(even? fej) (farok-reku-parsum akku2 farok) (farok-reku-parsum akku farok))]))

(writeln (parsum (list 1 2 3 4 5 8)))


