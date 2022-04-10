#lang racket
(define (myrev l)
  (if (null? l)
      '()
      (append (myrev (cdr l)) (list (car l)))
  )
)
(define (reverse lst)
  (cond
    [(empty? lst) empty]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (append (reverse farok) (list fej))]))
(println (reverse (list 1 2 3 4)))

(define (reverse1 lst) (reverse2 empty lst))
(define (reverse2 akku lst)
  (cond
    [(empty? lst) akku]
    [else
     (define fej (first lst))
     (define farok (rest lst))
     (define uj-akku )
     (reverse2 )]))
