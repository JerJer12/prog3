#lang racket
(provide (contract-out
          [amount (and/c number? integer?)]
          [deposit  (-> number? any/c)]
          [deposit2 (-> number? number? any/c)]
          [deposit3 (->i ([x number?]
                         [y number?])
                         [result (x y) any/c])]
          [inc (->i ([x number?])
                    [result (x) (and/c number?
                                (lambda (res) (= res (x . + . 1))))])]
          [balance (-> number?)]))
 
(define amount 0)
(define (deposit a) (set! amount (+ amount a)))
(define (deposit2 a b) (set! amount (+ amount a)))
(define (deposit3 a b) (set! amount (+ amount a)))
(define (inc szkk) (szkk . + . 1))
(define (balance) amount)