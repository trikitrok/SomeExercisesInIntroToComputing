#lang racket

(require rackunit)

(define (deep-list-accumulate comb base dls)
  (if (null? dls)
      base
      (let [(first-dls (first dls))]
        (comb (deep-list-accumulate comb base (rest dls))
           (if (list? first-dls)
               (deep-list-accumulate comb base first-dls)
               first-dls)))))

(define (deep-list-sum dls)
  (deep-list-accumulate + 0 dls))

(define (deep-list-prod dls)
  (deep-list-accumulate * 1 dls))

(check-equal? (deep-list-sum '()) 0)
(check-equal? (deep-list-sum (list (list 1))) 1)
(check-equal? (deep-list-sum (list (list 1) (list 1))) 2)
(check-equal? (deep-list-sum (list 1 (list 1))) 2)
(check-equal? (deep-list-prod (list 1 (list 2 (list 3 (list 4))))) 24)