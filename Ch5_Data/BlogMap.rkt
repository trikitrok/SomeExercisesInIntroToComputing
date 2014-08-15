#lang racket

(require rackunit)

(define (deep-list-map f dls)
  (if (null? dls)
      null
      (let [(first-dls (first dls))]
        (cons
         (if (list? first-dls)
             (deep-list-map f first-dls)
             (f first-dls))
         (deep-list-map f (rest dls))))))

(define (deep-list-square dls)
  (deep-list-map (lambda (x) (* x x)) dls))

(define (deep-list-double dls)
  (deep-list-map (lambda (x) (* 2 x)) dls))

(check-equal? 
 (deep-list-square (list 2)) 
 (list 4))

(check-equal? 
 (deep-list-square (list 1 (list 2 (list 3)))) 
 (list 1 (list 4 (list 9))))

(check-equal? 
 (deep-list-double (list 1 (list 2 (list (list 2 3 4) 3 (list 4))))) 
 (list 2 (list 4 (list (list 4 6 8) 6 (list 8)))))
