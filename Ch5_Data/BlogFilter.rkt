#lang racket

(require rackunit)

(define (deep-list-filter pred dls)
  (if (null? dls)
      null
      (let [(first-dls (first dls))
            (rest-dls (rest dls))]
        (if (list? first-dls)
            (cons (deep-list-filter pred first-dls)
                  (deep-list-filter pred rest-dls))
            (if (pred first-dls)
                (cons first-dls
                      (deep-list-filter pred rest-dls))
                (deep-list-filter pred rest-dls))))))

(define (even? x)
  (zero? (modulo x 2)))

(define (evens-in dls)
  (deep-list-filter even? dls))

(define (odds-in dls)
  (deep-list-filter 
   (lambda (x) (not (even? x))) 
   dls))

(check-equal? 
 (evens-in (list 2 3 4)) 
 (list 2 4))

(check-equal? 
 (odds-in (list 2 3 4)) 
 (list 3))

(check-equal? 
 (odds-in (list 1 (list 2 (list 3)))) 
 (list 1 (list (list 3))))

(check-equal? 
 (evens-in (list 1 (list 2 (list 3)))) 
 (list (list 2 (list))))

(check-equal? 
 (evens-in (list 1 (list 2 (list (list 2 3 4) 3 (list 4))))) 
 (list (list 2 (list (list 2 4) (list 4)))))

(check-equal? 
 (odds-in (list 1 (list 2 (list (list 2 3 4) 3 (list 4))))) 
 (list 1 (list (list (list 3) 3 (list)))))