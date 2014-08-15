#lang racket

(require rackunit)

(define (pascal-triangle-number row col)
  (if (or (= col 0)
          (= col row))
      1
      (+ (pascal-triangle-number (- row 1) col)
         (pascal-triangle-number (- row 1) (- col 1)))))

(check-equal? 
 (pascal-triangle-number 0 0) 1)

(check-equal? 
 (pascal-triangle-number 1 0) 1)

(check-equal? 
 (pascal-triangle-number 1 1) 1)

(check-equal? 
 (pascal-triangle-number 2 0) 1)

(check-equal? 
 (pascal-triangle-number 2 1) 2)

(check-equal? 
 (pascal-triangle-number 2 2) 1)

(check-equal? 
 (pascal-triangle-number 3 1) 3)

(check-equal? 
 (pascal-triangle-number 3 2) 3)

(check-equal? 
 (pascal-triangle-number 4 1) 4)

(check-equal? 
 (pascal-triangle-number 4 2) 6)

(check-equal? 
 (pascal-triangle-number 4 3) 4)




(define (pascal-triangle-row n)
  (define (helper n i)
    (if (< i 0)
        null
        (cons (pascal-triangle-number n i)
              (helper n (- i 1)))))
  (helper n n))

(check-equal? (pascal-triangle-row 0) (list 1))
(check-equal? (pascal-triangle-row 1) (list 1 1))
(check-equal? (pascal-triangle-row 2) (list 1 2 1))

(define (range from to)
  (if (> from to)
      null
      (cons from 
            (range (+ from 1) to))))

(define (pascal-triangle n) 
  (map 
   pascal-triangle-row
   (range 0 n)))

(check-equal? 
 (pascal-triangle 0) 
 (list (list 1)))

(check-equal? 
 (pascal-triangle 1) 
 (list (list 1) (list 1 1)))

(check-equal? 
 (pascal-triangle 2) 
 (list (list 1) (list 1 1) (list 1 2 1)))

(check-equal? 
 (pascal-triangle 4) 
 (list (list 1) (list 1 1) (list 1 2 1) (list 1 3 3 1) (list 1 4 6 4 1)))