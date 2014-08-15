#lang racket

(require rackunit)

(define (list-accumulate f base ls)
  (if (null? ls)
      base
      (f (first ls) 
         (list-accumulate f base (rest ls)))))

(define (list-sum ls)
  (list-accumulate + 0 ls))

(define (nested-list-sum lols)
  (if (null? lols)
      0
      (+ (list-sum (first lols))
         (nested-list-sum (rest lols)))))

(check-equal? (nested-list-sum '()) 0)
(check-equal? (nested-list-sum (list (list 1))) 1)
(check-equal? (nested-list-sum (list (list 1) (list 1))) 2)

(define (deep-list-sum dls)
  (if (null? dls)
      0
      (let [(first-dls (first dls))]
        (+ (deep-list-sum (rest dls))
           (if (list? first-dls)
               (deep-list-sum first-dls)
               first-dls)))))

(check-equal? (deep-list-sum '()) 0)
(check-equal? (deep-list-sum (list (list 1))) 1)
(check-equal? (deep-list-sum (list (list 1) (list 1))) 2)
(check-equal? (deep-list-sum (list 1 (list 1))) 2)
(check-equal? (deep-list-sum (list 1 (list 2 (list 3 (list 4))))) 10)


(define (deep-list-accumulate comb base dls)
  (if (null? dls)
      base
      (let [(first-dls (first dls))]
        (comb (deep-list-accumulate comb base (rest dls))
           (if (list? first-dls)
               (deep-list-accumulate comb base first-dls)
               first-dls)))))

(check-equal? (deep-list-accumulate + 0 '()) 0)
(check-equal? (deep-list-accumulate + 0 (list (list 1))) 1)
(check-equal? (deep-list-accumulate + 0 (list (list 1) (list 1))) 2)
(check-equal? (deep-list-accumulate +  0 (list 1 (list 1))) 2)
(check-equal? (deep-list-accumulate * 1 (list 1 (list 2 (list 3 (list 4))))) 24)


(define (list-append xs ys)
  (if (null? xs)
      ys
      (cons (first xs)
            (list-append (rest xs) ys))))

(define (list-flatten lols)
  (if (null? lols)
      null
      (list-append 
       (first lols)
       (list-flatten (rest lols)))))

(define (deep-list-flatten dls)
  (if (null? dls)
      null
      (let [(first-dls (first dls))
            (rest-dls (rest dls))]
        (list-append 
         (if (list? first-dls)
             (deep-list-flatten first-dls)
             (list first-dls))
         (deep-list-flatten rest-dls)))))

(define deep-list-sum-2
  (compose list-sum deep-list-flatten))

(check-equal? (deep-list-sum-2 '()) 0)
(check-equal? (deep-list-sum-2 (list (list 1))) 1)
(check-equal? (deep-list-sum-2 (list (list 1) (list 1))) 2)
(check-equal? (deep-list-sum-2 (list 1 (list 1))) 2)
(check-equal? (deep-list-sum-2 (list 1 (list 2 (list 3 (list 4))))) 10)

(define (deep-list-map f dls)
  (if (null? dls)
      null
      (let [(first-dls (first dls))]
        (cons
         (if (list? first-dls)
             (deep-list-map f first-dls)
             (f first-dls))
         (deep-list-map f (rest dls))))))

(check-equal? 
 (deep-list-map 
  (lambda (x) (* x x)) 
  (list 2)) 
 (list 4))

(check-equal? 
 (deep-list-map 
  (lambda (x) (* x x)) 
  (list 1 (list 2 (list 3)))) 
 (list 1 (list 4 (list 9))))

(check-equal? 
 (deep-list-map 
  (lambda (x) (* x x)) 
  (list 1 (list 2 (list (list 2 3 4) 3 (list 4))))) 
 (list 1 (list 4 (list (list 4 9 16) 9 (list 16)))))

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

(check-equal? 
 (deep-list-filter 
  (lambda (x) (> x 3)) 
  (list 2 3 4)) 
 (list 4))

(check-equal? 
 (deep-list-filter 
  (lambda (x) (> x 3)) 
  (list (list 2 3 4)(list 2 3 4)))
 (list (list 4)(list 4)))

(check-equal? 
 (deep-list-filter 
  (lambda (x) (<= x 2)) 
  (list 1 (list 2 (list 3)))) 
 (list 1 (list 2 (list))))

(check-equal? 
 (deep-list-filter 
  (lambda (x) (> x 2)) 
  (list 1 (list 2 (list (list 2 3 4) 3 (list 4))))) 
 (list (list (list (list 3 4) 3 (list 4)))))

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