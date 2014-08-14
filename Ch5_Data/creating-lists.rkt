#lang racket

(require rackunit)

(define (list-square ls)
  (if (null? ls)
      null
      (cons (* (first ls) (first ls))
            (list-square (rest ls)))))

(check-eq? (list-square '()) '())
(check-equal? (list-square '(2)) '(4))
(check-equal? (list-square '(1 2 3)) '(1 4 9))

(define (list-map f ls)
  (if (null? ls)
      null
      (cons (f (first ls))
            (list-map f (rest ls)))))

(define (list-square-2 ls)
  (list-map (lambda (x) (* x x)) ls))

(check-eq? (list-square-2 '()) '())
(check-equal? (list-square-2 '(2)) '(4))
(check-equal? (list-square-2 '(1 2 3)) '(1 4 9))

(define (list-increment ls)
  (list-map (lambda (x) (+ x 1)) ls))

(check-eq? (list-increment '()) '())
(check-equal? (list-increment '(2)) '(3))
(check-equal? (list-increment '(1 2 3)) '(2 3 4))

(define (list-filter pred ls)
  (if (null? ls)
      null
      (if (pred (first ls))
          (cons (first ls) 
                (list-filter pred (rest ls)))
          (list-filter pred (rest ls)))))

(define (negatives ls)
  (list-filter (lambda (elem) (< elem 0)) ls))

(check-eq? (negatives '()) '())
(check-equal? (negatives '(-1 2 -3)) '(-1 -3))
(check-equal? (negatives '(1 -2 3)) '(-2))

(define (list-remove pred ls)
  (list-filter (compose not pred) ls))

(define (positives ls)
  (list-remove (lambda (elem) (< elem 0)) ls))

(check-eq? (positives '()) '())
(check-equal? (positives '(-1 2 -3)) '(2))
(check-equal? (positives '(1 -2 3)) '(1 3))

(define (list-unique-elements ls)
  (define (ls-contains? elem ls)
    (if (null? ls)
        false
        (if (= (first ls) elem)
            true
            (ls-contains? elem (rest ls)))))
  
  (define (list-reverse ls)
    (define (helper acc ls)
      (if (null? ls)
          acc
          (helper (cons (first ls) acc) (rest ls))))
    (helper '() ls))
  
  (define (helper unique-ls ls)
    (if (null? ls)
        unique-ls
        (if (ls-contains? (first ls) unique-ls)
            (helper unique-ls (rest ls))
            (helper (cons (first ls) unique-ls) 
                    (rest ls)))))
  
  (list-reverse (helper '() ls)))

(check-eq? (list-unique-elements '()) '())
(check-equal? (list-unique-elements '(1 2 1 3 1 4 2)) '(1 2 3 4))
(check-equal? (list-unique-elements '(1 2 1 3 2 4 5)) '(1 2 3 4 5))

(define (list-append xs ys)
  (if (null? xs)
      ys
      (cons (first xs)
            (list-append (rest xs) ys))))

(check-equal? (list-append '() '()) '())
(check-equal? (list-append '(1 2) '()) '(1 2))
(check-equal? (list-append '() '(1 2)) '(1 2))
(check-equal? (list-append '(1 2) '(3 4)) '(1 2 3 4))

(define (list-reverse ls) 
  (if (null? ls)
      null
      (list-append 
       (list-reverse (rest ls))
       (list (first ls)))))

(check-eq? (list-reverse '()) '())
(check-equal? (list-reverse '(1 2 3 4)) '(4 3 2 1))

(define (list-accumulate f base ls)
  (if (null? ls)
      base
      (f (first ls) 
         (list-accumulate f 
                          base 
                          (rest ls)))))

(define (list-reverse-2 ls) 
  (list-accumulate 
   (lambda (elem rest-ls)
     (list-append rest-ls (list elem))) 
   null 
   ls))

(check-eq? (list-reverse-2 '()) '())
(check-equal? (list-reverse-2 '(1 2 3 4)) '(4 3 2 1))

(define (rev-ints-to n)
  (if (= n 0)
     null
     (cons n (rev-ints-to (- n 1)))))

(define ints-to
  (compose list-reverse rev-ints-to))

(check-eq? (ints-to 0) '())
(check-equal? (ints-to 1) '(1))
(check-equal? (ints-to 2) '(1 2))

(define (factorial n) 
  (list-accumulate * 1 (ints-to n)))

(check-eq? (factorial 0) 1)
(check-eq? (factorial 1) 1)
(check-eq? (factorial 2) 2)
(check-eq? (factorial 3) 6)
(check-eq? (factorial 4) 24)

