#lang racket

(require rackunit)

(define (list-length ls) 
  (if (null? ls)
      0
      (+ 1 (list-length (rest ls)))))

(check-eq? (list-length '()) 0) 
(check-eq? (list-length '(1)) 1)
(check-eq? (list-length '(1 2)) 2)

(define (list-sum ls)
  (if (null? ls)
      0
      (+ (first ls) (list-sum (rest ls)))))

(check-eq? (list-sum '()) 0) 
(check-eq? (list-sum '(1)) 1)
(check-eq? (list-sum '(1 2)) 3)

(define (list-max ls) 
  (define (biggest a b)
    (if (> a b) a b))
  
  (if (null? ls)
      0
      (biggest 
       (first ls)
       (list-max (rest ls)))))

(check-eq? (list-max '()) 0) 
(check-eq? (list-max '(1)) 1)
(check-eq? (list-max '(1 2)) 2)
(check-eq? (list-max '(3 1 2)) 3)
(check-eq? (list-max '(3 1 4 2)) 4)

(define (list-accumulate f base ls)
  (if (null? ls)
      base
      (f (first ls) 
         (list-accumulate f 
                          base 
                          (rest ls)))))

(define (list-sum-2 ls)
  (list-accumulate + 0 ls))

(check-eq? (list-sum-2 '()) (list-sum '())) 
(check-eq? (list-sum-2 '(1 2)) (list-sum '(1 2))) 
(check-eq? (list-sum-2 '(1 2 3)) (list-sum '(1 2 3)))

(define (list-max-2 ls) 
  (list-accumulate 
   (lambda (a b)
     (if (> a b) a b))
   0 
   ls))

(check-eq? (list-max-2 '()) (list-max '())) 
(check-eq? (list-max-2 '(1)) (list-max '(1)))
(check-eq? (list-max-2 '(1 2)) (list-max '(1 2)))
(check-eq? (list-max-2 '(3 1 2)) (list-max '(3 2 1)))
(check-eq? (list-max-2 '(3 1 4 2)) (list-max '(3 1 4 2)))

(define (list-length-2 ls) 
  (list-accumulate 
   (lambda (elem rest-ls-length)
     (+ 1 rest-ls-length))
   0
   ls))

(check-eq? (list-length-2 '()) (list-length '())) 
(check-eq? (list-length-2 '(1)) (list-length '(1)))
(check-eq? (list-length-2 '(1 2)) (list-length '(1 2)))

(define (is-list? ls)
  (if (null? ls)
      true
      (if (pair? ls)
          (is-list? (rest ls))
          false)))

(check-true (is-list? '()))
(check-false (is-list? 1))
(check-true (is-list? '(1)))
(check-true (is-list? '(1 2)))

(define (is-list-2? ls)
  (or (null? ls)
      (and (pair? ls)
           (is-list-2? (rest ls)))))

(check-eq? (is-list-2? '()) (is-list? '()))
(check-eq? (is-list-2? 1) (is-list? 1))
(check-eq? (is-list-2? '(1)) (is-list? '(1)))
(check-eq? (is-list-2? '(1 2)) (is-list? '(1 2)))

(define (list-accumulate-2 f base ls)
  (if (null? ls)
      base
      (if (pair? ls)
          (f (first ls) 
             (list-accumulate f 
                              base 
                              (rest ls)))
          false)))

(define (is-list-3? ls)
  (list-accumulate-2
   (lambda (elem rest-is-list)
     true)
   true
   ls))

(check-eq? (is-list-3? '()) (is-list? '()))
(check-eq? (is-list-3? 1) (is-list? 1))
(check-eq? (is-list-3? '(1)) (is-list? '(1)))
(check-eq? (is-list-3? '(1 2)) (is-list? '(1 2)))

(define (list-get-element ls n)
  (if (null? ls)
      (error "index out of range")
      (if (= n 0) 
          (first ls)
          (list-get-element (rest ls) (- n 1)))))

(check-eq? (list-get-element '(0 1 2) 0) 0)
(check-eq? (list-get-element '(0 1 2) 1) 1)
(check-eq? (list-get-element '(0 1 2) 2) 2)
(check-exn
 exn:fail?
 (lambda ()
   (list-get-element '(0 1 2) 3)))

(define (list-last-element ls)
  (if (null? ls)
      (error "empty-list")
      (let 
          [(rest-ls (rest ls))]
        (if (null? rest-ls)
            (first ls)
            (list-last-element rest-ls)))))

(check-eq? (list-last-element '(0)) 0)
(check-eq? (list-last-element '(0 1)) 1)
(check-eq? (list-last-element '(0 1 2)) 2)
(check-exn
 exn:fail?
 (lambda ()
   (list-last-element '())))

(define (list-ordered? test ls)
  (if (null? ls)
      true
      (let 
          [(rest-ls (rest ls))]
        (if (null? rest-ls)
            true
            (and (test (first ls) 
                       (first rest-ls))
                 (list-ordered? test rest-ls))))))

(check-true (list-ordered? < '()))
(check-true (list-ordered? < '(3)))
(check-true (list-ordered? < '(1 4)))
(check-true (list-ordered? < '(2 3 5)))
(check-false (list-ordered? < '(1 3 2)))
(check-false (list-ordered? < '(3 2)))

(define (list-ordered-2? test ls)
  (or (null? ls)
      (null? (rest ls))
      (and (test (first ls) 
                 (first (rest ls)))
           (list-ordered? test (rest ls)))))

(check-true (list-ordered-2? < '()))
(check-true (list-ordered-2? < '(3)))
(check-true (list-ordered-2? < '(1 4)))
(check-true (list-ordered-2? < '(2 3 5)))
(check-false (list-ordered-2? < '(1 3 2)))
(check-false (list-ordered-2? < '(3 2)))