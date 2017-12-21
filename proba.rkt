#lang racket

(define (sum1 xs)
  (if(null? xs)
     0
     (if(number?(car xs))
        (+(car xs)(sum1(cdr xs)))
        (+(sum1 (car xs))(sum1(cdr xs))))))

(define test (sum1(list 1 2 3 4 5 6(list 1 2 3(list 1 2)))))


;(define test1 (sum1(list 1 2 3 (list 1)(list #t)(list 5 6 7)"foo")))

(define test2 (sum1(list (list 1 2)(list 2 3)(list 3 4))))

(letrec([is-even? (lambda(n)
                    (or (zero? n)
                        (is-odd? (sub1  n))))]
        [is-odd? (lambda (n)
                   (and (not (zero? n))
                        (is-even? (sub1 n))))])
  (is-odd? 10))