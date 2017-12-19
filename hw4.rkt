
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
     (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map(lambda(x) (string-append x suffix))xs ))
(define (list-nth-mod xs n)
  (cond [(< n 0)(error "list-nth-mod: negative number")]
        [(empty? xs)(error "list-nth-mod: empty list")]
        [#t (car(list-tail xs (remainder n (length xs)))) ]))
(define (stream-for-n-steps s n)
  null)
(define funny-number-stream
  (lambda ()(2)))
(define dan-than-dog
  (lambda ()(5)))
(define (stream-add-zero s)
  (lambda ()(11)))
(define (cycle-lists xs ys)
  (lambda ()(6)))
(define (vector-assoc v vec)
  #f)
(define (cached-assoc xs n)
  (lambda ()(13)))
  