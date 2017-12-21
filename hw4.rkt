
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define ones (lambda () (cons 1 ones)))
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
(define (stream-for-n-steps stream n)
  (letrec ([f (lambda (stream ans)
              (let ([pr (stream)])
                (if (<= n ans)
                    null
                    (cons (car pr)(f (cdr pr)(+ ans 1))) )))])
    (f stream 0)))

(define funny-number-stream
  (letrec ([f  (lambda (x) (if (= (modulo x 5) 0)
                               (cons (- x) (lambda()(f (+ x 1))))
                               (cons x (lambda()( f (+ x 1)))) ))])
    (lambda() (f 1))))
                 
(define dan-then-dog
 (letrec ([f  (lambda (x) (if (= (modulo x 2) 1)
                               (cons "dan.jpg" (lambda()(f (+ x 1))))
                               (cons "dog.jpg" (lambda()( f (+ x 1)))) ))])
    (lambda() (f 1))))
(define (stream-add-zero stream)
  (let([pr (stream)])
    (cons (car pr) (stream-add-zero (cdr stream)))))
  
(define (cycle-lists xs ys)
  (lambda ()(6)))
(define (vector-assoc v vec)
  #f)
(define (cached-assoc xs n)
  (lambda ()(13)))
  