
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;Problem 1
(define (sequence low high stride)
  (if (<= low high)
     (cons low (sequence (+ low stride) high stride))
      null))
; Problem 2
(define (string-append-map xs suffix)
  (map(lambda(x) (string-append x suffix))xs ))
; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0)(error "list-nth-mod: negative number")]
        [(empty? xs)(error "list-nth-mod: empty list")]
        [#t (car(list-tail xs (remainder n (length xs)))) ]))
; Problem 4
(define (stream-for-n-steps stream n)
  (letrec ([f (lambda (stream ans)
              (let ([pr (stream)])
                (if (<= n ans)
                    null
                    (cons (car pr)(f (cdr pr)(+ ans 1))) )))])
    (f stream 0)))
; Problem 5
(define funny-number-stream
  (letrec ([f  (lambda (x) (if (= (modulo x 5) 0)
                               (cons (- x) (lambda()(f (+ x 1))))
                               (cons x (lambda()( f (+ x 1)))) ))])
    (lambda() (f 1))))
; Problem 6                 
(define dan-then-dog
 (letrec ([f  (lambda (x) (if (= (modulo x 2) 1)
                               (cons "dan.jpg" (lambda()(f (+ x 1))))
                               (cons "dog.jpg" (lambda()( f (+ x 1)))) ))])
    (lambda() (f 1))))
; Problem 7
(define (stream-add-zero stream)
  (letrec([f (lambda(x)(cons
                 (cons 0 (car (x)))
               (lambda()(f (cdr(x))))))])
     (lambda()(f stream))))
; Problem 8
(define (cycle-lists xs ys)
  (letrec([f (lambda(x)(cons
                        (cons (list-nth-mod xs x)(list-nth-mod ys x))
                        (lambda()(f(+ x 1))))
                             )])
    (lambda()(f 0))))
; Problem 9
(define (vector-assoc v vec)
  (letrec([l (vector-length vec) ]
          [f (lambda(n)
               (if (< n l)
               (if(and (pair? (vector-ref vec n))(equal? v (car(vector-ref vec n))))
                (vector-ref vec n)
                (f(+ n 1)))
               #f))])
    (f 0)))
; Problem 10 
(define (cached-assoc xs n)
  (letrec([cash (make-vector n)]
          [slot 0]
          [f (lambda(x)
               (let ([ans (vector-assoc x cash )]
                     )
                 (if ans
                     (begin
                       ;(write "Cashed answer" )
                       ;(write slot)
                     ans)
                     (let ([new-ans (assoc x xs)]
                           )
                         (if new-ans
                             (begin
                               ;(write "New answer")
                               ;(write slot)
                               (vector-set! cash slot new-ans )
                               (set! slot (modulo (+ slot 1) n))
                               ;(write slot)
                             new-ans )
                             #f)))))])
                         
                     
                    
    f))
 ; Challenge problem
(define-syntax while-less
  (syntax-rules (do)
    ((while-less x do y)
      (let ([z x])
        (letrec ([loop (lambda ()
			                  (let ([w y])
		 	                    (if (or (not (number? w)) (>= w z))
			                        #t
			                        (loop))))])
          (loop))))))
  