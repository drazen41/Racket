#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (list-ref xs (remainder n (length xs)))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pair (s)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))

(define (funny-number-stream)
  (letrec ([f (lambda (n) (cons (if (= 0 (remainder n 5)) (- n) n) (lambda () (f (+ n 1)))))])
    (f 1)))

(define (dan-then-dog)
  (letrec ([f (lambda (x)
                (cons (if x "dan.jpg" "dog.jpg") (lambda () (f (not x)))))])
    (f #t)))

(define (stream-add-zero s)
  (lambda ()
    (let ([pair (s)])
      (cons (cons 0 (car pair)) (stream-add-zero (cdr pair))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([search (lambda (i) 
                     (if (>= i (vector-length vec))
                         #f
                         (let ([p (vector-ref vec i)])
                           (if (and (pair? p) (equal? v (car p)))
                               p
                               (search (+ i 1))))))])
    (search 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n)]
           [i 0])
    (lambda (v)
      (let ([ans (vector-assoc v cache)])
        (if ans
            ans
            (let ([new-ans (assoc v xs)])
              (if new-ans
                  (begin (vector-set! cache i new-ans)
                         (set! i (remainder (+ i 1) n))
                         new-ans)
                  #f)))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([upper e1])
       (letrec ([loop (lambda ()
                        (if (< e2 upper)
                            (loop)
                            #t))])
         (loop)))]))