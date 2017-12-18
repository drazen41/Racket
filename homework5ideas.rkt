#lang racket
 (define phunk (lambda() (- 7 4)))

(define b (lambda() (+ 3 2))) ;zero argument function = thunk