#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5_sol2.rkt")
;(require "hw5.rkt")

(require rackunit)
(define env (cons(cons "x" (int 1))null)); env je racket lista racket parova
(define eval (eval-under-env (var "x") env));
(define lstlst (list(cons "x" (int 10))))
(define clos-call (call mupl-map (fun #f "x" (add (var "x") (int 7)))))
(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")

   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")

   ;; test fun -> fun nameopt(string) formal(string) body (exp) - u body nameopt vezati za funkciju (rekurzija), formal za argument u body ; fun #f formal body -> nerekurzivna funkcija
    ;(check-equal? (eval-under-env (fun #f "x" (add (var "x") (int 7))) (int 1)) (int 8) "fun test")
    
   ;; mlet test -> mlet(var e body) - e (exp) i var (string) su env, body evaluira sa env
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   (check-equal? (eval-exp(mlet "f" (fun "length" "lst" (ifgreater (isaunit (var "lst")) (int 0) (int 0) (add (int 1) (call (var "length") (snd (var "lst")))))) (call (var "f") (aunit))))(int 0) "mlet test2")
   ;(check-equal? (eval-under-env (var "x")env)(int 1) "eval-under-env test")
   ;(check-equal? (eval-under-env (add(int 5)(var "x"))env)(int 6) "eval-under-env test")

   ;; call test - e1 (closure-obavezno), e2 (environment za closure)
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call (closure '() (fun "sum-to" "n" (ifgreater (var "n") (int 1) (add (var "n") (call (var "sum-to") (add (var "n") (int -1)))) (int 1)))) (int 1))) (int 8) "call test2")
   
   ;;snd/fst test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   (check-equal? (eval-exp (fst (apair (add (int 1) (int 3)) (aunit)))) (int 4) "fst test")
   (check-equal? (eval-exp (snd (apair (add (int 1) (int 3)) (add(int 1)(int 3))))) (int 4) "snd test 2")
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
    (check-equal? (eval-exp (isaunit (apair (int 2) (int 1)))) (int 0) "isaunit test 1")
     (check-equal? (eval-exp (isaunit (fst (apair (aunit) (aunit))))) (int 1) "isaunit test 2")
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (fst (apair (aunit) (int 0))) (int 4) (int 10))) (int 4) "ifaunit test")
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
    ;(check-equal? (eval-exp (mlet* (list (cons "x" (var "y"))(cons "y" (int 20))) (var "x"))) (int 20) "mlet* test 2")
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (add(int 1)(int 1)) (int 2) (int 3) (int 4))) (int 3) "ifeq test 2")
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
              (apair (int 8) (aunit)) "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                  (racketlist->mupllist 
                  (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
