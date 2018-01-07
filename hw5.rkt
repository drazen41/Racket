;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist list)
  (if (null? list)
      (aunit)
      (apair(car list)(racketlist->mupllist (cdr list)))))
(define (mupllist->racketlist es)
  (if (aunit? es)
      null
      (cons (apair-e1 es)(mupllist->racketlist (apair-e2 es)))))
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e)
         e]
        [(aunit? e)
         e]
        [(apair? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)(int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e )env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater works only with integers")))]
        [(mlet? e)
         (let (;[v1 (mlet-var e)]
               ;[v2 (mlet-e e)])
               [foo (cons(cons (mlet-var e) (mlet-e e))env)])
           (eval-under-env (mlet-body e) foo))]
           ;foo)]
        [(fun? e)
         (let ([fn (fun-nameopt e)]
               [ff (fun-formal e)]
               [fb (fun-body e)])
           (eval-under-env fb (list(cons ff env))))]
          
         
        [(closure? e) 
         e
         ]
        [(call? e)
         (letrec ([v1 (eval-under-env(call-funexp e)env)]
               [v2 (eval-under-env (call-actual e) env) ])
           (if (closure? v1) 
              (letrec ([cf (closure-fun v1)]
                       [ce (closure-env v1)])
                      
                (eval-under-env cf v2  ))
                ;(cons v2 ce))
              ;(eval-under-env e env)
              (eval-under-env v1 (call-actual e))
               ))]
        [(fst? e)
         (let([f(fst-e e)])
           (if (apair? f)
               (eval-under-env (apair-e1 f) env)
             (error (format "MUPL fst-e is not apair: ~v" f))))]
        [(snd? e)
         (let([f(snd-e e)])
           (if (apair? f)
               (eval-under-env(apair-e2 f)env)
             (error (format "MUPL snd-e is not apair: ~v" f))))]   
        [(isaunit? e)       
         (if (aunit? (eval-under-env(isaunit-e e)env)) 
          (int 1)
          (int 0))]
         
               
         
         
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (aunit? (eval-under-env e1 null)) e2 e3))
  ;(if (
      
              
(define (mlet* lstlst e2)
  (letrec([env(list (car lstlst))]
         [v1 (eval-under-env e2 env)])
  (if (empty?(cdr lstlst))
        v1
        (mlet* (cdr lstlst) v1))))
  ;(car lstlst))

(define (ifeq e1 e2 e3 e4)
 (let ([v1 (eval-under-env e1 null)]
       [v2 (eval-under-env e2 null)])
           (if (and (int? v1)(int? v2))
               (if (eq? (int-num v1) (int-num v2))
                   (eval-under-env e3 null)
                   (eval-under-env e4 null))
               (error "MUPL ifgreater works only with integers"))))
  ;e1)

;; Problem 4

(define (mupl-map e)
  (fun #f "f" (fun "loop" "xs" (ifgreater 
                    (isaunit (var "xs")) 
                    (int 0)
                    (aunit)
                    (apair
                     (call (var "f") (fst (var "xs")))
                     (call (var "loop") (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
