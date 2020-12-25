#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs
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

(define (racketlist->mupllist rlist)
  (if (null? rlist)
      (aunit)
      (apair (car rlist)
             (racketlist->mupllist (cdr rlist)))))

(define (mupllist->racketlist mlist)
  (if (aunit? mlist)
      '()
      (cons (apair-e1 mlist)
            (mupllist->racketlist (apair-e2 mlist)))))

;; Problem 2

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e] 
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error (format "MUPL addition applied to non-number: ~v ~v" v1 v2))))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) 
                      (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error (format "MUPL ifgreater cannot compare non-numbers: ~v ~v" v1 v2))))]
        [(fun? e)
         (closure env e)]
        [(call? e)
         (let ([funexp (eval-under-env (call-funexp e) env)]
               [actual (eval-under-env (call-actual e) env)])
           (if (closure? funexp)
               (let* ([fun (closure-fun funexp)]
                      [nameopt (fun-nameopt fun)]
                      [formal (fun-formal fun)]
                      [env (cons (cons formal actual) (closure-env funexp))])
                 (if (string? nameopt)
                     (eval-under-env (fun-body fun) (cons (cons nameopt funexp) env))
                     (eval-under-env (fun-body fun) env)))
               (error (format "MUPL funexp is not a closure: ~v" funexp))))]
        [(mlet? e)
         (let ([var (mlet-var e)]
               [val (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e)
                           (cons (cons var val) env)))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([res (eval-under-env (fst-e e) env)])
           (if (apair? res)
               (apair-e1 res)
               (error (format "MUPL fst only works for apair: ~v" res))))]
        [(snd? e)
         (let ([res (eval-under-env (snd-e e) env)])
           (if (apair? res)
               (apair-e2 res)
               (error (format "MUPL snd only works for apair: ~v" res))))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([res (eval-under-env (isaunit-e e) env)])
           (if (aunit? res)
               (int 1)
               (int 0)))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([var (caar lstlst)]
            [expr (cdar lstlst)])
        (mlet var expr (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x")
                    (var "_y")
                    e4
                    (ifgreater (var "_y")
                               (var "_x")
                               e4
                               e3))))

;; Problem 4

(define mupl-map
  (fun #f "fun"
       (fun "map" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "fun") (fst (var "lst")))
                            (call (var "map") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map")
                   (fun #f "num"
                        (add (var "i") (var "num")))))))

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
