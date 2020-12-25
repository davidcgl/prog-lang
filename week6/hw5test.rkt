#lang racket

(require "hw5.rkt")
(require rackunit)

(define tests
  (test-suite
   "Tests for Assignment 5"
   
   ;; racketlist->mupllist test
   (check-equal? (racketlist->mupllist null)
                 (aunit)
                 "racketlist->mupllist test 1")
   (check-equal? (racketlist->mupllist (list (int 3)))
                 (apair (int 3) (aunit))
                 "racketlist->mupllist test 2")
   (check-equal? (racketlist->mupllist (list (int 3) (int 4)))
                 (apair (int 3) (apair (int 4) (aunit)))
                 "racketlist->mupllist test 3")
   
   ;; mupllist->racketlist test
   (check-equal? (mupllist->racketlist (aunit))
                 null
                 "racketlist->mupllist test 1")
   (check-equal? (mupllist->racketlist (apair (int 3) (aunit)))
                 (list (int 3))
                 "racketlist->mupllist test 2")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit))))
                 (list (int 3) (int 4))
                 "racketlist->mupllist test 3")
   
   ;; int test
   (check-equal? (eval-exp (int 1))
                 (int 1)
                 "int test")   
   ;; aunit test
   (check-equal? (eval-exp (aunit))
                 (aunit)
                 "aunit test")   

   ;; apair test
   (check-equal? (eval-exp (apair (int 1) (int 2)))
                 (apair (int 1) (int 2))
                 "apair test")   

   ;; closure test
   (check-equal? (eval-exp (closure '() (fun #f "x" (int 1))))
                 (closure '() (fun #f "x" (int 1)))
                 "closure test")   

   ;; var test
   (check-equal? (eval-under-env (var "y") (list (cons "x" (int 1)) (cons "y" (int 2))))
                 (int 2)
                 "var test")   

   ;; ifgreater test
   (check-equal? (eval-exp (ifgreater (int 1) (int 2) (int 3) (int 4)))
                 (int 4)
                 "ifgreater test 1")
   (check-equal? (eval-exp (ifgreater (int 2) (int 2) (int 3) (int 4)))
                 (int 4)
                 "ifgreater test 2")
   (check-equal? (eval-exp (ifgreater (int 3) (int 2) (int 3) (int 4)))
                 (int 3)
                 "ifgreater test 2")
   
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x"))))
                 (int 6)
                 "mlet test")
   
   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7))))
                                 (int 1)))
                 (int 8)
                 "call test")
   ;; apair test
   (check-equal? (eval-exp (apair (add (int 1) (int 2))
                                  (int 5)))
                 (apair (int 3) (int 5))
                 "apair test")
   
   ;; fst test
   (check-equal? (eval-exp (fst (apair (int 1) (int 2))))
                 (int 1)
                 "fst test")

   ;; snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2))))
                 (int 2)
                 "snd test")
    
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (int 2)))
                 (int 0)
                 "isaunit test 1")
   (check-equal? (eval-exp (isaunit (aunit)))
                 (int 1)
                 "isaunit test 2")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3)))
                 (int 2)
                 "ifaunit test 1")
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3)))
                 (int 3)
                 "ifaunit test 2")
   (check-equal? (eval-exp (ifaunit (add (int 1) (int 2)) (int 2) (int 3)))
                 (int 3)
                 "ifaunit test 3")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x")))
                 (int 10)
                 "mlet* test")
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4)))
                 (int 4)
                 "ifeq test 1")
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4)))
                 (int 3)
                 "ifeq test 2")
   (check-equal? (eval-exp (ifeq (add (int 1) (int 1)) (int 2) (int 3) (int 4)))
                 (int 3)
                 "ifeq test 3")
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7))))
                                 (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit))
                 "mupl-map test 1")

   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7))))
                                 (apair (int 1) (apair (int 2) (aunit))))) 
                 (apair (int 8) (apair (int 9) (aunit)))
                 "mupl-map test 2")
   
   ;; mupl-mapAddN test
   (check-equal? (eval-exp (call (call mupl-mapAddN (int 1)) (aunit)))
                 (aunit)
                 "mupl-mapAddN test 1")
   (check-equal? (eval-exp (call (call mupl-mapAddN (int 1)) (apair (int 1) (aunit))))
                 (apair (int 2) (aunit))
                 "mupl-mapAddN test 2")
   (check-equal? (eval-exp (call (call mupl-mapAddN (int 1))
                                 (apair (int 1) (apair (int 4) (aunit)))))
                 (apair (int 2) (apair (int 5) (aunit)))
                 "mupl-mapAddN test 3")

   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist (eval-exp (call (call mupl-mapAddN (int 7))
                                                       (racketlist->mupllist (list (int 3) (int 4) (int 9))))))
                 (list (int 10) (int 11) (int 16))
                 "combined test")
   
   ))

(require rackunit/text-ui)
(run-tests tests)
