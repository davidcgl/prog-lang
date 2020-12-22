#lang racket

(define (sum xs)
  (if (null? xs) 0 (+ (car xs) (sum (cdr xs)))))

(define x (sum (list 1)))
(define y (sum (list 1 2 3)))

(print x)
(print y)

