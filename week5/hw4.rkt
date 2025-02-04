#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; 1. Write a function sequence that takes 3 arguments low, high, and stride, all assumed to be
; numbers. Further assume stride is positive. sequence produces a list of numbers from low to high
; (including low and possibly high) separated by stride and in sorted order. Sample solution: 4 lines.

(define (sequence low high stride)
  (if (> low high)
      empty
      (cons low (sequence (+ low stride) high stride))))

; 2. Write a function string-append-map that takes a list of strings xs and a string suffix and
; returns a list of strings. Each element of the output should be the corresponding element of the
; input appended with suffix (with no extra space between the element and suffix). You must use
; Racket-library functions map and string-append. Sample solution: 2 lines.

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

; 3. Write a function list-nth-mod that takes a list xs and a number n. If the number is negative,
; terminate the computation with (error "list-nth-mod: negative number"). Else if the list is empty,
; terminate the computation with (error "list-nth-mod: empty list"). Else return the i th element of
; the list where we count from zero and i is the remainder produced when dividing n by the list’s
; length. Library functions length, remainder, car, and list-tail are all useful – see the Racket
; documentation. Sample solution is 6 lines.

(define (list-nth-mod xs n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(empty? xs) (error "list-nth-mod: empty list")]
    [else
     (define pos (remainder n (length xs)))
     (car (list-tail xs pos))]))

; 4. Write a function stream-for-n-steps that takes a stream s and a number n. It returns a list
; holding the first n values produced by s in order. Assume n is non-negative. Sample solution: 5
; lines. Note: You can test your streams with this function instead of the graphics code.

(define (stream-for-n-steps s n)
  (if (<= n 0)
      empty
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

; 5. Write a stream funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3,
; ...) except numbers divisble by 5 are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...).
; Remember a stream is a thunk that when called produces a pair. Here the car of the pair will be a
; number and the cdr will be another stream.

(define (funny-number-stream)
  (define (next-number n)
    (define head (if (zero? (modulo n 5)) (- n) n))
    (cons head (lambda () (next-number (+ n 1)))))
  (next-number 1))

; 6. Write a stream dan-then-dog, where the elements of the stream alternate between the strings
; "dan.jpg" and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk
; that when called produces a pair of "dan.jpg" and a thunk that when called produces a pair of
; "dog.jpg" and a thunk that when called... etc. Sample solution: 4 lines.

(define (dan-then-dog)
  (define (dan) (cons "dan.jpg" dog))
  (define (dog) (cons "dog.jpg" dan))
  (dan))
  
; 7. Write a function stream-add-zero that takes a stream s and returns another stream. If s would
; produce v for its i th element, then (stream-add-zero s) would produce the pair (0 . v) for its i th
; element. Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion. Note:
; One of the provided tests in the file using graphics uses (stream-add-zero dan-then-dog) with
; place-repeatedly.

(define (stream-add-zero s)
  (define (next-pair s)
    (define pr (s))
    (cons (cons 0 (car pr)) (lambda () (next-pair (cdr pr)))))
  (lambda () (next-pair s)))

; 8. Write a function cycle-lists that takes two lists xs and ys and returns a stream. The lists may
; or may not be the same length, but assume they are both non-empty. The elements produced by the
; stream are pairs where the first part is from xs and the second part is from ys. The stream cycles
; forever through the lists. For example, if xs is ’(1 2 3) and ys is ’("a" "b"), then the stream
; would produce, (1 . "a"), (2 . "b"), (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"), (2 .
; "b"), etc. Sample solution is 6 lines and is more complicated than the previous stream problems.
; Hints: Use one of the functions you wrote earlier. Use a recursive helper function that takes a
; number n and calls itself with (+ n 1) inside a thunk.

(define (cycle-lists xs ys)
  (define (next-pair n)
    (define x (list-nth-mod xs n))
    (define y (list-nth-mod ys n))
    (cons (cons x y) (lambda () (next-pair (+ n 1)))))
  (lambda () (next-pair 0)))

; 9. Write a function vector-assoc that takes a value v and a vector vec. It should behave like
; Racket’s assoc library function except (1) it processes a vector (Racket’s name for an array)
; instead of a list, (2) it allows vector elements not to be pairs in which case it skips them, and
; (3) it always takes exactly two arguments. Process the vector elements in order starting from 0. You
; must use library functions vector-length, vector-ref, and equal?. Return #f if no vector element is
; a pair with a car field equal to v, else return the first pair with an equal car field. Sample
; solution is 9 lines, using one local recursive helper function.

(define (vector-assoc v vec)
  (define (find-element pos)
    (cond
      [(>= pos (vector-length vec)) #f]
      [else
       (define ans (vector-ref vec pos))
       (if (and (pair? ans) (equal? (car ans) v))
           ans
           (find-element (+ pos 1)))]))
  (find-element 0))

; 10. Write a function cached-assoc that takes a list xs and a number n and returns a function that
; takes one argument v and returns the same thing that (assoc v xs) would return. However, you should
; use an n-element cache of recent results to possibly make this function faster than just calling
; assoc (if xs is long and a few elements are returned often). The cache must be a Racket vector of
; length n that is created by the call to cached-assoc (use Racket library function vector or
; make-vector) and used-and-possibly-mutated each time the function returned by cached-assoc is
; called. Assume n is positive.
;
; The cache starts empty (all elements #f). When the function returned by cached-assoc is called, it
; first checks the cache for the answer. If it is not there, it uses assoc and xs to get the answer
; and if the result is not #f (i.e., xs has a pair that matches), it adds the pair to the cache before
; returning (using vector-set!). The cache slots are used in a round-robin fashion: the first time a
; pair is added to the cache it is put in position 0, the next pair is put in position 1, etc. up to
; position n − 1 and then back to position 0 (replacing the pair already there), then position 1, etc.
;
; Hints:
; - In addition to a variable for holding the vector whose contents you mutate with vector-set!,
; use a second variable to keep track of which cache slot will be replaced next. After modifying the
; cache, increment this variable (with set!) or set it back to 0.
; - To test your cache, it can be useful to add print expressions so you know when you are using the
; cache and when you are not. But remove these print expressions before submitting your code.
; - Sample solution is 15 lines.

(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define cache-pos 0)
  (define (cache-add ans)
    (begin
      (vector-set! cache cache-pos ans)
      (set! cache-pos (modulo (+ 1 cache-pos) n))
      ans))
  (define (find-and-cache v)
    (define ans (assoc v xs))
    (if ans (cache-add ans) #f))
  (lambda (v)
    (let ([ans (vector-assoc v cache)])
      (if ans ans (find-and-cache v)))))

; 11. (Challenge Problem:) Define a macro that is used like (while-less e1 do e2) where e1 and e2
; are expressions and while-less and do are syntax (keywords). The macro should do the following:
; - It evaluates e1 exactly once.
; - It evaluates e2 at least once.
; - It keeps evaluating e2 until and only until the result is not a number less than the result of
;   the evaluation of e1.
; - Assuming evaluation terminates, the result is #t.
; - Assume e1 and e2 produce numbers; your macro can do anything or fail mysteriously otherwise.

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([limit e1]
              [loop-until-gte (lambda () (if (>= e2 limit)
                                             #t
                                             (loop-until-gte)))])
       (loop-until-gte))]))
