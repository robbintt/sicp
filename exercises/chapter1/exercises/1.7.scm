;;;
;;; Problem: The good-enough? procedure doens't work well for the square
;;; roots of very small numbers.
;;;
;;; Alternative strategy for sqrt procedure called good-enough? is to
;;; watch how guess changes from one iteration to the next, and
;;; stop when the change is a very small fraction of the guess. Design
;;; a square root procedure that uses this kind of "end test". Does this
;;; work better for small and large numbers?
;;;
;;; Needs:
;;; We need to track the previous value of guess, and subtract the current
;;; guess from this previous value.
;;; Where? We need to do this in good-enough? as our test case.
;;; Currently we get the radicand in there so we'll have to pass prev-guess
;;; instead, which means we'll need prev-guess in the main method, sqrt-iter.


; square a number
(define (square x) (* x x))

; average two numbers
(define (average x y) (/ (+ x y) 2))

; average the guess with x/guess to improve the guess
(define (improve guess radicand)
  (average guess (/ radicand guess)))

; determine when a new guess is close enough to the previous guess
(define (good-enough? guess prev-guess) 
  (< (/ (abs (- guess prev-guess)) guess) 0.0001 ))

; main method, use the control flow
(define (sqrt-iter guess radicand prev-guess)
  (if (good-enough? guess prev-guess)
    guess
    (sqrt-iter (improve guess radicand)
               radicand
               guess)))

; Give it something to chew on, set the first guess to 1.0.
(define (sqrt x) 
  (sqrt-iter 1.0 x 0))

(sqrt 0.002)
