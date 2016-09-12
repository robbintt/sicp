; generalize iterative improvement
; rewrite 1.1.7's sqrt and 1.3.3's fixed-point in terms of iterative-improve


;;; BEGIN 1.1.7

;;; special case of newtons sqrt algorithm method called by heron of alexandria
;; note: radicand is the number you are finding the square root of.
;; note2: sqrt-iter gives an example of iteration with no special construct
;; other than the ordinary ability to call a procedure (SICP p.25)

; square a number
(define (square x) (* x x))

; average two numbers
(define (average x y) (/ (+ x y) 2))

; average the guess with x/guess to improve the guess
(define (improve guess radicand)
  (average guess (/ radicand guess)))

; determine when a new guess is close enough to the previous guess
(define (good-enough? guess radicand) 
  (< (abs (- (square guess) radicand)) 0.00001))

; main method, use the control flow
(define (sqrt-iter guess radicand)
  (if (good-enough? guess radicand)
    guess
    (sqrt-iter (improve guess radicand)
               radicand)))

; Give it something to chew on, set the first guess to 1.0.
(define (sqrt x) 
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))
(sqrt 0.002)

;;; END 1.1.7

;;; BEGIN 1.3.3

; Fixed Point
; A fixed point comes from applying f(x) for a given x continuously until 
; the value does not change very much.

; from genmethod half-interval / and from 1.1.7 sqrt
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (average x y) (/ (+ x y) 2))

; arbitrary, reasonable for problem
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; formulate square root as a special case of fixed-point
; doesn't work yet
(define (sqrt-divergent x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

; reformulated with average damping:
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt 9)

;;; END 1.3.3


; expected use case:
; (iterative-improve good-enough? improve)
(define (iterative-improve good-enough? improve-guess)
  ; this method takes a guess and returns the result when it satisifies good-enough?
  ; it should improve the guess using improve-guess
  (define (improve-recursor guess)
    (let ((improved-guess (improve-guess guess)))
    (if (good-enough? guess improved-guess)
    improved-guess
    (improve-recursor improved-guess))))
  improve-recursor)

; first problem: where is the test value / radicand stored?
; lets just keep it in internal function definitions
(define (sqrt-iter-improve radicand)

  (define (improve guess)
    (average guess (/ radicand guess)))
  ; use stub because we don't actually use the improved-guess from iterative-improve
  (define (good-enough? guess stub) 
    (< (abs (- (square guess) radicand)) 0.00001))

  ((iterative-improve good-enough? improve) 1.0))

(sqrt-iter-improve 9)
(sqrt-iter-improve 25)
(sqrt-iter-improve 10000)


(define (fixed-point-iter-improve f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

; test on original fixed point functions, worked
(fixed-point-iter-improve cos 1.0)
(fixed-point-iter-improve (lambda (y) (+ (sin y) (cos y))) 1.0)

; now to formulate sqrt in terms of this fixed-point
(define (sqrt-iter-improve-fp x)
  (fixed-point-iter-improve (lambda (y) (average y (/ x y)))
               1.0))

(sqrt-iter-improve-fp 9)
(sqrt-iter-improve-fp 25)
(sqrt-iter-improve-fp 10000)
