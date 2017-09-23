;;; special case of newtons sqrt algorithm method called by heron of alexandria

;; note: radicand is the number you are finding the square root of.
;;
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
  (< (abs (- (square guess) radicand)) 0.001))

; new if statement compound from cond - this is problem 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; main method, use the control flow
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; Give it something to chew on, set the first guess to 1.0.
(define (sqrt x) 
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))
