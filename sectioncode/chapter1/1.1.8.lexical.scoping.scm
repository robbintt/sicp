;; set up with block structure using lexical scoping
;; I did this conversion manually from the old code to the code on p.30

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (sqrt x) 
  ; these definitions have been moved into the scope of sqrt and
  ; are now in the lexical scope of sqrt, which means they have
  ; access to x. As such, x is no longer passed.
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess) 
    (< (abs (- (square guess) x)) 0.00001))
  ; main method, use the control flow
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))
(sqrt 0.002)
