;;; Prove Fib(n) is the closest integer to phi^n / sqrt(5)
;;;
;;; 


;;; square x
(define (square x) (* x x))

;;; average x and y
(define (average x y) (/ (+ x y) 2))

;;; give square root of x
(define (sqrt x) 
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess) 
    (< (abs (- (square guess) x)) 0.00001))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;; give x ** y (exponent)
(define (** x y)
  (define (**iter result count)
    (cond ((= 0 count) 1)
          ((= 1 count) result)
          (else (**iter (* x result) (- count 1)))))
  (**iter x y))


