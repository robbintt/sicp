;;; Prove Fib(n) is the closest integer to phi^n / sqrt(5)
;;; I have no idea how the psi definition is arrived at, but I can use
;;; induction to just do it based on my functions below.

;;;
;;; DEFINITIONS: Below are definitions used to perform math.
;;;
;;; NEED: an approximation method
;;; (approx x NUM)  Return x truncated to a certain number of digits.
;;; We could also take 2 numbers and say if they are within +/-x of each other.
;;; This seems easier at a glance

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

;;; linear iteration following a->a+b, b->a. SICP p.39.
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

;;; direct definition of phi and psi
;;; still not sure how psi is relevant, gonna have to look that up.
(define (phi)
  (/ (+ 1 (sqrt 5)) 2))
(define (psi)
  (/ (- 1 (sqrt 5)) 2))


;;;
;;;  1.13 Prove that Fib(n) is the closest integer to (phi**n  / sqrt(5))
;;;  I've written an iterative test structure a few times now.
;;;

(define (test-fib-phi n)
  (define (test-iter-fp c)
    (cond ((= c 0) #t)
          (= (fib c) (/ (- (** (phi) c) (** (psi) c)) (sqrt 5))
          (test-iter-fp (- c 1)))
          (else #f)))
  (test-iter-fp n))

(test-fib-phi 50)


