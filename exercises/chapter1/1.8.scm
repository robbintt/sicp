;;; Implement a cube root procedure analogous to the square root procedure.
;;; There is a formula in the book used as a guide.
;;;
;;; Steps:
;;; Use the Square root as a template

;;; also used in the cube method to improve the guess.
(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (improve guess radicand)
  (average guess (/ radicand guess)))

(define (good-enough? guess radicand) 
  (< (abs (- (square guess) radicand)) 0.00001))

; MAIN - sqrt improvement
(define (sqrt-iter guess radicand)
  (if (good-enough? guess radicand)
    guess
    (sqrt-iter (improve guess radicand)
               radicand)))

; set up
(define (sqrt x) 
  (sqrt-iter 1.0 x))


;;; used to test cube root 1.8
(define (cube x) (* x x x))

;;; used to improve cube method 1.8
(define (cube-improve guess radicand)
  (/ (+ (/ radicand (square guess)) (* 2 guess)) 3))

;;; modified for cube
(define (cube-good-enough? guess radicand) 
  (< (abs (- (cube guess) radicand)) 0.00001))

; MAIN - cube root improvement
(define (cuberoot-iter guess radicand)
  (if (cube-good-enough? guess radicand)
    guess
    (cuberoot-iter (cube-improve guess radicand)
               radicand)))

; set up
(define (curt x) 
  (cuberoot-iter 1.0 x))

(curt 27)
