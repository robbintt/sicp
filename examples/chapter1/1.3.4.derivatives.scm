; 1.3.4 Notes
; 
; Newtons method uses a fixed point, but first we should define derivative
; 
; Newtons method is used to find a square root at the bottom

; deriv takes a procedure as an argument and returns a procedure as a value
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
          dx)))

(define dx 0.00001)


; yield the derivative of the cube of 5
(define (cube x) (* x x x))
((deriv cube) 5)

; now use newton's method as a fixed point process
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
; the method takes a method, g, we want to find a 
; nearby zero on, and a guess about where it is.
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; define sqrt in terms of the zero of y |-> y*y - x
; start with an initial guess of 1
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;
;
; Reused code from previous parts of the chapter
;
; Fixed Point
;
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

; Average damping can be generalized like so:
(define (average-damp f)
  (lambda (x) (average x (f x))))


(sqrt 25)

