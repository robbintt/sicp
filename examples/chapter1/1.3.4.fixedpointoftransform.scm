; "Elements with the fewest restrictions" are said to have "first-class" status.
; Rights and Privileges of first-class elements:
; - They may be named as variables.
; - They may be passed as arguments to procedures.
; - They may be returned as results of procedures.
; - They may be included in data structures.
;
; LISP awards procedures full first-class status. 
; "This poses challenges for efficient implementation, but the resulting gain in expressive power is enormous"
; The footnote mentions that the major cost is reserving storage for a procedure's free variables, and that in section 4.1 we see that Scheme stores those variables in the procedure's environment.

; a general method for applying a fixed point on a transformation of a function
; here we generalize the transform away
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
; next we use this generalization to rebuild the two fixed-point methods for arriving at a sqrt.


; recast the first square root, y |-> x / y, in terms of average-damp
(define (sqrt1 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))


; recast newton's method square root method from fixed point of newton transform of y |-> y*y - x
(define (sqrt2 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x ))
                            newton-transform
                            1.0))


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

(define dx 0.00001)
; deriv takes a procedure as an argument and returns a procedure as a value
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
          dx)))
; now use newton's method as a fixed point process
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))




(sqrt1 25)

(sqrt2 25)
