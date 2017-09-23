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

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; kind of a design issue if the function doesn't have a fixed point eh?
; could assign a max recursion depth... but that's a little blunt
; (fixed-point (lambda (x) (* x x)) 2)

; formulate square root as a special case of fixed-point
(define (sqrt-divergent x)
  (fixed-point (lambda (y) (/ x y))
               1.0))
; (sqrt 9)
; We can't even run this because fixed point does not converge as mentioned in notes above.
; This class of not converging is OSCILLATING. It oscillates between x and 1/x.
; We can use the special case that sqrt(x) < x to force convergence
; y = x/y, y+y = y+(x/y), y = (y + x/y) / 2, which introduces the average equation.

; This technique is called "average damping" and is used to aid 
; convergence of fixed point searches.
; I suppose it works by trying to reformulate the function until it has 
; an average somewhere useful.

; reformulated with average damping:
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; now we're good!
(sqrt 9)
