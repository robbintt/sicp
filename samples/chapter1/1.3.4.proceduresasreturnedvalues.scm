; 1.3.4 Notes
; The |-> symbol in the text means 'maps to' and is mathematics' way of saying 'lambda'.
; Lambda refers to an anonymous procedure in Scheme.
;
; Average damping can be generalized like so:
(define (average-damp f)
  (lambda (x) (average x (f x))))
; reformulate sqrt using average-damp.
; each of the three ideas that contribute to sqrt are made clear:
;   1. fixed-point search
;   2. average damping
;   3. y |-> x/y
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; now lets reuse our concepts for cube root
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))


; Reused code from previous parts of the chapter
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


; test the new sqrt - it works
(sqrt 9)
; test the cube root - it works
(cube-root 27)
