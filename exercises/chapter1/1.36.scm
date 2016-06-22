; Modify fixed-point so that it prints the sequence of approximates it generates.
; use newline and display primitives from exercise 1.22
;
; Find a solution to x^x = 1000 by finding a fixed point of x->log(1000)/log(x)
; use scheme's primitive log procedure which computes natural logarithms.
;
; compare the number of steps this takes with and without average damping.
; Note that you cannot start fixed point with a guess of 1, log(1)=0 and 
; thus would cause division by zero.

; first lets dump the fixed point procedure in:
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (average x y) (/ (+ x y) 2))
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

;(fixed-point cos 1.0)
;(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; lets define the transformation used to get the golden ratio as a fixed point
;(define (golden-transformation x) (+ 1 (/ 1 x)))
;(fixed-point golden-transformation 1.0)

; guesses = 40
(define (f x) (/ (log 1000) (log x)))


(fixed-point f 1.01)

; lets apply average damping to x->log(1000)/log(x):
; 2x = x + log(1000) / log(x)
; x = (x + (log(1000)/log(x)))/2
(define (f-avgdamp x) (average x (/ (log 1000) (log x))))

; guesses = 16
(fixed-point f-avgdamp 1.01)


; 16 is a lot less than 40
; Probably the most important observation is that average damping seems
; to leap over the oscillating ranges in our examples.
; It's really taking the midpoint of the original value and the next value
; Assuming an oscillation is at its peak it will completely neutralize the
; oscillation.  If x is below the peak of the oscillation then it will damp
; by less.
;
;
