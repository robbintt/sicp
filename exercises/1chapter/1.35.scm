; Shwo that the golden ratio, phi (sec 1.2.2) is a fixed point of
; the transformation x -> 1 + 1/x and use this fact to compute
; phi by means of the fixed-point procedure.


; first lets dump the fixed point procedure in:
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (average x y) (/ (+ x y) 2))
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

;(fixed-point cos 1.0)
;(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; show: phi is a fixed point of x->1+1/x
;1+1/2, x=1
;1.5
;1+1/1.5, x=1.5
;1.666
;1+1/1.666, x=1.666
;1.6000
;1+1/1.600, x=1.600
;etc:
;1.625
;1.615
;1.619
;1.618
;1.618 <= converged within tolerance of 0.001

; lets define the transformation used to get the golden ratio as a fixed point
(define (golden-transformation x) (+ 1 (/ 1 x)))

(fixed-point golden-transformation 1.0)
