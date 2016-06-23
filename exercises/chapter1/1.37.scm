; calculated k-term continued fractions
;


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


; the kth term is nk/dk
; the division must be unzipped after n[k],d[k] is calculated
; definition:
; cont-frac(k) = n[1] / (d[1] + ( n[2] / ( d[2] + ( ... ... ) + d[k-1] + n[k]/d[k] )))
;
; our case is a simplification where we know n, d for all k. (to calculate phi)
; this won't help us due to the problem's requirements


; This works but I am obviously missing a callback to the fixed-point code pattern.
(define (cont-frac n d k) 
  (define (cont-frac-helper n d k i)
    (let ((n-value (n i))
          (d-value (d i)))
      (if (= k i)
        (/ n-value d-value)
        (/ n-value (+ d-value (cont-frac-helper n d k (+ i 1)))))))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (cont-frac-helper n d k 1))

; the fixed-point code pattern can definitely be reused here...
(define (cont-frac n d k) 
  (define (cont-frac-helper n d k i)
    (let ((n-value (n i))
          (d-value (d i)))
      (if (= k i)
        (/ n-value d-value)
        (/ n-value (+ d-value (cont-frac-helper n d k (+ i 1)))))))
  (cont-frac-helper n d k 1))

;;; here is the fixed point pattern built to accept cont-frac
;;; note that here we are looking for a fixed point for i, the depth of the fractions.
;;; we are trying to find the number of iterations of the cont-frac method 
;;; relative to the output of the function.
;;; kind of different... not really a fixed point at all
;;; before we were directly finding the fixed point of an algebraic function
;;;
;;; a separate function defines how guess changes. weird!
(define (fixed-point-cont-frac n d f first-guess guess-step)
  (define (try guess)
    (newline)
    (display (f n d guess))
    (let ((next (guess-step guess)))
      (if (close-enough? (f n d guess) (f n d next))
        guess
        (try next))))
  (try first-guess))

; lambdas in the problem for n, d:
(define (n i) 1.0)
(define (d i) 1.0)

(cont-frac n d 40)

(define (increment guess) (+ 1 guess))

(fixed-point-cont-frac n d cont-frac 1 increment)

;;; FINALLY we need to turn cont-frac into a recursive/iterative function. analyze the
;;; method written above and do the other way.t

; it looks like an iterative method because it increments a helper function in the
; recursive call.
;
; a recursive function would put the incrementor state into the method, lets do that?







