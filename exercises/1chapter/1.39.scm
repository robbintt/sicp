; calculated k-term continued fractions
;
; Approximate tangent with the given continued fraction


; first lets dump the fixed point procedure in:
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y) (/ (+ x y) 2))

; the kth term is nk/dk
; the division must be unzipped after n[k],d[k] is calculated
; definition:
; cont-frac(k) = n[1] / (d[1] + ( n[2] / ( d[2] + ( ... ... ) + d[k-1] + n[k]/d[k] )))

; the fixed-point code pattern can definitely be reused here...
(define (cont-frac n d k) 
  (define (cont-frac-helper n d k i)
    (let ((n-value (n i))
          (d-value (d i)))
      (if (= k i)
        (/ n-value d-value)
        (/ n-value (+ d-value (cont-frac-helper n d k (+ i 1)))))))
  (cont-frac-helper n d k 1))

(define (convergence-detector n d f first-guess next)
  (define (try guess)
    (if (close-enough? (f n d guess) (f n d (next guess)))
      guess
      (try (next guess))))
  (try first-guess))

; lambdas in the problem for n, d:
(define (phi-n i) 1.0)
(define (phi-d i) 1.0)

(cont-frac phi-n phi-d 40)

(define (increment x) (+ 1 x))

; tangent function stuff below
; the numerator here is no longer always one.  
; instead, x is the input to the tan function.

(define (tan-cf n d k x) 
  (define (cont-frac-helper n d k i x)
    (let ((n-value (n x))
          (d-value (d i)))
      (cond ((= k i) (/ (* n-value n-value) d-value))
            ((= 1 i) (/ n-value (- d-value (cont-frac-helper n d k (+ i 1) x))))
            (else (/ (* n-value n-value) (- d-value (cont-frac-helper n d k (+ i 1) x)))))))
  (cont-frac-helper n d k 1 x))

(convergence-detector phi-n phi-d cont-frac 1 increment)

; yield the integer expansion for the tangent calculator
(define (tan-int-expansion-d i) 
  ; output only odd numbers
  (if (= i 1)
    1
    (- (* i 2) 1)))

(define (tan-n i) i)

(tan-int-expansion-d 1)
(tan-int-expansion-d 2)
(tan-int-expansion-d 3)
(tan-int-expansion-d 4)
(tan-int-expansion-d 5)
(tan-int-expansion-d 6)
(tan-int-expansion-d 7)
(tan-int-expansion-d 8)
(tan-int-expansion-d 9)
(tan-int-expansion-d 10)

; yields tangent function where x is radians
(exact->inexact (tan-cf tan-n tan-int-expansion-d 10 1))
(exact->inexact (tan-cf tan-n tan-int-expansion-d 10 2))
(exact->inexact (tan-cf tan-n tan-int-expansion-d 10 3))
(exact->inexact (tan-cf tan-n tan-int-expansion-d 10 4))
(exact->inexact (tan-cf tan-n tan-int-expansion-d 10 5))
(exact->inexact (tan-cf tan-n tan-int-expansion-d 10 6))
(exact->inexact (tan-cf tan-n tan-int-expansion-d 10 7))
; interestingly 8 needs much more depth
(exact->inexact (tan-cf tan-n tan-int-expansion-d 15 8))
