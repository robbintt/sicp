; calculated k-term continued fractions
;


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

(convergence-detector phi-n phi-d cont-frac 1 increment)

(define (euler-d i) 
  (if (= 2 (modulo i 3)) 
    (* 2 (+ 1 (floor (/ i 3))))
    1))

(euler-d 1)
(euler-d 2)
(euler-d 3)
(euler-d 4)
(euler-d 5)
(euler-d 6)
(euler-d 7)
(euler-d 8)
(euler-d 9)
(euler-d 10)

; yields e using euler's sequence for d
(+ 2 (cont-frac phi-n euler-d 40))
