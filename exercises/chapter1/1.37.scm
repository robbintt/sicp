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
; Lets isolate the try method from the cont-frac method.
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

; a general iterative convergence detector for f.
; it does reuse close-enough? from before.
; it is necessary to re-tool the close-enough and function definition
; to accomodate the inputs of you are trying to detect.
; 
; it would probably be better to pass f-wrapper which would pack all arguments
; of f except the guess. however, for this we need to use closures.
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

;;; FINALLY we need to turn cont-frac into a recursive/iterative function. analyze the
;;; method written above and do the other way.


; an iterative function will pass the result each time
; a little help from bill the lizard. for some reason i couldn't get my head
; around the beginning state of the iterator. it's obvious written out. most things are.
(define (cont-frac-iter n d k) 
  (define (frac-iter k result)
    (let ((n-value (n k))
          (d-value (d k)))
      (if (= k 1)
        (/ n-value (+ d-value result))
        (frac-iter (- k 1) (/ n-value (+ d-value result))))))
  (frac-iter (- k 1) (/ (n k) (d k))))

(cont-frac-iter phi-n phi-d 40)

; this seems to hang, probably got an oscillator in the iterative function?
;(convergence-detector phi-n phi-d cont-frac-iter 1 increment)
; It is verified verified, (= k 1) will hang.  if you make one attempt it 
; will need a special case for (= k 0) due to design.  The special case 
; would return just (/ (n k) (d k)).
(convergence-detector phi-n phi-d cont-frac-iter 2 increment)


