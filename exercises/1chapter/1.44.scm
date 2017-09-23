; write a n-fold 'smooth' function by implementing 'smooth' and applying 'repeated' from 1.43

(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (average x y z) (/ (+ x y z) 3))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated proc x)
  (cond ((= x 1)
         proc)
        (else (compose proc (repeated proc (- x 1))))))


; use fixed dx from examples...
(define dx 0.00001)

; 'smoothing' is averaging f(x), f(x-dx), and f(x+dx)
(define (smooth f)
  (lambda (x) 
    (average (f x) (f (- x dx)) (f (+ x dx)))))

; define a general method for retrieving the n-th smoothed function
(define n-fold-smoothed-function (lambda (x) (repeated smooth x)))

dx
n-fold-smoothed-function

; my usage sample, note that this generates the 5-fold-smoothed function.
; to use the function you would need to wrap it again and pass it f
(n-fold-smoothed-function 5)

; total use case - smooth the square function five times and use it on the value 2.
; this isn't a great test case, lets defer to bill the lizard
(((n-fold-smoothed-function 1) square) 6.48074)
(((n-fold-smoothed-function 2) square) 6.48074)
(((n-fold-smoothed-function 3) square) 6.48074)
(((n-fold-smoothed-function 4) square) 6.48074)
(((n-fold-smoothed-function 5) square) 6.48074)
(((n-fold-smoothed-function 10) square) 6.48074)

; bill's test case:
(define pi 3.14)

; bill uses a high dx, lets replicate his numbers exactly
(define dx 0.70)

; test to make sure everything is working... it is
(sin (/ pi 2))
(sin 0)
(sin pi)
((smooth sin) (/ pi 2))
((smooth sin) pi)

; these values are off, presumably the implementations are different
; the comments say bill the lizard's answer is wrong.
; FURTHER NOTES: this seems to be a pretty contested answer.
(((n-fold-smoothed-function 1) sin) (/ pi 2))
(((n-fold-smoothed-function 2) sin) (/ pi 2))
(((n-fold-smoothed-function 3) sin) (/ pi 2))
(((n-fold-smoothed-function 4) sin) (/ pi 2))

; answer seems right: see square of 6.48074 on lines 37-42
; i found this output to check against my smoothing and implemented the square above
; http://jots-jottings.blogspot.com/2011/09/sicp-exercise-144-smoothed-functions.html
