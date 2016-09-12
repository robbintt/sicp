; 1.45 empirically build a n-damping method for building n-root finding methods.
;
;

; utility functions from 1.43, 1.44
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (average x y) (/ (+ x y) 2))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated proc x)
  (cond ((= x 1)
         proc)
        (else (compose proc (repeated proc (- x 1))))))


; arbitrary, reasonable for problem
(define tolerance 0.00001)

; 1.3.3 fixed point
; A fixed point comes from applying f(x) for a given x continuously until 
; the value does not change very much.
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


; define a general method for retrieving the n-th smoothed function
(define n-fold-average-damp (lambda (x) (repeated average-damp x)))

n-fold-average-damp
(n-fold-average-damp 2)


; average damping sqrt
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(sqrt 9)

; average damping cuberoot
(define (cuberoot x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y))))
               1.0))
(cuberoot 27)

; needs two damps, lets implement repeated damping
(define (four-root x)
  (fixed-point ((n-fold-average-damp 2) (lambda (y) (/ x (* y y y))))
               1.0))
(four-root 81)

; so lets try a 5-root
(define (five-root x)
  (fixed-point ((n-fold-average-damp 2) (lambda (y) (/ x (expt y 4))))
               1.0))
(five-root (expt 3 5))

; 6-root
(define (six-root x)
  (fixed-point ((n-fold-average-damp 2) (lambda (y) (/ x (expt y 5))))
               1.0))
(six-root (expt 3 6))

; 7-root
(define (seven-root x)
  (fixed-point ((n-fold-average-damp 2) (lambda (y) (/ x (expt y 6))))
               1.0))
(seven-root (expt 3 7))

; 8-root
; diverges with 3 damps
(define (eight-root x)
  (fixed-point ((n-fold-average-damp 3) (lambda (y) (/ x (expt y 7))))
               1.0))
(eight-root (expt 3 8))

; 9-root
(define (nine-root x)
  (fixed-point ((n-fold-average-damp 3) (lambda (y) (/ x (expt y 8))))
               1.0))
(nine-root (expt 3 9))

; some data on required damps:
; root  damps
; 2     1
; 3     1
; 4     2
; 8     3
;
; we clearly need to programatically search for data, it is too slow building each function
;
; lets generalize to n-th root now...
; we know we need to increment the number of average damps following some algorithm
; number of damps is assumed to be a dependent variable of number of root being performed.
; we also need to pass a n for nth root, so we need to calculate an n-th power.
; we can use (expt a b) for a^b
;
; lets use the proposed function in this form:
; (n-root n)
; where is the n-th root the user requests...

(define (n-root-fixed-damp n)
  (lambda (x)
    ; use let later to define the number of damps in terms of n
    (fixed-point ((n-fold-average-damp 4) (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

(define (n-root-tester j n)
  (expt j n))

(define (root-test-iterator n)
  (newline)
  (display ((n-root-fixed-damp n) (n-root-tester 2 n)))
  (if (= n 0) n (root-test-iterator (- n 1))))

(root-test-iterator 31)

; more data on damping
; now we have a great pattern.
; could only establish the pattern by finding roots of powers of 2.
; Floating-point overflows occurred above around 15 on numbers 3 through 9
; roots damps
; 2     1
; 4     2
; 8     3
; 16    4
; 32    5

; procedure to quickly find the base of a particular number
; for n=1, if 2^n > n, damps = n, else n+=1 and try again
(define (power-bounder n b x)
  (if (< (expt b n) x) (power-bounder (+ n 1) b x) n))

(power-bounder 1 2 33)

; final simple n-root method

(define (n-root n)
  (lambda (x)
    ; use a let here to define the number of damps in terms of n
    (fixed-point ((n-fold-average-damp (power-bounder 1 2 x)) (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

((n-root 32) (expt 2 32))


