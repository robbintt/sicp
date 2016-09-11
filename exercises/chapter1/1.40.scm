; 1.3.4 Notes
; 
; Newtons method uses a fixed point, but first we should define derivative
; 
; Newtons method is used to find a square root at the bottom

; deriv takes a procedure as an argument and returns a procedure as a value
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
          dx)))

(define dx 0.00001)


; yield the derivative of the cube of 5
(define (cube x) (* x x x))
((deriv cube) 5)

; now use newton's method as a fixed point process
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
; the method takes a method, g, we want to find a 
; nearby zero on, and a guess about where it is.
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; define sqrt in terms of the zero of y |-> y*y - x
; start with an initial guess of 1
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

; this should return a function which is a cubic of the requested form
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))



;
;
; Reused code from previous parts of the chapter
;
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

; Average damping can be generalized like so:
(define (average-damp f)
  (lambda (x) (average x (f x))))


(sqrt 25)


; the cubic method should accomodate this:
; note: roots were verified visually with wolfram alpha
; http://www.wolframalpha.com/input/?i=graph++x%5E3%2B2x%5E2-1x-1+for+%5B%2F%2Fquantity:-10%2F%2F%5D+%3C%3D+x+%3C%3D+%5B%2F%2Fquantity:10%2F%2F%5D+and+%5B%2F%2Fquantity:-10%2F%2F%5D+%3C%3D+y+%3C%3D+%5B%2F%2Fquantity:10%2F%2F%5D 
(define a 2)
(define b -1)
(define c -1)
(newtons-method (cubic a b c) 1)
(newtons-method (cubic a b c) -1)
(newtons-method (cubic a b c) -2)
