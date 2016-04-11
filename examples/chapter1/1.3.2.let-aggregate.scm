
; compare the 3 following:

; define an internal function then call it 
; note that the value of the final expression is returned, in this case the value
; of f-helper
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

; call the lambda in place, feeding it x and y at the end
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; above, the wrapped expression that feeds variables and calls the lambda is 
; replaced by the let syntax.
; this still returns the anonymous function in the expression of f, assigning
; the anonymous function specified by the let to f.
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
