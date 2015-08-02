;;; special case of newtons sqrt algorithm method called by heron of alexandria

;; note: radicand is the number you are finding the square root of.
;;
;; note2: sqrt-iter gives an example of iteration with no special construct
;; other than the ordinary ability to call a procedure (SICP p.25)


; Give it something to chew on, set the first guess to 1.0.
(define (sqrt x) 
  (define (square x) (* x x))
  (define (average x y) (/ (+ x y) 2))
  (define (improve guess radicand)
    (average guess (/ radicand guess)))
  (define (good-enough? guess radicand) 
    (< (abs (- (square guess) radicand)) 0.00001))
  ; main method, use the control flow
  (define (sqrt-iter guess radicand)
    (if (good-enough? guess radicand)
      guess
      (sqrt-iter (improve guess radicand)
                   radicand)))
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))
(sqrt 0.002)
