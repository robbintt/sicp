; 2.1.1 Notes
;

; from 2.0

; linear combination for numbers
; assuming + and * have not been remapped from mit-scheme defaults
(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

; linear combination abstraction 
; for "rational numbers, complex numbers, polynomials, or whatever."
; where add and mul perform appropriate operations for (a b x y)
(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))


; 2.1.1 Arithmetic Operations for Rational Numbers

; selectors and constructors implement abstract data in terms 
; of the concrete representation.

; assume: we already have a way to construct (a constructor) a rational 
; number from a numerator and a denominator.
; also assume: we have a way of extracting (or selecting) its 
; numerator and denominator
; then we could express the relations on page 84

; constructor
; (make-rat n d) ; returns the rational number with the numerator integer n and denominator integer d

; selector
; (numer x) ; returns numerator of rational number x
; (denom x) ; returns denominator of rational number x

; note that these are not aliases to cons, car, cdr BUT serve as aliases
; this has an advantage that debugging is clear as it traces the route through this function
; cons takes exactly 2 arguments
; car and cdr take exactly 1 argument
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

; we can also define these strictly as aliases
; this is more efficient because they are aliases instead of calling cons, car, cdr
; but worse for debugging because there are way more calls to cons, car, cdr and 
; you will have a hard time finding the cons, car, cdr relevant to your issue
; (define make-rat cons)
; (define numer car)
; (define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
  


;; procedures from 2.1.1
;;

; add two rational numbers: add-rat
(define (add-rat x y)
  (make-rat (+ (* (numer y) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

; subtract two rational numbers: sub-rat
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

; multipy two rational numbers: mul-rat
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

; divide two rational numbers: div-rat
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

; test equality of two rational numbers: equal-rat?
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; tests

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
;(print-rat (mult-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(print-rat (sub-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (div-rat one-half one-third))
(equal-rat? one-half one-third)


; we can use gcd to have make-rat reduce the rational number to lowest terms
; there is a built in gcd
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third))

; at this point the make-rat is explicitly the reducing one
