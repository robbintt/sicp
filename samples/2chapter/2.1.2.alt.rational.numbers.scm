; 2.1.2 Notes
;
; Alternately, we can perform rational number (fraction) reduction to lowest terms
; whenever we access the parts of the rational number, rather than during construction
; in other words, reduction can happen in the selectors instead of constructor
; because our abstraction for add-rat, sub-rat, mul-rat, div-rat are above this abstraction
; layer, it is not necessary to change them


; constructor
; (make-rat n d) ; returns the rational number with the numerator integer n and denominator integer d

; selector
; (numer x) ; returns numerator of rational number x
; (denom x) ; returns denominator of rational number x

(define (make-rat n d) (cons n d))
(define (numer x) 
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x) 
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

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

(define two-thirds (make-rat 6 9))

;; test that this is reduced
(print-rat two-thirds)
