;; Show we can represent pairs of nonnegative integers
;; using only numbers and arithmetic operations
;; if we represent the pair a and b as the integer that is
;; the product (2^a)*(3^b)
;; 
;; Give the corresponding definition of the procedures:
;; cons, car, cdr

;; we can only get multiples of 2 and/or 3

;; when do powers overlap? since odd*odd=odd and even*even=even, they do have no common powers.
;; we may be able to use this to get some information, likely just as an optimization if at all.

;; represent the above pairs as the integer product (cons)
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;; basis for car and cdr
;; count the number of times val can be divided by the factor
(define (get-basis val factor count)
  (if (= 0 (remainder val factor))
      (get-basis (/ val factor) factor (+ 1 count))
      count))

;; get the 2 portion of the factor (car)
;; we used an exercise to do something involving roots in chapter 3.
;; Divide by one factor until the modulus is not zero (until it does not evenly divide)
;; Then divide by the next factor until you reach 1
;; Tally each factor to get the car or the cdr
(define (car product)
  (get-basis product 2 0))

(define (cdr product)
  (get-basis product 3 0))
      

(cons 0 0) ;; 1
(cons 1 0) ;; 2
(cons 0 1) ;; 3
(cons 1 1) ;; 6

(car (cons 0 0))
(cdr (cons 0 0))

(cons 2 0) ;; 4
(cons 0 2) ;; 9
(cons 2 1) ;; 12
(cons 1 2) ;; 18
(cons 2 2) ;; 36

(car (cons 2 2)) ;; 2
(cdr (cons 2 2)) ;; 2


(cons 3 0) ; 8
(cons 0 3) ; 27

(car (cons 3 0)) ;; 3
(cdr (cons 3 0)) ;; 0

(cons 3 1) ; 24
(cons 1 3) ; 54

(cons 4 0) ; 16
