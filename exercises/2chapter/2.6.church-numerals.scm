;; in a language that can manipulate procedures we can get by without numbers
;; at least so far as non-negative integers are concerned
;; by implementing 0 and then implementing the operation of adding 1 as:
(define zero (lambda (f) (lambda (x) x)))

;; church numerals after alonzo church who invented lambda calculus
(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))
    ))

;; 1 in terms of zero, add-1
;; note that this returns a function defining 1
(display (add-1 zero))
(add-1 zero)


;; I didn't get this one, I want to come back to it. Try these methods:
;; 1. redefine one, two in terms of just f and x,
;; 2. reduce one and two more so that their definitions are clearer

;; 2021-11-26 two years later, i have some time:

;; formal definitions, which are nowhere in the book, per the concept of church numerals
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; now lets try a generic function for adding any church numerals
(define (add-chn j k) (lambda f (lambda (x) ((j k) x))))
;; addition should be commutative since the same system of church numerals should use the same definition of add
(define (add-chn j k) (lambda f (lambda (x) ((k j) x))))
