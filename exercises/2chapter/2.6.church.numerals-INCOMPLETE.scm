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

;; What is the lesson here?
;; idea: the lesson here will maybe be about reducing repeated add-1 functions in an interesting way???

;; problem: define one and two directly (not in terms of zero and add-1)
;; hint: use substitution to evaluate (add-1 zero) and write it (trent: write it in terms derived from above)
;; Give a direct definition of the addition procedure + (not in terms of repeated application add-1)
;; what does DIRECT DEFINITION mean, i guess only functions?

;; i changed the variables in the wrapping add-1 so i can more easily trace, but it might be causing confusion?
(define one 
  (lambda (g) 
    ;; this is the add-1 function substitution once on zero
    (lambda (y) 
      (g ((lambda (f) (lambda (x) x)) g) y))))
;; how do i even test this?
(add-1 one)

;; i guess for `two` i just substitute (add-1 one) and write it out. not too bad but important to keep the parentheses straight...
(define two 
  (lambda (h) 
    (lambda (z) 
      ;; this is the add-1 function substitution twice on zero, or the function one
      (h (((lambda (g) (lambda (y) (g ((lambda (f) (lambda (x) x)) g) y))) h) z)))))


;; how is (one one) different than (add-1 one)

;; give a direct definition of the addition procedure +
;; not in terms of repeated application of add-1
;; this will probably be apparent once you see `one` and `two` next to each other.
;; the challenge here is adding e.g. 1+2 to get 3, without applying add-1 2x to (one)
;; the zero can always come from the left side, how do we define addition without repeating add-1?
;; again direct definition probably means substitute add-1's contents
(define (add a b)
  (+ a b))

(define (add-b a b)
  ;; e.g. iterate over this self-referentially, n times
  ;; however, b will be in terms of the lambda calculus.
  ;; if we feed b into a, what do we get?
  ;; if we feed zero into zero, what do we get?
  (lambda (f) (lambda (x) (f ((n f) x)))))


;; I didn't get this one, I want to come back to it. Try these methods: 
;; 1. redefine one, two in terms of just f and x,
;; 2. reduce one and two more so that their definitions are clearer



