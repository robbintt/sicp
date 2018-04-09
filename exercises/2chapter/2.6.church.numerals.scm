;; in a language that can manipulate procedures we can get by without numbers
;; at least so far as non-negative integers are concerned
;; by implementing 0 and then implementing the operation of adding 1 as:
(define zero (lambda (f) (lambda (x) x)))

;; church numerals after alonzo church who invented lambda calculus
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; 1 in terms of zero, add-1
;; note that this returns a function defining 1
(display (add-1 zero))

;; problem: define one and two directly (not in terms of zero and add-1)
;; hint: use substitution to evaluate (add-1 zero) and write it (trent: write it in terms derived from above)
;; Give a direct definition of the addition procedure + (not in terms of repeated application add-1)
(define one (lambda (f) (lambda (x) (f ((lambda (g) (lambda (y) y)) f) x))))
;; how do i even test this? i am pretty sure this is right but how do i actually use it...

;; i guess for `two` i just substitute (add-1 one) and write it out. not too bad but important to keep the parentheses straight...
;; the lesson here will maybe be about reducing repeated add-1 functions in an interesting way???
(define two (2))


;; give a direct definition of the addition procedure +
;; not in terms of repeated application of add-1
;; this will probably be apparent once you see `one` and `two` next to each other.
(define (add a b)
  )
