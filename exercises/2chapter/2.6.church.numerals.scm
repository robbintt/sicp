;; in a language that can manipulate procedures we can get by without numbers
;; at least so far as non-negative integers are concerned
;; by implementing 0 and then implementing the operation of adding 1 as:
(define zero (lambda (f) (lambda (x) x)))

;; church numerals after alonzo church who invented lambda calculus
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; problem: define one and two directly (not in terms of zero and add-1)
;; hint: use substitution to evaluate (add-1 zero)
;; Give a direct definition of the addition procedure + (not in terms of repeated application add-1)


