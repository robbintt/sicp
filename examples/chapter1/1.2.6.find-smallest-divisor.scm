;;;
;;; Test primality by finding the smallest divisor of a number.
;;;

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 43)
(prime? 101)
(prime? 1729)
(prime? 117)

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
