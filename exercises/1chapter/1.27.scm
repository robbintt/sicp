;;;
;;; Implement: The Fermat Test
;;; This implementation is modified for problem 1.26.
;;;
;;; Instead of random, we iterate over all numbers below n.
;;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
         (else
           (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (fast-prime? n times)
  (cond ((= times 1) true)
        ((fermat-test n times) (fast-prime? n (- times 1)))
        (else false)))

;;; fast prime over all n
(define (comprehensive-prime? n) (fast-prime? n (- n 1)))

;;; if fast-prime? yields #t, then prime.
(comprehensive-prime? 1999)
(comprehensive-prime? 2000)

;;; carmichael numbers:
(comprehensive-prime? 561)
(comprehensive-prime? 1105)
(comprehensive-prime? 1729)
(comprehensive-prime? 2465)
(comprehensive-prime? 2821)
(comprehensive-prime? 6601)
