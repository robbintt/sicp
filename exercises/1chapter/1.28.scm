;;;
;;; Implement: The Miller-Rabin Test
;;; This implementation is modified from the Fermat Test in problem 1.27.
;;;
;;; We will keep our modified test (test for all integers < n) so that we can
;;; see the Miller-Rabin Test reject the carmichael numbers.
;;;
;;; Goal: Modify expmod to signal if it discovers a nontrivial square root of 1 modulo n
;;; Miller Rabin Test: a to the (n-1) power is congruent to 1 modulo n.
;;;
;;; Well, what is a nontrivial square root of 1 modulo n?
;;; A: a number not equal to 1 or n-1 whose square is equal to 1 modulo n. (from the text)
;;; 
;;;
;;; HINT: One convenient way to signal is to return 0 in expmod.
;;; Why is this a hint? Well, if we return 0, it short circuits the recursion and expmod 
;;; returns 1 which can never equal a, thus failing the test.
;;; 

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (root-test (expmod base (/ exp 2) m) m) m))
         (else
           (remainder (* base (expmod base (- exp 1) m)) m))))

;;; return (square a) unless it finds a nontrivial square root of 1 modulo n
;;; in the latter case, return 0, short circuiting the expmod function.
(define (root-test a n)
  (cond ((= a 1) (square a))
        ((= a (- n 1)) (square a))
        ((= (remainder (square a) n) 1) 0)
        (else (square a))))


(define (miller-rabin-test n a)
    (= (expmod a n n) a))

(define (fast-prime? n times)
  (cond ((= times 1) true)
        ((miller-rabin-test n times) (fast-prime? n (- times 1)))
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

;;; more real primes
(comprehensive-prime? 3)
(comprehensive-prime? 5)
(comprehensive-prime? 7)
(comprehensive-prime? 41)
(comprehensive-prime? 43)

