;;;
;;; Implement: The Fermat Test
;;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
         (else
           (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 1000 100)
(fast-prime? 1999 1000)
(fast-prime? 2000 1000)
(fast-prime? 2001 1000)

;;; 1729 is not prime but passes the fermat test. this isn't a good test case
(fast-prime? 1729 1000)
