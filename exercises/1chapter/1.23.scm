;;;
;;; Test primality by finding the smallest divisor of a number.
;;; THIS VERSION MODIFIED FOR PROBLEM 1.23
;;;

;;; 1.23 EXPLANATION PORTION (SEE LAST SENTENCE OF PROBLEM)
;;; PROBLEM ASSUMES: Halves test steps. Expect twice as fast.
;;; Is this confirmed? If not, what is the observed ratio?
;;; This problem, directly compared against 1.22 yields a ratio of 1/1.7 on the highest prime.
;;; Overhead from the next function occurs EVERY TIME we get the next test-divisor.
;;; Having a function inside a recursive method has a major impact.

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;;
;;; (prime? 43)
;;; (prime? 101)
;;; (prime? 1729)
;;; (prime? 117)
;;; 
;;; (smallest-divisor 199)
;;; (smallest-divisor 1999)
;;; (smallest-divisor 19999)
;;; 

;;;
;;; 1.22 - write search-for-primes which checks primality of consecutive
;;;         odd integers in a specified range. odd? is builtin.
;;; THIS VERSION MODIFIED FOR 1.23 - CORE CODE ABOVE, SCAFFOLDING BELOW (MODIFIED FROM EXAMPLE)
;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
         (report-prime (- (runtime) start-time)))
         (else #f)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
  #t

;;; find the next prime starting at i

(define (search-for-primes i)
  (cond ((< i 2) 
         (search-for-primes 2))
        ((even? i)
         (search-for-primes (+ i 1)))
        (else 
          (cond ((timed-prime-test i) i)
                (else (search-for-primes (+ i 1)))))))

  
;;; use your procedure to find the following:

;;; 3 smallest primes > 1000
(search-for-primes 1000)
(search-for-primes (+ (search-for-primes 1000) 1))
(search-for-primes (+ (search-for-primes (+ (search-for-primes 1000) 1)) 1))

;;; 3 smallest primes > 10000
(search-for-primes 10000)
(search-for-primes (+ (search-for-primes 10000) 1))
(search-for-primes (+ (search-for-primes (+ (search-for-primes 10000) 1)) 1))

;;; 3 smallest primes > 100000
(search-for-primes 100000)
(search-for-primes (+ (search-for-primes 100000) 1))
(search-for-primes (+ (search-for-primes (+ (search-for-primes 100000) 1)) 1))

;;; 3 smallest primes > 1000000
(search-for-primes 1000000)
(search-for-primes (+ (search-for-primes 1000000) 1))
(search-for-primes (+ (search-for-primes (+ (search-for-primes 1000000) 1)) 1))

;;; i kept going because computers are much faster now
;;; in order to approximate the difference in theta between two orders, i first
;;; must find a sufficiently large order of magnitude for a modern system
;;; 3 smallest primes > 100000000000
(search-for-primes 100000000000)
(search-for-primes (+ (search-for-primes 100000000000) 1))
(search-for-primes (+ (search-for-primes (+ (search-for-primes 100000000000) 1)) 1))

;;; this is indeed sqrt(10) ~ 3x slower than one order of magnitude lower.
;;; 3 smallest primes > 1000000000000
(search-for-primes 1000000000000)
(search-for-primes (+ (search-for-primes 1000000000000) 1))
(search-for-primes (+ (search-for-primes (+ (search-for-primes 1000000000000) 1)) 1))
