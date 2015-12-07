;;;
;;; Implement: The Fermat Test
;;; example from 1.2.6
;;;


(fast-prime? 1000 100)
(fast-prime? 1999 1000)
(fast-prime? 2000 1000)
(fast-prime? 2001 1000)

;;; 1729 is not prime but passes the fermat test. this isn't a good test case
(fast-prime? 1729 1000)

;;;
;;; Modify problem 1.22 to use fermat's 'fast-prime?' (the fermat method).
;;; THIS VERSION MODIFIED FOR PROBLEM 1.24
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

(define (prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

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
;;; test scaffolding taken from 1.22 for problem 1.24, no changes expected.
;;; CHANGES:
;;; prime? now takes 2 parameters. since the problem does not specify the
;;; number of times (precision) of the fermat test, we set times equal to
;;; the number we are testing. in practice, times would either need to detect
;;; retests and avoid them or use a bell curve to determine precision. for
;;; example, 65% accuracy is achieved by this test. if we tested 1.67*n then
;;; we would achieve 95% accuracy, etc. (see topic: 'bell curve' for more info).
;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n n) (report-prime (- (runtime) start-time)))
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
