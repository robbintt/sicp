
;;; 1.24 answer:
;;; ============
;;; I modified the number of tests to 100,000 in order to check the first 3 primes
;;; after 1,000 against the first 3 primes after 1,000,000. This worked and I got 
;;; this data:
;;; 1000 => 2.71 seconds
;;; 1000000 => 4.8 seconds.
;;; Important: we expect log(n) growth.
;;; ln(1,000) = 6.90
;;; ln(1,000,000) = 13.82
;;; 13.82 / 6.9 = 2.0, 4.25/2.80 = 1.51
;;; We were faster than the prediction. why?
;;; The discrepancy (25% faster): 
;;;     (there is always some discrepancy from application overhead)
;;;
;;; Idea 1:
;;;     The relevant log to take is the mean of the range of the random
;;;     number. for example, for 1000, it would be ln(500) and for 1000000
;;;     it would be ln(500000). Lets try these numbers.
;;; ln(500000)/ln(500) = 13.12/6.21 = 2.11
;;; in retrospect, this can be simplified with algebra to show it is not the cause.
;;; 
;;; Idea 2:
;;; It could be the difference in primes, since the first 3 primes are discrete data,
;;; higher primes would cause proportionally more calculations.
;;; unfortunately the primes after 1,000,000 are further from 1,000,000 than the primes
;;; after 1,000. Not helpful.
;;;
;;; Observations:
;;;     expmod searches down in 
;;;     fast-prime? runs fermat-test (times) times.
;;;     fermat-test runs expmod once.
;;;     test scaffolding does not add significant overhead (?)
;;;
;;; Idea 3:
;;; For the first 3 primes after 1000, 19/2 = 9 values were tested.
;;; For the first 3 primes after 1000000, 37/2 = 19 values were tested.
;;; even though double the values were tested, it still took less time
;;; than expected. This discrepancy is surprising.
;;; we know that the primes are expected to fail after a single test,
;;; the fully tested numbers are as follows:
;;; (fast-prime? 1000) yields 6 fully tested nonprimes
;;; (fast-prime? 1000000) yields 16 fully tested nonprimes.
;;; 16/6 = 2.67 times as many values.
;;; this would imply that the 1000000 range should take LONGER, but it was shorter.
;;; 
;;; PROBLEM:
;;; To verify Idea 3, I need to do fewer tests per prime, because modern processors store
;;; calculations they are likely to repeat in their cache, screwing up my measurements.
;;;
;;; FINALLY SOLVING IT:
;;; 1000 tests per prime (rather than the 100,000 in the data above)
;;; 3 primes after 1000 => 0.03 seconds
;;; 3 primes after 1000000 => 0.11 seconds
;;; This seems much closer to the expectations of idea 3. Instead of double, this took .11/.03 = 3.66
;;; SOLVED - see idea 3.
;;;
;;; SOLVED. I'm going to declare this solved, this problem is much harder for me than my predecessors.

;;;
;;; Modify problem 1.22 to use fermat's 'fast-prime?' (the fermat method).
;;; THIS VERSION MODIFIED FOR PROBLEM 1.24
;;; (no modifications yet)
;;;
;;;
;;; Implement: The Fermat Test
;;; example from 1.2.6
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
;;; >1. prime? now takes 2 parameters. since the problem does not specify the
;;; number of times (precision) of the fermat test, we set times equal to
;;; the number we are testing. in practice, times would either need to detect
;;; retests and avoid them or use a bell curve to determine precision. for
;;; example, 65% accuracy is achieved by this test. if we tested 1.67*n then
;;; we would achieve 95% accuracy, etc. (see topic: 'bell curve' for more info).
;;; >2. changed name of prime? to fast-prime? to accomodate above method.
;;; >3. change #1 did not work. also, it makes the problem hard to compare.
;;; instead we can use a fixed value for times. i think this is dumb but we can try it.
;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n 100) (report-prime (- (runtime) start-time)))
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
(search-for-primes 100000000000000)
(search-for-primes (+ (search-for-primes 100000000000000) 1))
(search-for-primes (+ (search-for-primes (+ (search-for-primes 100000000000000) 1)) 1))
