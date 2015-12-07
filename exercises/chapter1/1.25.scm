
;;; Problem 1.25
;;; Alyssa P Hacker thinks we can use fast-expt to save time in the expmod procedure implemented
;;; in the fermat test implementation provided by the book.
;;; Can we?
;;;
;;; Here is her implementation:
(define (alternate-expmod base exp m)
  (remainder (fast-expt base exp) m))


;;;
;;; Answer:
;;; Sure, it COULD be used.  As long as we modulo a^n, the fermat test will occur.
;;; See footnote 46, the procedure expmod (below) uses a remainder every step of the
;;; way. This keeps numbers near the order of whatever m is.
;;; The basic concept is x*y % m = ((x%m)*(y%m)) % m .  Why they slipped this in here,
;;; I do not know. Maybe computers were too slow to do this the naive way first when
;;; the book was written.


;;;
;;; fast-expt
;;; For space and steps theta(n) = log(n)
;;;

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(fast-expt 10 2) ;; 100
(fast-expt 5 3) ;; 125

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

