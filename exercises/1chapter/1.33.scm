
;;; write filtered-accumulate as a procedure

;;; filter methods will have a spec
;;;   filter methods take one value to test
;;;   filter methods return true or false

;;;
;;; PRIME TEST
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

;;; USE AS FILTER FOR filtered-accumulate
(define (comprehensive-prime? n) (fast-prime? n (- n 1)))
;;; 
;;; END PRIME TEST


;;;
;;; GCD TEST
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (gcd-test? a b)
  (if ((= (gcd a b) 1) true) false))

;;;
;;; END GCD TEST

;;; a filter with no constraints
(define (empty-filter x) true)

(define (filtered-accumulate combiner null-value filter-method term a next b)
  (cond ((> a b) null-value)
         ((filter-method a) (combiner (term a)
                         (filtered-accumulate combiner null-value filter-method term (next a) next b)))
         (else (combiner null-value
                         (filtered-accumulate combiner null-value filter-method term (next a) next b)))))

;;; utility procedures
(define (identity x) x)
(define (inc x) (+ x 1))

;;; product in terms of filtered-accumulate
(define (product term a next b)
  (filtered-accumulate * 1 empty-filter term a next b))

;;; sum in terms of filtered-accumulate
(define (sum term a next b)
  (filtered-accumulate + 0 empty-filter term a next b))

(define (factorial x)
  (product identity 1 inc x))
(factorial 3)

(define (sigma x)
  (sum identity 1 inc x))
(sigma 3)

;; 1.33a
(define (sum-over-squares-of-primes x)
  (filtered-accumulate + 0 comprehensive-prime? square 1 inc x))

(sum-over-squares-of-primes 1)
(sum-over-squares-of-primes 2)
(sum-over-squares-of-primes 3)
(sum-over-squares-of-primes 4)
(sum-over-squares-of-primes 5)

;; 1.33b
(define (gcd-test-maker a)
  (define (special-gcd-test? b)
    (if (= (gcd a b) 1) true false))
  special-gcd-test?)

(define (special-product x)
  (define (relative-prime-product n)
    (filtered-accumulate * 1 (gcd-test-maker n) identity 1 inc n))
  (relative-prime-product x))

(special-product 1)
(special-product 2)
(special-product 3)
(special-product 4)
(special-product 5)
(special-product 6)
(special-product 7)


