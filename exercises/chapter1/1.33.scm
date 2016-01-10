
;;; write filtered-accumulate as a procedure

;;; filter methods will have a spec
;;;   filter methods take one value to test
;;;   filter methods return true or false

;;; a filter with no constraints
(define (empty-filter x) #t)

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
(factorial 4)
(factorial 5)

(define (sigma x)
  (sum identity 1 inc x))

(sigma 3)
(sigma 4)
(sigma 5)

