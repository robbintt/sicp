
;;; show that sum and product are special cases of a still more general
;;; notion called 'accumulate' that combines a collection of terms,
;;; using some general accumulation function


;;; ACCUMULATE: recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
       (product term (next a) next b))))

;;; utility procedures
(define (identity x) x)
(define (inc x) (+ x 1))

;;; product in terms of accumulate
(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial x)
  (product identity 1 inc x))

(factorial 3)
(factorial 4)
(factorial 5)

;;; ACCUMULATE: iterative

(define (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner a result))))
  (iter a null-value))

(define (iter-product term a next b)
  (iter-accumulate * 1 term a next b))

(define (iter-factorial x)
  (iter-product identity 1 inc x))

(iter-factorial 3)
(iter-factorial 4)
(iter-factorial 5)


