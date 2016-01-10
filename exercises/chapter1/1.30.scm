
;;; demonstrate how the sum procedure can be
;;; written with iteration instead of linear recursion


;;; REFERENCE: recursive definition
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))


(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ a result))))
  (iter a 0))


(define (identity x) x)
(define (inc x) (+ x 1))


(define (sum-integers a b)
  (sum identity a inc b))

(define (iter-sum-integers a b)
  (iter-sum identity a inc b))

(sum-integers 0 10)

(iter-sum-integers 0 10)
