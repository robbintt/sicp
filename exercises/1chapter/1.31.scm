;;;
;;; a. write an analogous procedure called product
;;; show how to write factorial in terms of product
;;; use product to compute approximations to pi using
;;; the formula in the problem.
;;;

;;; utility procedures
(define (identity x) x)
(define (inc x) (+ x 1))

;;; PRODUCT: RECURSIVE
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (factorial x)
  (product identity 1 inc x))

(factorial 3)
(factorial 4)
(factorial 5)

;;; collect the terms to approximate pi
(define (pi-prod k)
  (define (next-result n)
    (cond ((and (= n k) (even? n)) n)
          ((and (= n k) (odd? n)) (/ 1 n))
          ((even? n) (square n))
          ((odd? n) (/ 1 (square n)))))

  (* 2 (product next-result 3 inc k)))

;(* 4 (pi-prod 1000))
(exact->inexact (* 4 (pi-prod 1000)))
;(* 4 (pi-prod 1001))
(exact->inexact (* 4 (pi-prod 1001)))

;;;
;;; b. if a was iterative, write product recursively,
;;;    if a was recursive, write product iteratively
;;;  

;;; SUM: ITERATIVE
(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* a result))))
  (iter a 1))

(define (iter-factorial x)
  (iter-product identity 1 inc x))

(iter-factorial 3)
(iter-factorial 4)
(iter-factorial 5)


