;;; This is the iterative exponentiation algorithm
;;; SICP - theta(n) in steps and theta(1) in space.
;;;
;;; It is constant in space because all state is stored in the counter.
;;; How is this different than the previous example?
;;;
;;; TAR 081215 - this process unrolls the power to multiplication in expt-iter.
;;; The iterator has a depth of counter (counter=n) and starts with a product of 1.
;;; The iterator goes 'counter' steps deep and returns the product to itself.
;;;
;;; This stores no extra states for each depth, so it is theta(1) in space.
;;; Depth is linear with proportion to n, so steps is theta(n)




(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product))))

(expt 10 2)
(expt 5 3)
