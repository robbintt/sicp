;;;
;;; now iterative
;;;
;;; definitions:
;;;
;;; counter starts at 1, product starts at 1
;;; first multiply the counter by the product, then increment the counter.
;;; factorial is the value of the product where the counter exceeds n.
;;; (test the counter, if greater than n return the factorial, else repeat the process.

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      ; we're still using recursion to do iteration, lol #justlispthings ??
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1)) ; always start at 1

(factorial 5)
