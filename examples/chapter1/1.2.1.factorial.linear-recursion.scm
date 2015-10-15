;;; n!, n * (n-1) * (n-2) ... 3 * 2 * 1
;;;
;;; The expansion for this is one multiplication per number in the sequence above.
;;; See figure 1.3, SICP

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(factorial 5)
