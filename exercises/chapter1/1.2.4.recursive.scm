;;; SICP Says: linear recursive, theta(n) steps and theta(n) space.
;;;
;;; NOT branching (only one call per depth)
;;; Depth increases linearly as n is incremented (linear).
;;; Operations increase linearly as n is incremented (expt is called repeatedly
;;; once depth is reached, once for each level of depth).

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))


(expt 10 2)

(expt 5 3)
