

;;; We haven't seen exp yet.
(define (square x)
  ;;; Also haven't seen log before
  (exp (double (log x))))

(define (double x) (+ x x))


(square 5)
