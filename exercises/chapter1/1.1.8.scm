

;;; We haven't seen exp yet.
(define (square x)
  (exp (double (log x))))

(define (double x) (+ x x))


(square 5)
