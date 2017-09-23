
;;; Observe that our model of evaluation allows for
;;; combinations whose operators are compound expressions.
;;; Use this observation to describe the behavior of the
;;; following procedure:


(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;; some tests
(a-plus-abs-b 1 -1)
(a-plus-abs-b 1 -100)
(a-plus-abs-b 100 -100)

;;; Define this behavior in some notes below:
;;;
;;; This returns an operator in place,
;;; if b>0 then (+ a b) is evaluated
;;; else (- a b) is evaluated.
;;;
;;; This is pretty tricky!
;;;
;;; Key Insight: An operator can come from a compound expression.

;;; As an exercise lets just try defining the abs procedure with this knowledge.
(define (abs x) ((if (< x 0) - +) x))

(abs -100)
(abs 100)
(abs 0)



