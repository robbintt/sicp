;;; In short - why is 'if' a special form if we can easily define it?


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

;;; Actual question: What would happen to the heron sqrt method if we used this?
;;;
;;; Here is Alyssa's rewritten portion of the sqrt program:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
;;; Answer: This will unwind the new-if statement using applicative-order
;;; evaluation. The result will be infinite attempts to calculate each component
;;; without evaluating against the good-enough? procedure.
;;; Actual result: ;Aborting!: maximum recursion depth exceeded
