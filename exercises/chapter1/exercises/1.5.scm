;;; 
;;; Applicative order evaluation - Evaluate after each substitution.
;;;
;;; Normal order evaluation - Apply every substitution possible before evaluating.
;;;

;; Ben's application to determine which one the interpreter is using:

; here we are defining p as its own evaluated result
(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

; see what happens with definite numbers... result evaluates correctly.
(test 0 5)

; on evaluation the repl hangs.
(test 0 (p))
;;; CONCLUSION: mit-scheme uses applicative-order interpretation.

; see if anything will evaluate after the hang. Probably not.
(test 0 5)

;; What behavior do we get for normal-order evaluation interpreters?
;;
;; Normal order will substitute 0 and (p) into test, where it is evaluated
;; and 0 is returned, because (= 0 0) yields 0 in this case.
;; y isn't tested which is the (p) component.


;; What behavior do we get for applicative-order evaluation interpreters?
;;
;; Applicative order will evaluate the arguments to test, then apply them.
;; This means it will never apply test because it will evaluate (p) forever.

