;;; 
;;; Applicative order evaluation - Evaluate after each substitution.
;;;
;;; Normal order evaluation - Apply every substitution possible before evaluating.
;;;

;; Ben's application to determine which one the interpreter is using:

; this self-definition is murky TAR 080115.1629
(define (p) (p))


(define (test x y)
  (if (= x 0)
    0
    y))

; evaluate
(test 0 (p))

;; What behavior do we get for applicative-order evaluation interpreters?


;; What behavior do we get for normal-order evaluation interpreters?
