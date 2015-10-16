; Problem 1.17
;
; this is a multiplication method using addition given in the problem
; the point of the mult procedure is to be analogous to the previous expt procedure example
; it recursively adds to get a product. the expt procedure recursively multipies to get an expt.
(define (mult a b)
  (if (= b 0)
    0
    (+ a (mult a (- b 1)))))

(mult 5 4)
;
; Q:    Assuming we have no multiplication method, create a fast-mult method analogous to fast-expt.
;       It must use a logarithmic number of steps, like to fast-expt.
;       Use the following assumed functions;
(define (double a) (+ a a))
(define (halve a) (/ a 2))

(double 10)
(halve 10)

; Assumptions:
; I will use the recursive form as in the recursive fast-expt example.
; in keeping with the analogy, I will assume we are performing b+b for any doubling.
; so for example:
; (fast-mult 5 2)
; 5 + 5
; 10
; (fast-mult 5 4)
; 5 + 5 = 10
; 10 + 10 = 20
; (fast-mult 10 10)
; 10 + 10 = 20
; 20 + 20 = 40
; 40 + 40 = 80
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-mult a b) 
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(fast-mult 10 10)
(fast-mult 7 9)
(fast-mult 10 13)
(fast-mult 25 25)
