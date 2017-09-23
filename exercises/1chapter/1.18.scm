;
; Q: Using results of 1.16 and 1.17, devise a procedure that
;       generates an iterative process for multiplying
;       two integers in terms of: add, double, and halve and
;       which uses a logarithhmic number of steps.
;
; SICP - This is known as the "russian peasant method" and was on a
;           papyrus copied from another source in 1700 B.C.
(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (even? n) (= (remainder n 2) 0 ))

(define (fast-mult b n)
  (fast-mult-iter 0 b n))

(define (fast-mult-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-mult-iter a (double b) (halve n)))
        (else (fast-mult-iter (+ a b) b (- n 1)))))

(fast-mult 5 1) 
(fast-mult 5 2) 
(fast-mult 5 3) 
(fast-mult 5 4) 

(fast-mult 10 1)
(fast-mult 100 2)

; This was thoroughly unchallenging, being nearly a replica of 1.16 using the logic of 1.17.
; The only difference is that identity of addition is a + 0 = a, whereas for multiplication, a * 1 = a
