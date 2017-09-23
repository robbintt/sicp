;;;
;;; Euclid's Algorithm
;;;


(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))


(gcd 10 4) ; 2

(gcd 100 90) ; 10

(gcd 156 24) ; 12
