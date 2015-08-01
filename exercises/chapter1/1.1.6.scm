;;; Nice idiom for a >= method.


(define (>= x y) 
  (or (> x y) (= x y)))

