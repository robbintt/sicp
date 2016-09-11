; define a procedure implementing composition
;
; ex:
; ((compose square inc) 6)
; 49
;
; so the outcome is that 7*7 = 49, which means increment is applied before square.


(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x) (* x x))
(define (inc x) (+ x 1))

((compose square inc) 6)
