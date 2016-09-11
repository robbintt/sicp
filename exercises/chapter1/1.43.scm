; write a 'repeated' method to perform an operation similar to 'double', n times.
;
; 'hint' in SICP says: compose may be helpful

; here is compose
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (repeated proc x)
  (cond ((= x 1)
         proc)
        (else (compose proc (repeated proc (- x 1))))))


((repeated square 1) 5)
; desired usage - expect 625, ((5^2)^2)
((repeated square 2) 5)


; lets do bill's int test:

((repeated inc 2) 5)
((repeated inc 3) 5)

; this test of a higher number of repeats exposed an issue where my code
; was originally behaving more like the 1.41 double process.
; (fixed)
((repeated inc 10) 10)
