
(lambda (x) (+ x 4))

(lambda (x) (/ 1.0 (* x (+ x 2))))

;; cube used below
(define (cube x) (* x x x))

;; pi-sum and integral simplifications still call sum from 1.3.1
;; very simple summation procedure (math capital sigma)
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))


(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; tests
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(* 8 (pi-sum 1 1000))
; tests worked

;; equivailent statements
(define (plus4 x) (+ x 4))
(plus4 5)
(define plus4 (lambda (x) (+ x 4)))
(plus4 5)
; tested return same results, easy enough to read

; lambda as operator
((lambda (x y z) (+ x y (square z))) 1 2 3)
; book: or more generally, in any context where we would normally use a procedure name.


