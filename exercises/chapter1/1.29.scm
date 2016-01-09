
;;; use integration by summation as a basis for Simpson's Rule

(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (gnarly x) (* 1 x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

;;; n must be even
(define (simpson-integral f a b n)
  (define (calc-h)
    (/ (- b a) n))
  (define (calc-yk k)
    (* (cond ((= k 0) 1)
             ((= k n) 1) 
             ((even? k) 2)
             (else 4))
       (f (+ a (* k (calc-h))))))
  (* (sum calc-yk 0 inc n)
     (/ (calc-h) 3)))


(simpson-integral cube 0 1 2)
(simpson-integral cube 0 1 10)
(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)

(simpson-integral gnarly 0 1 2)
(simpson-integral gnarly 0 1 10)
(simpson-integral gnarly 0 1 100)
(simpson-integral gnarly 0 1 1000)

;;; my analysis
;;; my algorithm computes 1/4 exactly.
;;; why does it return a result fraction form though?
