;;;
;;; Consider 3 summation procedures
;;; The common underlying pattern can be observed below
;;;

; helper function
(define (cube x) (* x x x))

;; compute the sum of the integers in the given range a through b
(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

;; compute sum of the cubes of integers in the given range
(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

;; compute the sum of a sequence of terms (see p 57) which coverges to pi/8 very slowly
(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;;; Note the common underlying pattern above
;;; here is an abstract expression of the underlying pattern
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

;;;
;;; this abstract procedure can do the three below procedures:
;;;

;;; helper identity function for sum-integers
(define (identity x) x)
(define (sum-integers2 a b)
  (sum identity a inc b))

;;; helper inc function for sum-cubes
(define (inc n) (+ n 1))
(define (sum-cubes2 a b)
  (sum cube a inc b))

;;; thirdly pi sum
;;; here the helper functions are moved inside the pi-sum method.
;;; this uses BLOCK STRUCTURE (1.1.8) which we have already seen.
(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


;; lets compare

(sum-integers 0 10)
(sum-integers2 0 10)

(sum-integers 0 100)
(sum-integers2 0 100)

(sum-cubes 0 1)
(sum-cubes2 0 1)

(sum-cubes 0 3)
(sum-cubes2 0 3)

(* 8 (pi-sum 1 1000))
(* 8 (pi-sum2 1 1000))

