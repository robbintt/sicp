;;; ackerman's function
;;; apparently doing some sort of nested loop.
;;; wikipedia: 
;;; "illustrates that not all total computable functions are primitive recursive"
;;; "deeply nested recursion, used to benchmark a compiler's ability to optimize recursion."
;;; 

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

;; give concise mathematical definitions for the functions computed by f g and h for
;; postive integer values n. For example, (k n) computes 5n^2.
;; (define (k n) (* 5 n n))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

;(f n) computes 2n
;(g n) computes (A 0 (A 1 (n-1))). 
; The component (A 1 (n-1)) computes to (A 0 (A 1 (n-2)))
; In turn, that computes to (A 0 (A 1 (n-3))
; This will resolve at value p when p = n-1, and thus y = 1, which yields 2.
; So in the terminating case p=n-1, A yields 2 for y.
; This can be passed into the parent to evaluate (A 0 2), which yields 4
; This is passed into the parent yielding (A 0 4) == 8.
; This continues on p times, yielding 2^(n-1)
;(h n) computes (A 0 (A 0 (n - 1)))

; used in (f n) (g n)
(define (dec x)
  (- x 1))
; used in (g n) NOT USED??
(define (square x)
  (* x x))

; used in (g n), only works for x > 0, y >= 0
(define (** x y)
  (define (**iter result count)
    (cond ((= 0 count) 1)
          ((= 1 count) result)
          (else (**iter (* x result) (- count 1)))))
  (**iter x y))



; test for (f n) up to n=100
(define (test_f c)
  (if (= c 0) 
    #t
    (if (= (f c) (* 2 c))
      (test_f (dec c))
      #f)))

(test_f 100)

; test for (g n) up to n=100
;(define (test_g c)
;  (if (= c 0) 
;    #t
;    (if (= (g c) (- (* 4 (square c)) (* 2 c)))
;      (test_g (dec c)) 
;      #f)))

;(test_g 100)
