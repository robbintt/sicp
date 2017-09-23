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
(A 2 3)
(A 2 5)
(A 2 6)
(A 3 3)

;; give concise mathematical definitions for the functions computed by f g and h for
;; postive integer values n. For example, (k n) computes 5n^2.
;; (define (k n) (* 5 n n))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

;(f n) computes 2n
;
;(g n) computes (A 0 (A 1 (n-1))). 
; The component (A 1 (n-1)) computes to (A 0 (A 1 (n-2)))
; In turn, that computes to (A 0 (A 1 (n-3))
; This will resolve at value p when p = n-1, and thus y = 1, which yields 2.
; So in the terminating case p=n-1, A yields 2 for y.
; This can be passed into the parent to evaluate (A 0 2), which yields 4
; This is passed into the parent yielding (A 0 4) == 8.
; This continues on p times, yielding 2^n.
; CONFUSING POINT: If the recursion occurs only n-1 times, why do we yield 2^n?
; CONFUSING ANSWER: We have to include the base case, 2, in the result, which is
; an extra 2, thus the answer is MORE PURE in the form 2*2^(n-1)
; I found this out by trial and error...
;
;(h n) computes (A 0 (A 0 (n - 1)))
; First, (h n) is (A 2 n), computing A yields (A 1 (A 2 n-1)).
; Focusing first on the internal (A 2 n-1), computing A yields (A 1 (A 2 n-2)))
; The internal (A 2 n-2) will continue until the base case n-p = 1, yielding 2.
; It behaves exactly like the internal case in (g n), yielding 2^n
; Now lets move on to the external (A 1 (2^n)), which simplifies to (A 0 (A 1 (2^n)-1))
; This expands to 2*(A 1 (2^n)-1), which simplifies to 2*(A 0 (A 1 (2^n)-2))
; The pattern becomes apparent: 2*2*(A 1 (2^n)-2)
; So for each expansion p, we get 2^p, with a limiting factor where 2^n = p which
; yields (A 1 0) = 2.
; Or in english, result == two times (two to the on) twos.
;
; This didn't quite work, unfortunately my test below doesn't work either, because
; it drops out at n=4 citing recursion depth.
; I seem to have experimentally found the solution below, but the test set is too small.
;
; (h 1)
; (A 2 1)
; 2
;
; (h 2)
; (A 2 2)
; (A 1 (A 2 1))
; (A 1 2) # important key point, the x=2 degenerates to x=1 if y is <= 2.
; (A 0 (A 1 1)
; (A 0 2)
; 4
;
; (h 3)
; (A 2 3)
; (A 1 (A 2 2))
; (A 1 (A 1 (A 2 1)))
; (A 1 (A 1 2))
; (A 1 (A 0 (A 1 1)))
; (A 1 (A 0 2))
; (A 1 4) ;;; BIG HINT - (A 2 3) reduces to (A 1 4).
; (A 0 (A 1 3))
; (A 0 (A 0 (A 1 2)))
; (A 0 (A 0 (A 0 (A 1 1))))
; (A 0 (A 0 (A 0 2)))
; (A 0 (A 0 4))
; (A 0 8)
; 16
; (h 3) result = 16
;
; (h 4)
; (A 2 4)
; (A 1 (A 2 3)) ;;; expanding (A X Y) yields (A X-1 Y+1)
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4)) ;;; expanding (A 1 X) yields 2^X 
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; (A 1 (A 0 (A 0 4)))  
; (A 1 (A 0 8))
; (A 1 16) ;;; This contraction of (A 0 X) terms is one expansion action
;
; TAR 080415 -  (h 4) always yields 2^2^2... expanding ^2 n times...
;               This model always does an early expansion of:
;               (A 2 N)
;               (A 1 (A 2 N-1))
;               (A 1 (A 1 (A 2 N-2)))
;               ...
;               (A 1 (A 1 ... (A 1 (A 2 N-P)) ... )), where p = (N-1)
;               (A 1 (A 1 ... (A 1 (A 2 1)) ... )) reduces to
;               (A 1 (A 1 ... (A 1 2))) ... )) which also reduces neatly to
;               2^2^2...^2 for a total of p times including the 4 from (A 2 1)
;
;               lets finish the first of p expansions:
;               (A 1 (A 1 ... (A 1 2) ... ))
;               (A 1 (A 1 ... (A 0 (A 1 1)) ... ))
;               (A 1 (A 1 ... (A 0 2) ... ))
;               (A 1 (A 1 ... 4 ... )) This includes the internal 2 from (A 2 1)
;
;
; We know (h 4) yields 65536 or 2^16, this could give a hint.
; This follows the general pattern 2^2^2^2 = 2^16 = 65536
;

;;; TESTS BELOW

; used in (f n) (g n)
(define (dec x)
  (- x 1))

; used in (g n) (h n), only works for x > 0, y >= 0
(define (** x y)
  (define (**iter result count)
    (cond ((= 0 count) 1)
          ((= 1 count) result)
          (else (**iter (* x result) (- count 1)))))
  (**iter x y))

; used in (h n), take a number to its own power repeatedly, e.g. 2^2^2... repeating n-1 times
(define (exp_repeat x y)
  ; may need special conditions for y = 0
  (define (exp_repeat_iter x result y)
    (if (= y 1)
         result
         (exp_repeat_iter x (** x result) (- y 1))))
  (exp_repeat_iter x x y))

       


; test for (f n) up to n=100 (starting at 100)
; PASSES
(define (test_f n)
  (if (= n 0) 
    #t
    (if (= (f n) (* 2 n))
      (test_f (dec n))
      #f)))

(test_f 100)

; test for (g n) up to n=100 (starting at 100)
; PASSES
(define (test_g n)
  (if (= n 0) 
    #t
    (if (= (g n) (** 2 n))
      (test_g (dec n)) 
      #f )))

(test_g 100)


; test for (h n) up to n=4 (starting at 4)
; PASSES
(define (test_h n)
  (if (= n 0) 
    #t
    (if (= (h n) (exp_repeat 2 n))
      (test_h (dec n)) 
      #f )))

(test_h 4) ;; recursion depth exceeded after 4
