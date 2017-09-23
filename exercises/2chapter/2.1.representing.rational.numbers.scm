; 2.1.1 Notes
;
; the notes are pasted here for use in solving Exercise 2.1

; Exercise 2.1
; deal with signs so that they reflect the overall state in the 
; numerator only, e.g. a negative, if present only exists on the numerator


; if n is positive and d is positive, do nothing
; if n is negative and d is positive, do nothing
; if n is negative and d is negative, then n becomes abs(n) (see final step)
; if n is positive and d is negative, then n becomes negative n (how).
; final step:
; d always becomes abs(d)

; need to be able to switch a sign, not just do (abs x)
(define (neg x) (* -1 x))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


; this reduces negatives to the numerator, n, and makes a rational
(define (make-rat n d)
  (define (n-checker n d)
    ; note that the cases (n, d) and (-n, d) do not need alteration
    ; also because of the idempotency of (abs d) we can always apply it
    ; this could be more explicit and carry all three cases, removing the (abs d)
    ; but then there might be an extra test (d-checker n d) which would
    ; need to be compared against the (abs d).  The shape of the incoming data
    ; would ultimately determine which is more efficient
    (cond ((and (< n 0) (< d 0)) (abs n))
          ((and (> n 0) (< d 0)) (neg n))
          (else n)))
  (let ((g (gcd n d)))
    (cons (/ (n-checker n d) g) (/ (abs d) g))))


(print-rat (make-rat 5 10))
(print-rat (make-rat 5 -10))
(print-rat (make-rat -5 10))
(print-rat (make-rat -5 -10))


