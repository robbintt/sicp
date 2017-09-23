
; suppose:
(define (f g)
  (g 2))

(f square)
; 4

( f (lambda (z) (* z (+ z 1))))
; 6


; what about
; (f f)
; ...
(f f)
; this yields (2 2)
; 2 does not take arguments and we get an error.
