; naively subtract as integers
; we must substract the smaller from the higher
; there is some sense of subtractive priority, e.g. do we subtract on
; the basis of the lower bound or the upper bound, or based on the provided
; order? I think since we could use either, we ought to use the provided order...
; this follows the same rule as simple subtraction where user defines the left
; hand item in (a - b = res)
(define (sub-interval x y)
  (make-interval (- (upper-bound x) (upper-bound y))
                 (- (lower-bound x) (lower-bound y))))

