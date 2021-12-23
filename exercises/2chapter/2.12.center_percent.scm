(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

; now it's time for working with percent tolerances, e.g. 3.5 +/- 15%

; alternate constructor for any interval given c and w
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

; alternate selectors for any interval to get center, width

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (percent-to-num p)
  (/ p 100))

(define (num-to-percent n)
  (* n 100))

; now we want percent tolerance constructor
(define (make-center-percent c p)
  (let ((width (* c (percent-to-num p))))
    (make-interval (- c width) (+ c width))))

; now lets make a selector percent to get the percent tolerance from a given interval
; which is (* 100 (/ width center))
; maybe i should define this without width and center, but it fits well...
(define (percent i)
  (num-to-percent (/ (width i) (center i))))

(make-center-width 5 1)
(center (make-center-width 5 1))
(width (make-center-width 5 1))
(percent (make-center-width 5 1))
; should construct the same interval as make-center-width
(make-center-percent (center (make-center-width 5 1)) (percent (make-center-width 5 1)))
