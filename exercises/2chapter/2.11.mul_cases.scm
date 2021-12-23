(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (interval-width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (upper-bound y))
                 (- (lower-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; throw if interval spans 0
; e.g. (-1, 1) interval is size 2
(define (div-interval x y)
  (cond ((and (< (lower-bound x) 0) (> (upper-bound x) 0)) (error "First interval spans 0."))
        ((and (< (lower-bound y) 0) (> (upper-bound y) 0)) (error "Second interval spans 0."))
        (else
          (mul-interval x
                        (make-interval (/ 1.0 (upper-bound y))
                                       (/ 1.0 (lower-bound y)))))))

; convert to 9 cases, only 1 of which require more than 2 multiplications
; use the signs of the endpoints of the intervals
; what are the 9 cases?
;   4 possible cases for pairs are -- -+ +- ++
;   we can consider 0 positive to keep the cases low
;   this gives 16 total cases, so some must behave the same
;   (-100 -1) (-100 -1) yields (1, 10000) where...
;
; this is annoying, do it later
;
(define (mul-interval-improved x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (cond  ((and (< (lower-bound x) 0) (> (upper-bound x) 0))
            (make-interval
              (* (lower-bound x) (lower-bound y))
              (* (upper-bound x) (upper-bound y))))
           ((or (= 0 (lower-bound x)) (= 0 (lower-bound y)))
            (make-interval
           ((< (upper-bound y) (lower-bound x))
            (make-interval
              (* (lower-bound x) (lower-bound y))
              (* (upper-bound x) (upper-bound y))))

    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
