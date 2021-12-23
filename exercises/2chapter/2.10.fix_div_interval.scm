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

(div-interval (make-interval -1 1) (make-interval 1 2))
(RESTART 1) ; recover from error
(div-interval (make-interval 1 2) (make-interval -1 1))
(RESTART 1) ; recover from error
(div-interval (make-interval 1 2) (make-interval 1 2))
