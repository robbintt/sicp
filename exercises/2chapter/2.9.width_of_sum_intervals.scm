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

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; an example of addition
(interval-width (add-interval (make-interval 1 2) (make-interval 10 100)))
(+ (interval-width (make-interval 1 2)) (interval-width (make-interval 10 100)))

; an example of subtraction, use abs value when subtracting 2 to get the width
(interval-width (sub-interval (make-interval 1 2) (make-interval 10 100)))
(abs (- (interval-width (make-interval 1 2)) (interval-width (make-interval 10 100))))

; mul doesn't work that way
(interval-width (mul-interval (make-interval 1 2) (make-interval 10 100)))
(* (interval-width (make-interval 1 2)) (interval-width (make-interval 10 100)))

; div doesn't work that way
(interval-width (div-interval (make-interval 1 2) (make-interval 10 100)))
(/ (interval-width (make-interval 1 2)) (interval-width (make-interval 10 100)))
