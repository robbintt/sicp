;; interval arithmetic - combining intervals, e.g. 6.8+-10% ohms in parallel with 4.7+-5% ohms yields the range 2.58 to 2.97 ohms.
;; in the example above, operating on the two intervals yields an interval result
;; abstract object "interval": two endpoints. given the endpoints, construct the interval using `make-interval`
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
;; trent: i surmise lower-bound/upper-bound will be a function/mask like car/cdr implementation

;; using min/max, get product of 2 intervals, which doesn't always cover the original interval range, e.g. (* (2,4), (4,8)) == (8, 32)
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
;; the interval data type does already make an assumption that lower-bound and upper-bound are accurate, so this implementation is overwrought
;; hmm see exercise 2.11, which recommends that there is actually one case that requires all this work
;; i prefer this one which uses the properties of the data structure, i think it is more flexible, e.g. if we aren't using arithmetical systems
(define (better-mul-interval x y)
  (make-interval (* (lower-bound x) (lower-bound y))
                 (* (upper-bound x) (upper-bound y))))

;; first interval x multiplied by the inverse of the second interval y
;; i suppose it takes the form x/y == x*(1/y) 
;; then applies (1/y) on an interval which is applied as (1/(y1, y2)) == (1/y1, 1/y2)
;; finally it computes x * (1/y1, 1/y2) by substituting the interval for x, as follows: (x1, x2) * (1/y1, 1/y2)
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
