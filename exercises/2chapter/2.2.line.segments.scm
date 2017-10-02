;; Exercise 2.2
;; consider line segments in a plane
;;
;; each segment represented by a pair of points, start-point and end-point
;; constructors: make-segment
;; selectors: start-segment, end-segment
;;
;; each point represented by a pair of numbers, x-coordinate and y-coordinate
;; constructors: make-point
;; selectors: x-point, y-point
;;
;; finally, using selectors and constructors, define a
;; procedure called midpoint-segment that takes a line segment
;; as an argument and returns its midpoint (average of coordinates of endpoint)
;;
;; use the print-point procedure listed on p.90
;; end of problem explanation

;; point constructor and selectors
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; segment constructor and selectors
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (avg x y) (/ (+ x y) 2))

;; the midpoint is the average of the x and y values
(define (midpoint-segment segment)
  (let ((start-x (x-point (start-segment segment)))
	(start-y (y-point (start-segment segment)))
        (end-x (x-point (end-segment segment)))
        (end-y (y-point (end-segment segment))))
    (make-point (avg start-x end-x) (avg start-y end-y))))


(define p1 (make-point 2 2))
(define p2 (make-point -2 -2))
(define seg1 (make-segment p1 p2))

(x-point p1)
(y-point p1)

(x-point p2)
(y-point p2)

(define midpoint (midpoint-segment seg1))

(print-point midpoint)
