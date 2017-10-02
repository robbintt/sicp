;; exercise 2.3
;; rectangles in a plane
;;
;; constructors
;; idea: a rectangle can be defined by any two points
;;
;; completeness: the perimeter and area can be defined by any two points.
;; errors: a pair of points might have a height or width of zero, in this
;; case the area would be zero. another issue is that perimeter is arrived
;; at by subtraction, this can be solved with absolute values. a third issue
;; is that area is a function of subtraction as well. it can also be solved with
;; absolute values.
;; 
;; these extensions don't seem valuable at first glance:
;; extension: a rectangle could correct on construction or selection to follow a regular pattern, for
;; example, it could always be defined in terms of (top-left-point, bottom-right-point). This requires
;; some complexity because it could be constructed with the top right and bottom left points.
;; extension: a rectangle could also correct on selection, as long as the api is consistent in traslating
;; a particular pair of points.  
;;
;; selectors:
;; select the first and second points of a rectangle
;;
;; create procedures that calculate perimeter and area of your rectangles
;; in terms of your constructors and selectors



;; implement a second representation for rectangles from the same constructors and selectors,
;; your procedures for perimeter and area should work for both representations

;; point constructor and selectors
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

;; from 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; rectangle constructor and selectors
;; a rectangle has the same description as a line segment
(define (make-rectangle start end)
  (cons start end))
(define (start-rectangle rectangle)
  (car rectangle))
(define (end-rectangle rectangle)
  (cdr rectangle))

;; line segment midpoint abstraction for reference
;; the midpoint is the average of the x and y values
(define (midpoint-segment segment)
  (let ((start-x (x-point (start-segment segment)))
	(start-y (y-point (start-segment segment)))
        (end-x (x-point (end-segment segment)))
        (end-y (y-point (end-segment segment))))
    (make-point (avg start-x end-x) (avg start-y end-y))))


;; perimeter and area share a calculation, each relies on a line segment
;; for any two points (x, y) and (a, b), you can arbitrarily cross the points:
;; using either (x, a), (y, b) or (x, b), (y, a), the former more naturally pairs x and y.
;; we have not yet written a method to find the length of a line segment so
;; it may not be useful to reintroduce the line segment abstraction from 2.2 entirely

;; rectangle perimeter
(define (perimeter rectangle)
  (let ((start-x (x-point (start-rectangle rectangle)))
	(start-y (y-point (start-rectangle rectangle)))
        (end-x (x-point (end-rectangle rectangle)))
        (end-y (y-point (end-rectangle rectangle))))
    (+ (* 2 (abs (- start-x end-x))) (* 2 (abs (- start-y end-y))))))

;; rectangle area
(define (area rectangle)
  (let ((start-x (x-point (start-rectangle rectangle)))
	(start-y (y-point (start-rectangle rectangle)))
        (end-x (x-point (end-rectangle rectangle)))
        (end-y (y-point (end-rectangle rectangle))))
    (* (- start-x end-x) (- start-y end-y))))


(define p1 (make-point 2 2))
(define p2 (make-point -2 -2))
(define myrect (make-rectangle p1 p2))

(define p3 (make-point 2 3))
(define p4 (make-point 9 10))
(define myrect2 (make-rectangle p3 p4))


(perimeter myrect)
(area myrect)

(perimeter myrect2)
(area myrect2)

