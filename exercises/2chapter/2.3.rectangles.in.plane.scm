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

;; test out permiter, area, and the selectors and constructors
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


;; now reimplement rectangle so that the perimeter and area procedures still work
;; how to make the procedures still work: define them in terms of selectors
;; 
;; right now we use two points to explain rectangle
;; alternative: use the zero-most point and the length for each dimension
;; * alternative: use any point and a negative or positive length for each dimension
(define (make-vector x x-len) (cons x x-len))
(define (get-vector-origin vector) (car vector))
(define (get-vector-magnitude vector) (cdr vector))
(define (get-vector-end vector) (+ (car vector) (cdr vector)))

;; if i pass a vector rectangle into area, perimeter - they require the start-rectangle and
;; end-rectangle selectors to work... but which definitions of them?
;; i suppose if i am redefining rectangle, i am redefining its selectors in terms of the
;; new rectangle as well, so i need to redefine start-rectangle and end-rectangle
;; ... so should i pass those functions into area and perimeter? nah we will just write over
;; the methods.
;; i don't like that i am writing a custom selector for the vector rectangle to get the points
;; it should be possible to make the functions area and perimeter work for the vectors native
;; to the vector rectangle...
(define (make-rectangle x-vector y-vector)
  (cons x-vector y-vector))
;; derive the start point of the vector rectangle
(define (start-rectangle rectangle)
  (make-point (get-vector-origin (car rectangle)) (get-vector-origin (cdr rectangle))))
;; derive the end point of the vector rectangle
(define (end-rectangle rectangle)
  (make-point (get-vector-end (car rectangle)) (get-vector-end (cdr rectangle))))



;; test out perimeter, area, and the selectors and constructors
(define v1 (make-vector 2 -4))
(define v2 (make-vector 2 -4))
(define myrect (make-rectangle v1 v2))

(define v3 (make-vector 2 7))
(define v4 (make-vector 2 7))
(define myrect2 (make-rectangle v3 v4))


(perimeter myrect)
(area myrect)

(perimeter myrect2)
(area myrect2)


;; i don't think perimeter and area are abstract enough to satisfy the problem
;; i had to write special selectors for the vector rectangle to interface with the point-
;; based perimeter and area functions
;; what other implementation of rectangle is reasonable?  should it still be in terms of points? that is cheating.
;;
;; the methods area and perimeter need a common interface with rectangle, so i guess you must
;; write selectors that are compatible. it seems like we are breaking an abstraction barrier...
;; it would be breaking an abstraction barrier because the selectors are not in terms of the base data type of vector
;; instead we are casting vectors to points through the lens of rectangle
