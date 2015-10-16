

;;; copied this from the book to illustrate the result
;;; it may be interesting to build some sort of recursion depth
;;; and recursive call counters to characterize O(n) for steps and space.
;;;
;;; the height of the recursive tree changes each time a number triples
;;; Thinking about it TAR 081215:
;;; imagine that the base case is a bucket with a capacity of 0.1,
;;; then if you increase the number, each base case may only return 0.08.
;;; As the input number increases, these base case values gradually fill.
;;; When the number is tripled, it creates a new layer of the recursive tree
;;; and the base case buckets are at the bottom of the threshold again.
;;; addendum TAR 081215:
;;; it should be possible to define a lower bound for the base case return value
;;; based on the input value, so you can expect the base case to be as low as
;;; b where 0 < b < 0.1 but not lower.
;;; the value in the problem of 0.1 radians is also arbitrary. There should be a
;;; 'splitting' point where this algorithm is no longer useful
;;; NEITHER ADDENDUM DIRECTLY RELEVANT TO THE PROBLEM FOCUS, O(n) FOR SIZE AND SPACE

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)

;;; space - log(n)
;;; size - log(n)
;;; as n is incremented, operations do not increase (for large n)
;;; as n is multiplied, the number of operations increases additively (for large n)
;;; 
;;; This approach is from:
;;; http://www.billthelizard.com/2009/12/sicp-exercise-115-calculating-sines.html
;;;
;;; I need to go back and review my understanding of logarithms, characterizing the
;;; pattern this way is very useful.
