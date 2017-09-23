;;; Implementing pascal's triangle.
;;;
;;; Observations:
;;; 1. each row has one more number than the last.
;;;     a. therefore rows alternatingly are even or odd.
;;;     b. A counter can be used to refer to the last row's values. This method requires us to keep a list of preceding values, which we have no idea how to do.
;;;     c. More likely, we could use a recursive tree to climb down each row, n. One design issue is that we want to climb down each row before continuing to the next row. **
;;; 2. The next row is the sum of each two adjacent numbers in the preceding row.
;;;     a. the first and last numbers of each row are always one.
;;;     b. the number of pairs for a row are always 2 less than the length of the row.
;;; 3. There is mirror-symmetry in the tree. Only half the information must be computed. **
;;; 4. The sum of each row, n, is 2^n
;;;
;;; ** take a look at the pascal's triangle below, and the half triangle as well.
;;;
;;;      1                   1
;;;     1 1                 1 
;;;    1 2 1               1 2
;;;   1 3 3 1             1 3 
;;;  1 4 6 4 1           1 4 6
;;; 1 5 10 10 5 1       1 5 10
;;;1 6 15 20 15 6 1   1 6 15 20
;;;
;;; The half tree makes things really obvious. We are spawning one additional process
;;; for each layer of depth into the recursive tree.
;;; Lets code!
;;; Looking deeper into the triangle, if we only have a subset of information it may
;;; not be enough to continue the recursive process.
;;; For example, lets take 5 10 10.. at first we only yield 15 20, then 35 only.
;;; Lets try 6 15 20 15 6... nope, we must have an edge to continue forever!

;; Finally had an idea after playing with my notes. This calculates the nth number
;; in a pascal triangle.
;; It does so by reducing that coordinate (r,d) to its parent coordinates.
;; It uses 2 base cases, listed in the cond of the pascal method below.
;; d = depth(horizontal), r = row(vertical)
;; currently doesn't handle bad depths... depth must satisfy: d <= r / 2
;; we could rectify depths to assign r/2 as center, and fix d=r to 1 and d=r-1 to 2, etc.
;; however, d must still satisfy d <= r in this case.

;; issue: if depth is greater than row length, the calculation must be dropped
;; This was explicitly tested for (pascal 2 3) and it fixed issues with depth 3 calculations
;; It follows that if this is manually fixed for (pascal 2 4) and pascal (3 4) then it
;; will fix all problems. We will catch all cases with ((> d r) 0)
(define (pascal r d)
  (cond ((> d r) 0)
        ((< d 2) 1)
        ((< r 3) 1)
        ; sum the parent numbers of the current number
        (else (+ (pascal (- r 1) (- d 1)) (pascal (- r 1) d)))))

;; procedure tests the 1-depth range of pascal
(define (test_depth1 r)
  (cond ((= r 1) #t) ; no 2-depth value for row 2 or row 1.
        ((= (pascal r 1) 1) (test_depth2 (- r 1)))
        (else #f)))

;; procedure tests the 2-depth range of pascal
(define (test_depth2 r)
  (cond ((= r 2) #t) ; no 2-depth value for row 2 or row 1.
        ((= (pascal r 2) (- r 1)) (test_depth2 (- r 1)))
        (else #f)))

(test_depth1 100)
(test_depth2 100)

(pascal 2 3) ; test bad size

(pascal 2 2)
(pascal 3 2)
(pascal 4 2)
(pascal 5 2)
(pascal 6 2)

(pascal 4 3) ; this is invalid
(pascal 5 3) ; procedure still fails for a depth > 2
(pascal 6 3)
(pascal 7 3)
(pascal 8 3)

(pascal 7 4)
(pascal 8 4)
(pascal 9 4)
(pascal 10 4)

(pascal 9 5)
(pascal 10 5)
(pascal 11 5)

(pascal 11 6)

;; lets test depths greater than 1/2 row length
;; the discount of depths greater than the row has rectified the recursive
;; algorithm for high depths, which I didn't plan for.
(pascal 4 4) ; 1
(pascal 5 4) ; 4
(pascal 10 9) ; 9
(pascal 10 7) ; 84

