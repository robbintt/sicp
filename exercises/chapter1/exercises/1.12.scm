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
;;;
;;; ** take a look at the pascal's triangle below.
;;;
;;;      1     
;;;     1 1
;;;    1 2 1
;;;   1 3 3 1
;;;  1 4 6 4 1
;;;
;;;
