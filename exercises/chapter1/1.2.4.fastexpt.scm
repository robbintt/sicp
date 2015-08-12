;;;
;;; fast-expt
;;;
;;; For space and steps theta(n) = log(n)
;;;
;;; This example explains the problem 1.15 in more detail.
;;;
;;;
;;; The book claims that an iterative process can also be made for which
;;; theta(n) = log(n) but the iterative process is apparently "not written
;;; down so straightforwardly as the recursive algorithm." Footnote 39 claims
;;; this iterative form is ancient. (See knuth 1981 section 4.6.3 for a detailed
;;; analsysis of this algorithm.)


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;;; SICP says remainder is a primitive procedure.
(define (even? n)
  (= (remainder n 2) 0))


(fast-expt 10 2)
(fast-expt 5 3)

