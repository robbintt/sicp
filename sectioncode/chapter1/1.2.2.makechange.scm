;;; Copied from the book


(define (count-change amount)
  ;; the number of coin types in first-denomination is 5
  (cc amount 5))

;;; count the coin denominations iteratively.
;;; in some sense this is a on dimensional space filling problem.
;;; if this were generalized for 3 dimensions it could be used for packages.
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1 )
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

;;; defines the parameters of the currency.
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


(count-change 100)
