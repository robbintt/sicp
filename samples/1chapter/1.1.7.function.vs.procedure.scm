;;; Newton's definition for a square root isn't valid scheme.


(define (sqrt x)
  (the y (and (>= y 0)
              (= (square y) x ))))

;; This doesn't work!!
;;
;; The contrast between a function and a procedure:
;;
;; function - describes properties of a square root / newton style
;; procedure - describes how to do something.
;;
;; ALSO:    Declarative Knowledge == Function (what is?)
;;          Imperative Knowledge == Procedure (how-to.)
