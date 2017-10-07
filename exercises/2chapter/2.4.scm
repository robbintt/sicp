;;
;; 


(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p )))

;; cdr is easy?
(define (cdr z)
  (z (lambda (p q) q )))

(define hi (cons "hello" "world"))
(car hi)
(cdr hi)
(car (cons "h" "w"))
(cdr (cons "h" "w"))

(define hi2 (cons (car (cons "h" "w")) (cdr (cons "h" "w"))))

(car hi2)
(cdr hi2)

;; did substitution by hand (see sec. 1.1.5 for substitution example...
;; but fyi it is just like you do in junior high algebra
