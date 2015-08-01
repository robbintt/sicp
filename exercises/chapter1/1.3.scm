;;;
;;; Define a procedure that takes three numbers as arguments and returns
;;; the sum of the squares of the two larger numbers.
;;;


;; sum of squares from chapter 1
(define (square x) (* x x))
(define (sum_of_squares x y) (+ (square x) (square y)))


;; how should we determine the two largest numbers
;; this procedure isn't used. instead conditions are checked manually
(define (yield_higher m n) (if (> m n) m n))

;; find the lowest of two numbers
(define (yield_lowest m n) (if (< m n) m n))

;; find the lowest of three numbers, this can be used in a comparison later.
(define (lowest_of_three x y z) (cond ((= (yield_lowest x y) (yield_lowest x z)) x)
                                      ((= (yield_lowest y x) (yield_lowest y z)) y)
                                      ((= (yield_lowest z x) (yield_lowest z y)) z)))

;; I don't have the tools to yield two results, so I will need to bake
;; this into the final procedure that consumes 2 arguments.
;; (define (highest_two a b c) ...)

;; now lets consume my procedures
;; 
(define (f a b c) (sum_of_squares (cond ((= (lowest_of_three a b c) a) b)
                                        ((= (lowest_of_three a b c) b) c)
                                        ((= (lowest_of_three a b c) c) a))
                                  (cond ((= (lowest_of_three a b c) a) c)
                                        ((= (lowest_of_three a b c) b) a)
                                        ((= (lowest_of_three a b c) c) b))))
(f 1 1 12)

