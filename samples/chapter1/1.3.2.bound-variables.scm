
(define x 5)

(+ (let ((x 3))
     (+ x (* x 10)))
   x)

(define x 2)

(let ((x 3)
      (y (+ x 2)))
  (* x y))


;; why didn't we do this:
(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (squarea))
     (* y b)
     (* a b)))
;; we could but define is overkill because the definitions a,b do not take
;; parameters -- they are not procedures.
;;
;; book note:
;; know if you are making an internal definition or an internal procedure
;; i believe the difference is that an internal definition redefines input variables
;; in terms used internally. this is a good use case for let.
;; 
;; very simply put, a definition takes no parameters whereas a procedure takes parameters.
;; both use define
;; 
