;;;
;;; now iterative
;;;
;;; definitions:
;;;
;;; counter starts at 1, product starts at 1
;;; first multiply the counter by the product, then increment the counter.
;;; factorial is the value of the product where the counter exceeds n.
;;; (test the counter, if greater than n return the factorial, else repeat the process.


;(define (factorial n)
;  (define (iter product counter)
;    (if (> counter n)
;      product
;      (iter (* counter product)
;            (+ counter 1))))
;  (iter 1 1)) ; always start at 1
;
;(factorial 5)

(define (factorial n)
  ; note that we refer to fact-iter in a block before it is defined.
  ; this block is part of a procedure executed after it is defined.
  (fact-iter 1 1 n))

; consider that a common definition of recursion is 'a function that refers to itself'
; but here we can clearly see that it's deeper. a 'recursive process' has a chain of
; deferred operations, yet we consider this to be an iterative process because the state
; is stored in the variables not in the recursion form. In the recursive form the
; state is stored in the substitution expansion.
; From SICP: In the iterative case the program variables provide a complete description
; of the state at any point in the process.
; SICP goes further: WE MUST NOT CONFUSE A 'RECURSIVE PROCESS' WITH A 'RECURSIVE PROCEDURE'
; A recursive procedure refers to itself. A recursive process stores state information
; in the recursion depth.
; Other languages require: 'for, while, do, repeat, until' a defect.
; Why? Because they store more procedure information for each 'recursive procedure' call.
; "The implementation of Scheme in chapter 5 will execute an iterative process in constant
; space, even if the iterative process is described by a recursive procedure. This
; implementation property is called TAIL-RECURSIVE.  In tail-recursive implementations
; a special iteration construct (for, while, until, etc.) are just syntactic sugar.
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    ; we're still using recursion to do iteration, lol #justlispthings ??
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

(factorial 5)
