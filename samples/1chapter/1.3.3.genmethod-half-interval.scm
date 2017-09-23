; Half-Interval Method
;
; Procedure p.67
;

; from 1.1.7 sqrt by heron of alexandria
; We use the same fixture for defining close-enough as well.
(define (average x y) (/ (+ x y) 2))

; the key difference between this an 1.1.7 is that here we are checking
; two guesses against each other, whereas 1.1.7 checks guess against fixed value.
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

; a note on average and close-enough?  :
; Both of these procedures represent unrecognised issues as yet by SICP.
; Providing a general solution for average would require more dynamic data structures.
; Providing a general method for close-enough? requires the same. However,
; close-enough? has the additional wrinkle of needing a tolerance.  SICP mentions
; that the tolerance will probably always be a magic number so this could be abstracted
; to a variable.

; general search method where f could be the half interval method
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))


; builtins used: positive?, negative?
; external definitions: close-enough?, average
; internal definitions: midpoint, test-value
;
; test-value: get the value of f at midpoint. simple enough.
;
; recursive method: search
; base case: close-enough? yields #t, return midpoint
; base case 2: cond positive? and cond negative? aren't true for test-value

; here we give the infrastructure required by the subspace of the half-interval-method.
; the cond block here is a little bit of wiring and an error state for this subproblem.
; The purpose of a-value and b-value is to map the inputs a,b onto the function f which
; is also a subproblem.
;
; in other words, the general search method is much more abstract.
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

; builtins: negative?, positive?, error
; internal definitions: a-value, b-value
; external definitions: search

; Question: How else can the general search method be used?
; I do immediately understand the separation of concerns but I am struggling to
; find another problem to map search onto.
; examples: - the general search relies on positive and negative tests of test-value
; oh well best to read on!

; test function from SICP
(define (f1 x)
  (- (* x x x) (* 2 x) 3))

; lets use it
(half-interval-method sin 2.0 4.0)
(half-interval-method f1 1.0 2.0)

