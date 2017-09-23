; double procedure
; 
; needs to wrap a function in itself
; for f(x), return f(f(x))
; what about for f(x,y) -- return f(f(x,y))
; doesn't quite work because we can't predict how many outputs...
; so lets solve for one output.


(define (inc x) (+ x 1))

(inc 1)
(inc 10)
(inc 100)

; feeding a process to itself doesn't work, it expects its input...
(define (double proc) 
  (lambda (x)
    (proc (proc x))))

; lets define how we want to use this
((double inc) 1)

; 2^2^2 increments are called. (2^4 = 2 * 2 * 2 * 2 = 16) yielding 21
(((double (double double)) inc) 5)
