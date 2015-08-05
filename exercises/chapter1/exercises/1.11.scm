;;;  define a recursive process based on the function in the example
;;;


(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

; I need to think up a test for this... i guess i could make the iterative process
; then test it with the iterative process. lets do that.
(f 7)



;;; define an iterative process (recursive procedure is fine) based on the function.
