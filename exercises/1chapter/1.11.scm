;;;  define a recursive process based on the function in the example
;;;


; RECURSIVE PROCESS:
; I need to think up a test for this... i guess i could make the iterative process
; then test it with the iterative process. lets do that.
(define (f n)
  (cond ((< n 4) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

; ITERATIVE PROCESS:
; define an iterative process (recursive procedure is fine) based on the function.
(define (fi n)
  (define (iter-f c n1 n2 n3) ; n1 is the most recently calculated, so it starts as n=3
    (if (= c n) ; we start at c = 3, terminate when c = n
      n1
      (iter-f (+ c 1)
              (+ n1 (* n2 2) (* n3 3))
              n1
              n2)))
  (iter-f 3 3 2 1))


; lets test this up to a high number
(define (f_test n)
  (cond ((= n 3) #t)
        ((= (fi n) (f n)) (f_test (- n 1)) )
        (else #f)))

(f_test 25)
