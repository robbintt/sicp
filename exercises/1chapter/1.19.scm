; here is the code in exercise 1.19

; the solution isn't hard, one hanging point is that the
; first transformation is simply substituting T for a and b.
; this means the second transformation is applying T into
; itself. At first I did way too much algebra, trying to
; perform what is essentially T'''.

; these naturally fall out when putting T'' in the form of T.
; p' = p^2+q^2
; q' = q^2+2pq
; note: this is done by extracting a and b, allowing you to
; substitute these equations for p and q in T, to get T''

(define (square a) (* a a))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; compute p'
                   (+ (square q) (* 2 (* p q))) ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)
(fib 20)
(fib 100)
