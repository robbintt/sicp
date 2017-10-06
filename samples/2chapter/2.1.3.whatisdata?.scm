;; 
;; What is data?
;; > "Some collection of selectors and constructors, together with specified conditions that these procedures must fulfill in order to be a valid representation." 
;; See: "method of abstract models" by C.A.R. Hoare (1972) -- procedure+conditions specification
;; See: "algebraic specification", procedures as elements of an abstract algebraic system with axioms corresponding to our word 'conditions', uses abstract algebra techniques to check assertions about data objects.
;; Both methods are surveyed in paper by Liskov and Zilles (1975)
;; paper: https://pdfs.semanticscholar.org/55da/67af3e10046a11713b4c81609b7483b3b9f0.pdf
;; 
;;


;; we can define cons, car, and cdr with ONLY procedures, NO data structures
(define (cons x y)
  ;; note that dispatch is returned by cons and holds the conditions for car and cdr
  ;; car and cdr in turn tell dispatch who they are
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

(define hi (cons "hello" "world"))

;; you can only car or cdr dispatch function from cons
;; the dispatch function holds the data and car/cdr supplies the case
(car hi)
(cdr hi)
;; the use of `dispatch` as a procedural representation of `pairs` is an
;; example of style of programming: `message passing`, 
;; `message passing` is a `basic tool` in ch3 for `modeling` and `simulation`


;; somewhat obviously: no other features of `pairs` are covered in this implementation

