
;;; integration by summation (p. 59)
;;; now lets use sum as a building block 

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;;; my analysis:  

;;; integral uses add-dx, which takes x and the parent scope dx and adds them.
;;; sum uses add-dx for the <next> a value
;;; add-dx is a simple incrementer just like the pi-sum or the builtin (+ a 1) in the
;;; unabstracted 3 samples that yielded the 'sum' abstraction.

;;; in one sample we give a=0, b=1 and dx=0.01. this specifies the range of the integral
;;; and the resolution of the integration (dx).

;;; >side note: we moved from using integers to decimal(float?), which shows in the very 
;;; small resolution abberances of the result as dx become smaller

;;; >side note: this integration is limited to single variable integration, not only by sum function but also by the summation method. this is all single variable calculus.

;;; considering the values 0, 1, 0.01 by hand (by mind)
;;; following the first calculation into 'sum', a=0+(.01/2)=2/100, b=1
;;; the summation then produces ever increasing quantities approaching a=b, a=.98+(.01/2)=1
;;; this is the terminal value which yields 0
;;; all the values summed yield 1/4


(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

