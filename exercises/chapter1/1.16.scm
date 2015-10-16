; Q: Design a procedure that evolves an
;       iterative exponentiation process
;       that uses successive squaring and
;       uses a logarithmic number of steps (as does fast expt).
;
; Core problem: fast-expt uses a recursive process, the core
;               of this problem is to use a iterative process.
;           
; quick thoughts:
; 1. i know that recursive processes can be unrolled to iterative.
; 2. a previously solved sicp problem probably has the insight required. try this first.
;
; Previous example - factorial (1.2.1)
; this example describes one iterative and one recursive algorithm for factorials.
; the key feature is that state is stored in variables so that the iterative form can pick back
; up at any point in the process.
; whereas the recursive function stores state in the depth of the algorithm.
; 
;
; extrapolating examples(2) 1.2.1:
; My approach will be to review the state of the recursive fast-expt process
; and abstract the recursively stored state into state variables, then repack
; this information into an iterative package.  In 1.2.1 iterative example, we call this 'fact-iter'.
; this is sort of a 'iterative scaffolding'
;
;
; The book gives a hint, "store an additional state variable a and define the state transformation
; in such a way that the product a(b^n) is unchanged from state to state.
; Furthermore, at the beginning of each process, a is to be taken to be 1, AND
; the answer is given by the value of a at the end of the process.
;
;
; Book learning note -- "In general, defining an 'invariant quantity' that remains unchanged from
;                           state to state is a powerful way to think about design of iterative
;                           algorithms."
;
;
;
; My immediate thoughts:    the iterative algorithm must be able to pick up the state at any point.
;                           this implies that the solution route for a set of solutions for b will
;                           only diverge as a approaches b^n, the solution. there are many other things
;                           that imply this as well, but that isn't the immediate exercise, so I will continue.
;
;
(define (even? n)
  (= (remainder n 2) 0))


; just storing a = 1 and exposing the iterative procedure
(define (fast-expt b n)
  
  (fast-expt-iter 1 b n))

; i had to defer to bill the lizard here, just for what new state to send each pass over (a,b,n), not structurally. 
; i was trying to hold b as fixed and square a each time. what i didn't consider is that we don't need the original
; value of b once we start squaring, so we can use that to store state. I had this realization when I did the
; actual math for n=7, the odd-route factored b is always already multiplied by the current power, n.
;
; What I was doing wrong:
; i was trying to hold b constant and store state information in a,
; i got fixated on this, when i could easily store state information in b
; the hint gives the explanation that a(b^n) should be constant from state
; to state, but i was trying to hold b constant from state to state.
; the only change that was necessary was to swap my squaring from the a to the b
; then move the a * b inside the fast-expt-iter in the else statement to store the state in a
; this always happens because 2/2 = 1, so the else must be hit at least once at the end of the algorithm.
(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))


(fast-expt 2 2)
(fast-expt 2 5)

(fast-expt 10 1)
(fast-expt 10 2)
(fast-expt 10 3)
(fast-expt 10 4)

; Final Notes:
; I got stuck trying to hold b fixed for no reason.  In the future, if I get stuck,
; I need to list out my assumptions and see which ones are poorly founded.
; There wasn't any reason I couldn't have easily finished this exercise, I got 99% there
; but should have stepped back and reviewed WHY I was trying to hold b fixed.
; it didn't really occur to me that I was doing that, only that b should be fixed so that
; i could use it. I even had it in my calculations that any time you take the odd path, you
; don't apply b, you apply some square of b. confusing at first.
;
; I also think this algorithm is a little dirty, which they do explicitly state in the book.
; Perhaps the better route would have been to look up the iterative algorithm in the footnote
; but that would have been cheating.



