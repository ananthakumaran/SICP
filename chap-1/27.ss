;; Exercise 1.27.Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the Fermat
;; test. That is, write a procedure that takes an integer n and tests whether an is congruent to a modulo n for
;; every a<n, and try your procedure on the given Carmichael numbers.

(define (expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp) (remainder (square (expmod base (/ exp 2) m))
			   m))
   (else
    (remainder (* base (expmod base (- exp 1) m))
	       m))))

(define (fermat-test? n a)
    (= (expmod a n n) a))

(define (fast-prime? n)
  (define (fast-prime-iter count)
    (cond
     ((= 1 count) true)
     ((fermat-test? n count) (fast-prime-iter (- count 1)))
     (else false)))
  (fast-prime-iter (- n 1)))

;; 561, 1105, 1729, 2465, 2821
(fast-prime? 1105)
