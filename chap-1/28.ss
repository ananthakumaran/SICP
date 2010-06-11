(define (expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp) (remainder (square (expmod base (/ exp 2) m))
			   m))
   (else
    (remainder (* base (expmod base (- exp 1) m))
	       m))))

(define (miller-rabin-test? n )
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ (random (- n 1)) 1)))

(define (fast-prime? n times)
  (cond
   ((= 0 times) true)
   ((miller-rabin-test? n) (fast-prime? n (- times 1)))
   (else false)))