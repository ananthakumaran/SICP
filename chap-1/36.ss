;; Exercise 1.36. Modify fixed-point so that it prints the sequence of approximations it generates, using
;; the newline and display primitives shown in exercise 1.22. Then find a solution to x^x = 1000 by
;; log(1000)/log(x). (Use Scheme's primitive log procedure, which computes finding a fixed point of x
;; natural logarithms.) Compare the number of steps this takes with and without average damping. (Note that
;; you cannot start fixed-point with a guess of 1, as this would cause division by log(1) = 0.)

(define (fixed-point f first-guess)
  (define tolerance 0.001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try-next guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
	  next
	  (try-next next))))
  (try-next first-guess))


;; x^x = 1000  
;; x = log(1000)/log(x)

;; tests
(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 101)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 57)


