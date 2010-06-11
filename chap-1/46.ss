;; Exercise 1.46. Several of the numerical methods described in this chapter are instances of an extremely
;; general computational strategy known as iterative improvement. Iterative improvement says that, to
;; compute something, we start with an initial guess for the answer, test if the guess is good enough, and
;; otherwise improve the guess and continue the process using the improved guess as the new guess. Write a
;; procedure iterative-improve that takes two procedures as arguments: a method for telling whether a
;; guess is good enough and a method for improving a guess. Iterative-improve should return as its
;; value a procedure that takes a guess as argument and keeps improving the guess until it is good enough.
;; Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms
;; of iterative-improve.


(define (iterative-improvement is-good? improve)
  (define (improve-iter guess) 
    (let ((next (improve guess)))
      (if (is-good? guess next)
	  guess
	  (improve-iter next))))
  (lambda (x) (improve-iter x)))


(define (fixed-point f first-guess)
  (define tolerance 0.001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  ((iterative-improvement close-enough? f) first-guess))

(define (average-damp f)
  (define (average a b) (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1.0))


;; test
(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
(sqrt 4)

