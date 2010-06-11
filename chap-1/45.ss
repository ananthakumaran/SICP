;; Exercise 1.45. We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed
;; point of y -> x/y does not converge, and that this can be fixed by average damping. The same method
;; works for finding cube roots as fixed points of the average-damped y x/y2. Unfortunately, the process
;; does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for y
;; x/y3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average
;; damp of y x/y3) the fixed-point search does converge. Do some experiments to determine how many
;; average damps are required to compute nth roots as a fixed-point search based upon repeated average
;; damping of y x/yn-1. Use this to implement a simple procedure for computing nth roots using fixed-
;; point, average-damp, and the repeated procedure of exercise 1.43. Assume that any arithmetic
;; operations you need are available as primitives.



(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try-next guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try-next next))))
  (try-next first-guess))


(define (average-damp f)
  (define (average a b) (/ (+ a b) 2))
  (lambda (x) (average x (f x))))



(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) (f ((repeated f (- n 1)) x)))))

(define (fifth-root x)
  (fixed-point
   (repeated (average-damp (lambda (y) (/ x (product y 3)))) 3)
   1.0))

(define (product x n)
   (if (= n 0) 
       1
       (* x (product x (- n 1)))))

(fifth-root 16)


(define (sqrt x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point
   (average-damp (lambda (y) (/ x (square y))))
   1.0))
