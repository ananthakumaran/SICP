;; Exercise 1.40. Define a procedure cubic that can be used together with the newtons-method
;; procedure in expressions of the form

;; (newtons-method (cubic a b c) 1)

;; to approximate zeros of the cubic x3 + ax2 + bx + c.


(define (average a b) (/ (+ a b) 2))
(define (fixed-point f first-guess)
  (define tolerance 0.001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try-next guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try-next next))))
  (try-next first-guess))

(define dx 0.000001)
(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x))
       dx)))


(define (newton-transform g)
  (lambda (x) 
    (- x 
       (/ (g x)
	  ((deriv g) x)))))


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (cubic a b c)
 (fixde-point-of-transform (lambda (x) (+
			     (* x x x)
			     (* a (square x))
			     (* b x)
			     c))
			   newton-transform
			   1)


;; test
(define (cube x) (* x x x))
((deriv cube) 5)
(sqrt 4)
