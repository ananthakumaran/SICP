;; Exercise 1.44. The idea of smoothing a function is an important concept in signal processing. If f is a
;; function and dx is some small number, then the smoothed version of f is the function whose value at a point
;; x is the average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth that takes as input a procedure
;; that computes f and returns a procedure that computes the smoothed f. It is sometimes valuable to
;; repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtained the n-fold
;; smoothed function. Show how to generate the n-fold smoothed function of any given function using
;; smooth and repeated from exercise 1.43.


(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) (f ((repeated f (- n 1)) x)))))

(define dx 0.0001)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

((smooth square) 2)

(define (n-fold-smooth n)
  (lambda (f x)
   ((repeated (smooth f) n) x)))

((n-fold-smooth 5) square 1)

	
	  
