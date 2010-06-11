;; Exercise 1.29. Simpson's Rule is a more accurate method of numerical integration than the method
;; illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as
;; 
;; h/3[y0 + 4y1 + 2y2 + 4y3 + ..... + 2yn-2 + 4yn-1 + yn]
;;
;; where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing n increases the accuracy of the
;; approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of the
;; integral, computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n =
;; 100 and n = 1000), and compare the results to those of the integral procedure shown above.


(define (sum term next a b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term next (next a) b))))


(define (integral f a b n)
  (define (co x)
    (cond
     ((or (= 0 x) (= n x)) 1)
     ((even? x) 2)
     (else 4)))
  (define h (/ (- b a) n))
  (define (next x) (+ x 1))
  (define (fun k)
    (* (co k) 
       (f (+ a (* k h)))))
  (* (sum fun next 0 n) (/ h 3.0)))

(define (cube x) (* x x x))

(integral cube 0 1 10)