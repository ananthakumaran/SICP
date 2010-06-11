;; Exercise 2.5. Show that we can represent pairs of nonnegative integers
;; using only numbers and arithmetic operations if we represent the pair
;; a and b as the integer that is the product 2a 3b. Give the
;; corresponding definitions of the procedures cons, car, and cdr.


(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (pow-count n d)
  (define (iter result count)
    (let ((r (remainder result d)))
      (if (not (zero? r))
	  count
	  (iter (/ result d) (+ count 1)))))
  (iter n 0))
  

(define (car x)
  (pow-count x 2))
(define (cdr x)
  (pow-count x 3))

(car (cons 3 5))
(cdr (cons 0 1))

	  
