;; Exercise 1.39. A continued fraction representation of the tangent function was published in 1770 by the
;; German mathematician J.H. Lambert:
;; where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent
;; function based on Lambert's formula. K specifies the number of terms to compute, as in exercise 1.37.


(define (cont-frac n d k combiner)
  (define (cont-iter result k)
    (let ((n-value (n k))
	  (d-value (d k)))
      (if (= 0 k)
	  result
	  (cont-iter (/ n-value (combiner d-value result)) (- k 1)))))
(cont-iter (/ (n k) (d k)) (- k 1)))


(define (tan-cf x k)
  (cont-frac (lambda (n) 
	       (if (= 1 n)
		   x
		   (square x)))
	     (lambda (n)
	       (- (* n  2) 1))
	     k
	     -))

(tan-cf 60.0 100)

(tan 60)