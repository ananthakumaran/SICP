;; Exercise 2.1. Define a better version of make-rat that handles both positive and negative arguments.
;; Make-rat should normalize the sign so that if the rational number is positive, both the numerator and
;; denominator are positive, and if the rational number is negative, only the numerator is negative.

(define (gcd a b)
  (if (= a 0)
      b
      (gcd (remainder b a) a)))
	
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d)))
	(d (abs d))
	(n (if (negative? d)
	       (- n)
	       n))) 
    (cons (/ n g) (/ d g))))

;; test
(make-rat 1 2)
(make-rat 1 -2)
(make-rat -1 2)
(make-rat -1 -2)

