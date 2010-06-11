;; Exercise 2.10. Ben Bitdiddle, an expert systems programmer, looks over
;; Alyssa's shoulder and comments that it is not clear what it means to
;; divide by an interval that spans zero. Modify Alyssa's code to check
;; for this condition and to signal an error if it occurs.

(define (div-interval x y)
  (let ((u (upper-bound y))
	(l (lower-bound y)))
    (if (or (zero? u) (zero? l))
	(error " upper bound and lower bound should be non zero " )
	(mul-interval x
		      (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y)))))))

;; test

(define (make-interval a b) (cons a b))

(define (apply f x)
  (let ((a (car x))
	(b (cdr x)))
    (f a b)))

(define (upper-bound x)
  (apply max x))

(define (lower-bound x)
  (apply min x))

(define z (make-interval 0 0))
(define x (make-interval 0 0))

(div-interval z x)
