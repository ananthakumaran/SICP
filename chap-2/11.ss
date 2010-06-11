;; Exercise 2.11. In passing, Ben also cryptically comments: ``By testing
;; the signs of the endpoints of the intervals, it is possible to break
;; mul-interval into nine cases, only one of which requires more than two
;; multiplications.'' Rewrite this procedure using Ben's suggestion.

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
	(x2 (upper-bound x))
	(y1 (lower-bound y))
	(y2 (upper-bound y)))
    (cond
     ((positive? x1)
      (cond))
     (else
      (cond
       ((positive? x2)
	(cond))
       (else
	(cond
	 ((positive? y1) )



