;; Exercise 2.18. Define a procedure reverse that takes a list as
;; argument and returns a list of the same elements in reverse order:

;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)


(define (reverse lat)
  (define (iter lat result)
    (if (null? lat)
	result
	(iter (cdr lat) (cons (car lat) result))))
  (iter lat '()))

(reverse (list 1 4 9 16 25))

