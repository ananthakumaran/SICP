;; Exercise 2.59. Implement the union-set operation for the
;; unordered-list representation of sets.

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else
	 (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond  ((null? set1) set2)
	 ((not (element-of-set? (car set1) set2))
	  (cons (car set1)
		(union-set (cdr set1) set2)))
	 (else
	  (union-set (cdr set1) set2))))

;; test
(union-set '(1 2 3) '(4 5 6))
(union-set '(1 2 3) '(2 3 4))
(union-set '() '(2))
(union-set '() '())


