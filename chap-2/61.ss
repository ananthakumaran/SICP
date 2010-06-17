;; Exercise 2.61. Give an implementation of adjoin-set using the ordered
;; representation. By analogy with element-of-set? show how to take
;; advantage of the ordering to produce a procedure that requires on the
;; average about half as many steps as with the unordered representation.

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (cond ((= x (car set)) set)
	    ((< x (car set)) (cons x set))
	    (else (cons (car set) (adjoin-set x (cdr set)))))))

;; test
(adjoin-set 2 '(5))
(adjoin-set 2 '(1  5))
(adjoin-set 2 '(1))


