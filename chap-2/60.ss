;; Exercise 2.60. We specified that a set would be represented as a list
;; with no duplicates. Now suppose we allow duplicates. For instance, the
;; set {1,2,3} could be represented as the list (2 3 2 1 3 2 2).  Design
;; procedures element-of-set?, adjoin-set, union-set, and
;; intersection-set that operate on this representation. How does the
;; efficiency of each compare with the corresponding procedure for the
;; non-duplicate representation? Are there applications for which you
;; would use this representation in preference to the non-duplicate one?

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else
	 (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else
	 (intersection-set (cdr set1) set2))))
    
(define (union-set set1 set2)
  (append set1 set2))

;; n - size of elements
;; 
;; element-of-set? 
;; unique O(n)
;; duplicate O(n)
;;
;; adjoin-set
;; unique O(n)
;; duplicate O(1)
;;
;; intersection-set
;; unique O(n2)
;; duplicate O(n2)
;;
;; union-set 
;; unique O(n2)
;; duplicate O(n) (appends value)


