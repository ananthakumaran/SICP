;; Exercise 2.32. We can represent a set as a list of distinct elements,
;; and we can represent the set of all subsets of the set as a list of
;; lists. For example, if the set is (1 2 3), then the set of all subsets
;; is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following
;; definition of a procedure that generates the set of subsets of a set
;; and give a clear explanation of why it works:

;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;; 	(append rest (map <??> rest)))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))


(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
	(append rest (map 
		      (lambda (x)
			(cons (car s) x)) rest)))))


;; it is easy to explain with a example
;; eg  (1 2)
;;
;; 1. first find the subset for all the element except the first element
;;    so for the example the subset of (2) is
;;    (() (2))

;; 2. now  insert the first element ie (1) in all the list
;;    ((1) (1 2))

;; 3. append the above two list
;;    (() (2) (1) (1 2))

;; boundary case
;; subset of empty list is '(())


