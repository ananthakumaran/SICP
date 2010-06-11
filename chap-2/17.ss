;; Exercise 2.17. Define a procedure last-pair that returns the list that
;; contains only the last element of a given (nonempty) list:

;; (last-pair (list 23 72 149 34))
;; (34)


(define (last-pair lat)
  (if (null? (cdr lat))
      lat
      (last-pair (cdr lat))))

(last-pair (list 23 72 149 34))