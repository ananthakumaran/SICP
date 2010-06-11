;; Exercise 1.30. The sum procedure above generates a linear recursion. The procedure can be rewritten so
;; that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the
;; following definition:
;;
;; (define (sum term a next b)
;;    (define (iter a result)
;;        (if <??>
;;             <??>
;;             (iter <??> <??>)))
;;    (iter <??> <??>))
;;

(define (sum term next a b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

;; test
(define (sum-of-integers a b)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (sum identity inc a b))

(sum-of-integers 1 5)
	

