;; Exercise 2.21. The procedure square-list takes a list of numbers as
;; argument and returns a list of the squares of those numbers.

;; (square-list (list 1 2 3 4))
;; (1 4 9 16)

;; Here are two different definitions of square-list. Complete both of
;; them by filling in the missing expressions:

;; (define (square-list items)
;;   (if (null? items)
;;       nil
;;       (cons <??> <??>)))

;; (define (square-list items)
;;   (map <??> <??>))

(define (map f lat)
  (if (null? lat)
      '()
      (cons (f (car lat))
	    (map f (cdr lat)))))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (square x)) items))

(square-list (list 1 2 3 4))

