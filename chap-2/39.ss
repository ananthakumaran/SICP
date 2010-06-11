;; Exercise 2.39. Complete the following definitions of reverse (exercise
;; 2.18) in terms of fold-right and fold-left from exercise 2.38:

;; (define (reverse sequence)
;;    (fold-right (lambda (x y) <??>) nil sequence))
;; (define (reverse sequence)
;;    (fold-left (lambda (x y) <??>) nil sequence))


(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
   (fold-right (lambda (x result) (append result (list x))) '() sequence))
(define (reverse sequence)
   (fold-left (lambda (result x) (cons x result)) '() sequence))

(reverse '(1 2 3))


(append '(1 2) '(2 3))
