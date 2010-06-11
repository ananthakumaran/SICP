;; Exercise 2.41. Write a procedure to find all ordered triples of
;; distinct positive integers i, j, and k less than or equal to a given
;; integer n that sum to a given integer s.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond
   ((null? sequence) '())
   ((predicate (car sequence))
    (cons (car sequence) 
	  (filter predicate (cdr sequence))))
   (else
    (filter predicate (cdr sequence)))))


(define (triples n s)
  (filter (ordered-triple? s)
	  (flatmap (lambda (i)
		     (flatmap (lambda (j)
				(map (lambda (k) (list i j k))
				     (enumerate-interval 1 (- j 1))))
			      (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

(define (ordered-triple? s)
  (lambda (triple)
    (and (= (length triple) 3)
	 (= (accumulate + 0 triple) s))))

;; test
(triples 5 10)
