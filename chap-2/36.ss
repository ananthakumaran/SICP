;; Exercise 2.36. The procedure accumulate-n is similar to accumulate
;; except that it takes as its third argument a sequence of sequences,
;; which are all assumed to have the same number of elements. It applies
;; the designated accumulation procedure to combine all the first
;; elements of the sequences, all the second elements of the sequences,
;; and so on, and returns a sequence of the results. For instance, if s
;; is a sequence containing four sequences, ((1 2 3) (4 5 6) (7 8 9) (10
;; 11 12)), then the value of (accumulate-n + 0 s) should be the sequence
;; (22 26 30). Fill in the missing expressions in the following
;; definition of accumulate-n:

;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;;       nil
;;       (cons (accumulate op init <??>)
;; 	    (accumulate-n op init <??>))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (seqs-n select seq)
  (if (null? seq)
      '()
      (cons (select seq)
	    (seqs-n select (cdr seq)))))
  
(define (car-n seq)
  (seqs-n caar seq))
(define (cdr-n seq)
  (seqs-n cdar seq))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (car-n seqs))
	    (accumulate-n op init (cdr-n seqs)))))

(accumulate-n +
	      0
	      '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
