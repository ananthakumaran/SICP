;; Exercise 2.37. Suppose we represent vectors v = (vi) as sequences of
;; numbers, and matrices m = (mij) as sequences of vectors (the rows of
;; the matrix). For example, the matrix is represented as the sequence
;; ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use
;; sequence operations to concisely express the basic matrix and vector
;; operations. These operations (which are described in any book on
;; matrix algebra) are the following: We can define the dot product as17

;; (define (dot-product v w)
;;     (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for
;; computing the other matrix operations. (The procedure accumulate-n is
;; defined in exercise 2.36.)

;; (define (matrix-*-vector m v)
;;     (map <??> m))
;; (define (transpose mat)
;;     (accumulate-n <??> <??> mat))
;; (define (matrix-*-matrix m n)
;;     (let ((cols (transpose n)))
;;        (map <??> m)))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; test
(dot-product '(1 2 3) '(4 5 6))


(define (matrix-*-vector m v)
    (map (lambda (row)
	   (map * v row))
	 m))

;; test
(matrix-*-vector '((1 2 3) (1 2 3) (1 2 3)) '(2 1 2))


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


(define (transpose mat)
    (accumulate-n cons '() mat))

;; test
(transpose '((1 1 1) (1 2 2) (3 3 3)))


(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
      (map (lambda (x)
	     (accumulate-n + 0 
			   (transpose (map (lambda (y)
					     (map * x y))
					   cols))))
	   m)))

;; test
(matrix-*-matrix '((2 0) (0 2)) '((1 2) (1 5)))
