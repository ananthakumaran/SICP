;; Exercise 1.37. a. An infinite continued fraction is an expression of the form
;; As an example, one can show that the infinite continued fraction expansion with the Ni and the Di all equal
;; to 1 produces 1/ , where is the golden ratio (described in section 1.2.2). One way to approximate an
;; infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation -- a
;; so-called k-term finite continued fraction -- has the form
;; Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di of the
;; terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n
;; d k) computes the value of the k-term finite continued fraction. Check your procedure by approximating
;; 1/ using
;; (cont-frac (lambda (i) 1.0)
;;                   (lambda (i) 1.0)
;;                   k)
;; for successive values of k. How large must you make k in order to get an approximation that is accurate to
;; 4 decimal places?

;; b. If your cont-frac procedure generates a recursive process, write one that generates an iterative
;; process. If it generates an iterative process, write one that generates a recursive process.


;; recursive
(define (cont-frac n d k)
  (define (cont-recur count)
    (let ((n-value (n k))
	  (d-value (d k)))
      (if (= k count)
	  (/ n-value d-value)
	  (/ n-value (+ d-value (cont-recur (+ count 1)))))))
  (cont-recur 1))

;; iterative
(define (cont-frac n d k)
  (define (cont-iter result k)
    (let ((n-value (n k))
	  (d-value (d k)))
      (if (= 0 k)
	  result
	  (cont-iter (/ n-value (+ d-value result)) (- k 1)))))
(cont-iter (/ (n k) (d k)) (- k 1)))
       
	  
(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10))
