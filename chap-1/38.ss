;; Exercise 1.38. In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus
;; Continuis, which included a continued fraction expansion for e - 2, where e is the base of the natural
;; logarithms. In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....
;; Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e, based on
;; Euler's expansion.

(define (cont-frac n d k)
  (define (cont-iter result k)
    (let ((n-value (n k))
	  (d-value (d k)))
      (if (= 0 k)
	  result
	  (cont-iter (/ n-value (+ d-value result)) (- k 1)))))
(cont-iter (/ (n k) (d k)) (- k 1)))

;; x  2 5 8 11
;; -----------
;; r  2 4 6 8
;; 
;; r = ((x+1)/3) * 2
(define (fractionibus x)
  (cond
   ((= 0 (remainder (+ x 1) 3)) (* (/ (+ x 1) 3) 2))
   (else 1)))


(cont-frac (lambda (x) 1.0) fractionibus 100)
