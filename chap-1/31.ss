;;Exercise 1.31.
;;a. The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as
;;higher-order procedures.51 Write an analogous procedure called product that returns the product of the
;;values of a function at points over a given range. Show how to define factorial in terms of product.
;;Also use product to compute approximations to using the formula52
;;
;; pi/4 = 2.4.4.6.6.8..../3.3.5.5.7.7
;;
;;b. If your product procedure generates a recursive process, write one that generates an iterative process.
;;If it generates an iterative process, write one that generates a recursive process.


;; recursive
(define (product term next a b)
  (if (> a b)
      1
      (* (term a) (product term next (next a) b))))

;; iterative
(define (product term next a b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))


(define (factorial n)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (product identity inc 1 n))


(define (pi n) 
  (define (inc-by-two x) (+ x 2))
  (/ (* 8.0
     (/ (product square inc-by-two 4 n)
	(product square inc-by-two 3 n)))
     n))

(pi 1000)


  

