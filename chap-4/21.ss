;; *Exercise 4.21:* Amazingly, Louis's intuition in *Note Exercise
;; 4-20:: is correct.  It is indeed possible to specify recursive
;; procedures without using `letrec' (or even `define'), although the
;; method for accomplishing this is much more subtle than Louis
;; imagined.  The following expression computes 10 factorial by
;; applying a recursive factorial procedure:(4)

;; ((lambda (n)
;;    ((lambda (fact)
;;       (fact fact n))
;;     (lambda (ft k)
;;       (if (= k 1)
;; 	  1
;; 	  (* k (ft ft (- k 1)))))))
;;  10)

;; a. Check (by evaluating the expression) that this really does
;; compute factorials.  Devise an analogous expression for
;; computing Fibonacci numbers.

;; b. Consider the following procedure, which includes mutually
;; recursive internal definitions:

;; (define (f x)
;;   (define (even? n)
;;     (if (= n 0)
;; 	true
;; 	(odd? (- n 1))))
;;   (define (odd? n)
;;     (if (= n 0)
;; 	false
;; 	(even? (- n 1))))
;;   (even? x))

;; Fill in the missing expressions to complete an alternative
;; definition of `f', which uses neither internal definitions
;; nor `letrec':

;; (define (f x)
;;   ((lambda (even? odd?)
;;      (even? even? odd? x))
;;    (lambda (ev? od? n)
;;      (if (= n 0) true (od? <??> <??> <??>)))
;;    (lambda (ev? od? n)
;;      (if (= n 0) false (ev? <??> <??> <??>)))))

;; test
(define factorial (lambda (n)
		     ((lambda (fact)
			(fact fact n))
		      (lambda (ft k)
			(if (= k 1)
			    1
			    (* k (ft ft (- k 1))))))))

(factorial 5)

;; fibonacci
((lambda (n)
  ((lambda (fib)
     (fib fib '() 1 1 0))
   (lambda (fib result a b c)
     (if (= c n)
	 result
	 (fib fib (cons a result) b (+ a b) (+ c 1))))))
   7)

;; f
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 0)
(f 1)
(f 2)
(f 3)
