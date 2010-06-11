;; Exercise 1.33. You can obtain an even more general version of accumulate (exercise 1.32) by
;; introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived
;; from values in the range that satisfy a specified condition. The resulting filtered-accumulate
;; abstraction takes the same arguments as accumulate, together with an additional predicate of one argument
;; that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the
;; following using filtered-accumulate:
;;
;; a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime?
;; predicate already written)
;;
;; b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i
;; < n such that GCD(i,n) = 1).

(define (filtered-accumulate combiner filter? null-value term next a b)
  (if (> a b)
      null-value
      (if (filter? a)
	  (combiner (term a) (filtered-accumulate combiner filter? null-value term next (next a) b))
	  (filtered-accumulate combiner filter? null-value term next (next a) b))))

(define (sum filter? term next a b)
  (filtered-accumulate + filter? 0 term next a b))

(define (product filter? term next a b)
  (filtered-accumulate * filter? 1 term next a b))

(define (sum-of-square-of-prime a b)
  (define (inc x) (+ x 1))
  (sum prime? square inc a b))

(define (prime? n)
(define (smallest-divisor n)
(define (find-divisor n test-divisor)
  (define (divides? n divisor)
    (= 0 (remainder n divisor)))
  (cond
   ((> (square test-divisor) n) n)
   ((divides? n test-divisor) test-divisor)
   (else
    (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))
  (= (smallest-divisor n) n))

(define (product-of-relative-prime n)
  (define (gcd? i)
    (= (gcd i n) 1))
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (product gcd? identity inc 1 n))

(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

;; test
(sum-of-square-of-prime 2 3)
(product-of-relative-prime 5)