;; Exercise 2.7. Alyssa's program is incomplete because she has not
;; specified the implementation of the interval abstraction. Here is a
;; definition of the interval constructor:

;; (define (make-interval a b) (cons a b))

;; Define selectors upper-bound and lower-bound to complete the
;; implementation.

(define (make-interval a b) (cons a b))

(define (apply f x)
  (let ((a (car x))
	(b (cdr x)))
    (f a b)))

(define (upper-bound x)
  (apply max x))

(define (lower-bound x)
  (apply min x))

;;test
(define inter (make-interval 11 45))
(upper-bound inter)
(lower-bound inter)