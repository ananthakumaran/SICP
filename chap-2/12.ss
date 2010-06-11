;; Exercise 2.12. Define a constructor make-center-percent that takes a
;; center and a percentage tolerance and produces the desired
;; interval. You must also define a selector percent that produces the
;; percentage tolerance for a given interval. The center selector is the
;; same as the one shown above.

(define (make-interval a b) (cons a b))

(define (apply f x)
  (let ((a (car x))
	(b (cdr x)))
    (f a b)))

(define (upper-bound x)
  (apply max x))

(define (lower-bound x)
  (apply min x))

(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (let ((w (- (center i) (lower-bound i))))
    (/ (* 100 w) (center i))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))


;; test
(define mkp (make-center-percent 3.5 4.28))
(center mkp)
(percent mkp)





