;; Exercise 2.48. A directed line segment in the plane can be represented
;; as a pair of vectors -- the vector running from the origin to the
;; start-point of the segment, and the vector running from the origin to
;; the end- point of the segment. Use your vector representation from
;; exercise 2.46 to define a representation for segments with a
;; constructor make-segment and selectors start-segment and end-segment.

(define (make-vect x y)
  (cons x y))

(define (make-segment x y)
  (cons x y))

(define start-segment car)
(define end-segment cdr)

;; test
(define a (make-vect 1 1))
(define b (make-vect 2 2))

(define seg (make-segment a b))
(start-segment seg)
(end-segment seg)