;; Exercise 2.46. A two-dimensional vector v running from the origin to a
;; point can be represented as a pair consisting of an x-coordinate and a
;; y-coordinate. Implement a data abstraction for vectors by giving a
;; constructor make-vect and corresponding selectors xcor-vect and
;; ycor-vect. In terms of your selectors and constructor, implement
;; procedures add-vect, sub-vect, and scale-vect that perform the
;; operations vector addition, vector subtraction, and multiplying a
;; vector by a scalar:


(define (make-vect x y)
  (cons x y))

(define xcor-vect car)
(define ycor-vect cadr)

(define (add-vect a b)
  (make-vect
   (+ (xcor-vect a) (xcor-vect b))
   (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect
   (- (xcor-vect a) (xcor-vect b))
   (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect v s)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))
      

