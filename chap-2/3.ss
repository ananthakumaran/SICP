;; Exercise 2.3. Implement a representation for rectangles in a
;; plane. (Hint: You may want to make use of exercise 2.2.) In terms of
;; your constructors and selectors, create procedures that compute the
;; perimeter and the area of a given rectangle. Now implement a different
;; representation for rectangles. Can you design your system with
;; suitable abstraction barriers, so that the same perimeter and area
;; procedures will work using either representation?


(define (make-segment start end)
  (cons start end))

(define start-segment car)
(define end-segment cdr)

(define (make-point x y)
  (cons x y))

(define x-point car)
(define y-point cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-rectangle length breadth)
  (cons length breadth))

(define length car)
(define breadth car)


(define (distance segment)
  (let ((x1 (x-point (start-segment segment)))
	(y1 (y-point (start-segment segment)))
	(x2 (x-point (end-segment segment)))
	(y2 (y-point (end-segment segment))))
    (+ (square (- x2 x1))
       (square (- y2 y1)))))


(define p1 (make-point 0 0))
(define p2 (make-point 1 0))

(define to 
