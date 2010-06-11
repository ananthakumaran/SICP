;; Exercise 2.2. Consider the problem of representing line segments in a
;; plane. Each segment is represented as a pair of points: a starting
;; point and an ending point. Define a constructor make-segment and
;; selectors start-segment and end-segment that define the representation
;; of segments in terms of points. Furthermore, a point can be
;; represented as a pair of numbers: the x coordinate and the y
;; coordinate.  Accordingly, specify a constructor make-point and
;; selectors x-point and y-point that define this
;; representation. Finally, using your selectors and constructors, define
;; a procedure midpoint-segment that takes a line segment as argument and
;; returns its midpoint (the point whose coordinates are the average of
;; the coordinates of the endpoints). To try your procedures, you'll need
;; a way to print points:

;; (define (print-point p)
;;    (newline)
;;    (display "(")
;;    (display (x-point p))
;;    (display ",")
;;    (display (y-point p))
;;    (display ")"))


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

(define (midpoint-segment line)
  (let ((x1 (x-point (start-segment line)))
	(y1 (y-point (start-segment line)))
	(x2 (x-point (end-segment line)))
	(y2 (y-point (end-segment line))))
    (make-point (average x1 x2)
		(average y1 y2))))

(define (average x y) (/ (+ x y) 2))


(define p1 (make-point 10 0))
(define p2 (make-point 20 10))
(midpoint-segment (make-segment p1 p2))