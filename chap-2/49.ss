;; Exercise 2.49. Use segments->painter to define the following primitive
;; painters:

;; a. The painter that draws the outline of the designated frame.

;; b. The painter that draws an ``X'' by connecting opposite corners of
;; the frame.

;; c. The painter that draws a diamond shape by connecting the midpoints
;; of the sides of the frame.

;; d. The wave painter.


(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (drawline
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; a

(define (outline->painter)
  (let ((bottom-left (make-vect 0.0 0.0))
	(bottom-right (make-vect 1.0 0.0))
	(top-left (make-vect 0.0 1.0))
	(top-right (make-vect 1.0 1.0)))
    (segments->painter 
     (list (make-segment bottom-left bottom-right)
	   (make-segment bottom-right top-right)
	   (make-segment top-right top-left)
	   (make-segment top-left bottom-left)))))

;; b
(define (x->painter)
  (let ((bottom-left (make-vect 0.0 0.0))
	(bottom-right (make-vect 1.0 0.0))
	(top-left (make-vect 0.0 1.0))
	(top-right (make-vect 1.0 1.0)))
    (segments->painter
     (list (make-segment bottom-left top-right)
	   (make-segment bottom-right top-left)))))

;; c
(define (diamond->painter)
  (let ((bottom (make-vect 0.5 0.0))
	(right (make-vect 1.0 0.5))
	(top (make-vect 0.5 1.0))
	(left (make-vect 0.0 0.5)))
    (segmets->painter
     (list (make-segment bottom right)
	   (make-segment right top)
	   (make-segment top left)
	   (make-segment left bottom)))))

;; d
(define (wave)
  (define (m-s x1 y1 x2 y2)
    (make-segment (make-vect x1 y1) (make-vect x2 y2)))
  (segmets->painter
   (list (m-s  0.3 0.0 0.35 0.5)
	 (m-s  0.35 0.5 0.3 0.55)
	 (m-s  0.3 0.55 0.2 0.4)
	 (m-s  0.2 0.4 0.0 6.0)
	 (m-s  0.4 0.0 0.5 0.2)
	 (m-s  0.5 0.2 0.6 0.0)
	 (m-s  0.7 0.0 0.6 0.4)
	 (m-s  0.6 0.4 1.0 0.2)
	 (m-s  1.0 3.0 0.7 0.65)
	 (m-s  0.7 0.65 0.6 0.65)
	 (m-s  0.6 0.65 0.63 0.8)
	 (m-s  0.63 0.8 0.6 1.0)
	 (m-s  0.4 1.0 0.37 0.8)
	 (m-s  0.37 0.8 0.4 0.65)
	 (m-s  0.4 0.65 0.3 0.65)
	 (m-s  0.3 0.65 0.2 0.6)
	 (m-s  0.2 0.6 0.0 0.8))))


