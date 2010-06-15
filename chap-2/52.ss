;; Exercise 2.52. Make changes to the square limit of wave shown in
;; figure 2.9 by working at each of the levels described above. In
;; particular:

;; a. Add some segments to the primitive wave painter of exercise 2.49
;; (to add a smile, for example).

;; b. Change the pattern constructed by corner-split (for example, by
;; using only one copy of the up- split and right-split images instead of
;; two).

;; c. Modify the version of square-limit that uses square-of-four so as
;; to assemble the corners in a different pattern. (For example, you
;; might make the big Mr. Rogers look outward from each corner of the
;; square.)

;; a (add a cross /)
(define (wave)
  (define (m-s x1 y1 x2 y2)
    (make-segment (make-vect x1 y1) (make-vect x2 y2)))
  (segmets->painter
   (list (m-s 0.3 0.0 0.35 0.5)
	 (m-s 0.35 0.5 0.3 0.55)
	 (m-s 0.3 0.55 0.2 0.4)
	 (m-s 0.2 0.4 0.0 6.0)
	 (m-s 0.4 0.0 0.5 0.2)
	 (m-s 0.5 0.2 0.6 0.0)
	 (m-s 0.7 0.0 0.6 0.4)
	 (m-s 0.6 0.4 1.0 0.2)
	 (m-s 1.0 3.0 0.7 0.65)
	 (m-s 0.7 0.65 0.6 0.65)
	 (m-s 0.6 0.65 0.63 0.8)
	 (m-s 0.63 0.8 0.6 1.0)
	 (m-s 0.4 1.0 0.37 0.8)
	 (m-s 0.37 0.8 0.4 0.65)
	 (m-s 0.4 0.65 0.3 0.65)
	 (m-s 0.3 0.65 0.2 0.6)
	 (m-s 0.2 0.6 0.0 0.8)
	 (m-s 0.0 0.0 1.0 1.0))))

;; b TODO do some changes
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))
;; c TODO do some changes
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


