;; *Exercise 3.17:* Devise a correct version of the `count-pairs'
;; procedure of *Note Exercise 3-16:: that returns the number of
;; distinct pairs in any structure.  (Hint: Traverse the structure,
;; maintaining an auxiliary data structure that is used to keep track
;; of which pairs have already been counted.)

(define (count-pairs x)
  (let ((distint-pair '()))
    (define (loop x)
      (if (or (not (pair? x)) (member x distint-pair))
	  0
	  (begin
	    (set! distint-pair (cons x distint-pair))
	    (+ (loop (car x))
	       (loop (cdr x))
	       1))))
    (loop x)))

;; test
(define x (list 'a 'b))
(define z1 (cons x x))
(count-pairs (cons x x))
(count-pairs (cons (list 'a 'b) (list 'a 'b)))
(count-pairs (list 1 2 3 4 5 6 7))
