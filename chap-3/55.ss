;; *Exercise 3.55:* Define a procedure `partial-sums' that takes as
;; argument a stream S and returns the stream whose elements are S_0,
;; S_0 + S_1, S_0 + S_1 + S_2, ....  For example, `(partial-sums
;; integers)' should be the stream 1, 3, 6, 10, 15, ....

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define (from x)
  (cons-stream x 
	       (from (+ x 1))))

(define (partial-sums s)
  (define result (cons-stream (car s) (add-stream result (stream-cdr s))))
  result)

(define ex (partial-sums (from 1)))

(define (loop from to p)
  (if (> from to)
      'done
      (begin 
	(p from)
	(loop (+ from 1) to p))))

(loop 0 10 (lambda (x)
	     (display (stream-ref ex x))
	     (display " ")))



