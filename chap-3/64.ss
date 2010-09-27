;; *Exercise 3.64:* Write a procedure `stream-limit' that takes as
;; arguments a stream and a number (the tolerance).  It should
;; examine the stream until it finds two successive elements that
;; differ in absolute value by less than the tolerance, and return
;; the second of the two elements.  Using this, we could compute
;; square roots up to a given tolerance by

;; (define (sqrt x tolerance)
;;   (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit stream tolerance)
  (let ((a (stream-car stream))
	(b (stream-car (stream-cdr stream))))
    (if (<= (abs (- a b)) tolerance)
	b
	(stream-limit (stream-cdr stream) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; test
(sqrt 5 .000001)
(sqrt 4 .00000001)

