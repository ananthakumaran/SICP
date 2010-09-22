;; *Exercise 3.54:* Define a procedure `mul-streams', analogous to
;; `add-streams', that produces the elementwise product of its two
;; input streams.  Use this together with the stream of `integers' to
;; complete the following definition of the stream whose nth element
;; (counting from 0) is n + 1 factorial:

;; (define factorials (cons-stream 1 (mul-streams <??> <??>)))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials (from 1))))

(define (from x)
  (cons-stream x 
	       (from (+ x 1))))

(stream-ref factorials 5)
