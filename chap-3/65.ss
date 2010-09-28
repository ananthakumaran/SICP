;; *Exercise 3.65:* Use the series

;;                1     1     1
;;    ln 2 = 1 - --- + --- - --- + ...
;;                2     3     4

;; to compute three sequences of approximations to the natural
;; logarithm of 2, in the same way we did above for [pi].  How
;; rapidly do these sequences converge?


(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define (partial-sums s)
  (define result (cons-stream (car s) (add-stream result (stream-cdr s))))
  result)

(define (log-2-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (log-2-summands (+ n 1)))))

(define log-2-stream
  (partial-sums (log-2-summands 1)))

(stream-ref log-2-stream 1000)

