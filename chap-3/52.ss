;; *Exercise 3.52:* Consider the sequence of expressions

;; (define sum 0)

;; (define (accum x)
;;   (set! sum (+ x sum))
;;   sum)

;; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; (define y (stream-filter even? seq))
;; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;; 			 seq))

;; (stream-ref y 7)

;; (display-stream z)

;; What is the value of `sum' after each of the above expressions is
;; evaluated?  What is the printed response to evaluating the
;; `stream-ref' and `display-stream' expressions?  Would these
;; responses differ if we had implemented `(delay <EXP>)' simply as
;; `(lambda () <EXP>)' without using the optimization provided by
;; `memo-proc'?  Explain

(define (display-line x)
  (newline)
  (display x))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
			 seq))

;;; solution starts here
(stream-ref y 7)
;; ;Value: 136

(display-stream z)
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210
;; ;Value: done

;; if we invoke the `z' procedure multiple times then the implementation of
;; the delay procedure will affect the result. So the implementation of
;; delay matters when the functions passed to the `stream-*' introduces
;; side effects(such as assignment in the above).