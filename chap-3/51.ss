;; *Exercise 3.51:* In order to take a closer look at delayed
;; evaluation, we will use the following procedure, which simply
;; returns its argument after printing it:

;; (define (show x)
;;   (display-line x)
;;   x)

;; What does the interpreter print in response to evaluating each
;; expression in the following sequence?(7)

;; (define x (stream-map show (stream-enumerate-interval 0 10)))

;; (stream-ref x 5)

;; (stream-ref x 7)

(define (display-line x)
  (newline)
  (display x))
(define (show x)
  (display-line x)
  x)
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

(define x (stream-map show (stream-enumerate-interval 0 10)))

;;; solution starts here
(stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5
;;Value: 5
(stream-ref x 7)
;; 6
;; 7
;;Value: 7
(stream-ref x 5)
;;Value: 5

;; delay function will memorize the value during the first call of 
;; the function and won't compute the value if it is called again