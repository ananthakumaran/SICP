;; *Exercise 3.58:* Give an interpretation of the stream computed by
;; the following procedure:

;; (define (expand num den radix)
;;   (cons-stream
;;    (quotient (* num radix) den)
;;    (expand (remainder (* num radix) den) den radix)))

;; (`Quotient' is a primitive that returns the integer quotient of two
;; integers.)  What are the successive elements produced by `(expand
;; 1 7 10)'?  What is produced by `(expand 3 8 10)'?

(define (loop from to p)
  (if (> from to)
      'done
      (begin 
	(p from)
	(loop (+ from 1) to p))))

(define (display-stream e x)
  (display (stream-ref e x))
  (display " "))

(define (expand num den radix)
   (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(define e (expand 1 7 10))
(loop 0 100 (lambda (x) (display-stream e x)))


;; 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5
;; 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8
;; 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5
;; ;Value: done

(define e (expand 3 8 10))
(loop 0 100 (lambda (x) (display-stream e x)))

;; 3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;; 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;; 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;; ;Value: done

