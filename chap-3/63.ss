;; *Exercise 3.63:* Louis Reasoner asks why the `sqrt-stream'
;; procedure was not written in the following more straightforward
;; way, without the local variable `guesses':

;; (define (sqrt-stream x)
;;   (cons-stream 1.0
;; 	       (stream-map (lambda (guess)
;; 			     (sqrt-improve guess x))
;; 			   (sqrt-stream x))))

;; Alyssa P. Hacker replies that this version of the procedure is
;; considerably less efficient because it performs redundant
;; computation.  Explain Alyssa's answer.  

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (display x)
  (newline)
  (cons-stream 1.0
	       (stream-map (lambda (guess)
			     (sqrt-improve guess x))
			   (sqrt-stream x))))

(stream-ref (sqrt-stream 2) 10)
;; 2
;; 2
;; 2
;; 2
;; 2
;; 2
;; 2
;; 2
;; 2
;; 2
;; 2
;; ;Value: 1.414213562373095

;; This methods creates n streams to calculate n numbers
;; for eg for n = 4 the above method will create a streams
;; like this
;; (2  1... 1... 1...)
;;     (2   1... 1...)
;;          (2   1...)
;;               (2  )
    


;; Would the two versions
;; still differ in efficiency if our implementation of `delay' used
;; only `(lambda () <EXP>)' without using the optimization provided
;; by `memo-proc' (section *Note 3-5-1::)?

;; The delay will not affect the above method bcoz it already calculating
;; the numbers by redundant computation. But the delay method will affect
;; the previous sqrt-stream method