;; Exercise 2.40. Define a procedure unique-pairs that, given an integer
;; n, generates the sequence of pairs (i,j) with 1< j< i< n. Use
;; unique-pairs to simplify the definition of prime-sum-pairs given
;; above.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

;; test
(unique-pairs 4)

(define (prime? n)
  (define (find-divisor n test-divisor)
    (cond
     ((> (square test-divisor) n) n)
     ((divides? n test-divisor) test-divisor)
     (else
      (find-divisor n (+ test-divisor 1)))))
  (define (divides? n divisor)
    (= 0 (remainder n divisor)))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (prime-pair? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (let ((a (car pair))
	(b (cadr pair)))
    (list a b (+ a b))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-pair?
	       (unique-pairs n))))

;; test
(prime-sum-pairs 6)


