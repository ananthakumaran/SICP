;;Exercise 1.21. Use the smallest-divisor procedure to find the smallest divisor of each of the
;;following numbers: 199, 1999, 19999

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? n test-divisor) test-divisor)
   (else
    (find-divisor n (+ test-divisor 1)))))

(define (divides? n divisor)
  (= 0 (remainder n divisor)))
