(define (search-for-primes start end)
  (cond
   ((> start end) true)
   (((lambda ()
      (timed-prime-test start)
      false)))
   (else
       (search-for-primes (+ start 2) end))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-time (- (runtime)  start-time))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-time elapsed-time)
  (display "*****")
  (display elapsed-time))


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
