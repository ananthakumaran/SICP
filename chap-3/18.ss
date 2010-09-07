;; *Exercise 3.18:* Write a procedure that examines a list and
;; determines whether it contains a cycle, that is, whether a program
;; that tried to find the end of the list by taking successive `cdr's
;; would go into an infinite loop.  *Note Exercise 3-13:: constructed
;; such lists.

(define (cycle? x)
  (define (loop x visited-pairs)
    (cond ((null? x) #f)
	  ((member x visited-pairs) #t)
	  (else (loop (cdr x) (cons x visited-pairs)))))
  (loop x '()))

;; test
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(cycle? (list 5 4 4))
(cycle? (make-cycle (list 5 4 4)))


