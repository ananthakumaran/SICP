;; *Exercise 5.4:* Specify register machines that implement each of
;; the following procedures.  For each machine, write a controller
;; instruction sequence and draw a diagram showing the data paths.

;; a. Recursive exponentiation:

;; (define (expt b n)
;;   (if (= n 0)
;;       1
;;       (* b (expt b (- n 1)))))

(controller
 (assign continue (label expt-done))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-expt))
 (goto (label expt-loop))
 after-expt
 (restore contine)
 (assign val (op *) (reg b) (reg val))
 (goto (reg continue))
 base-case
 (assign val (const 1))
 (goto (reg continue))
 expt-done)

;; b. Iterative exponentiation:

;; (define (expt b n)
;;   (define (expt-iter counter product)
;;     (if (= counter 0)
;; 	product
;; 	(expt-iter (- counter 1) (* b product))))
;;   (expt-iter n 1))

(controller
 (assign (reg p) (const 1))
 expt-loop
 (test (op =) (reg c) (const 0))
 (branch (label expt-done))
 (assign c (op -) (reg c) (const 1))
 (assign p (op *) (reg p) (reg b))
 (goto (label expt-loop))
 expt-done)
