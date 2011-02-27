;; *Exercise 5.13:* Modify the simulator so that it uses the
;; controller sequence to determine what registers the machine has
;; rather than requiring a list of registers as an argument to
;; `make-machine'.  Instead of pre-allocating the registers in
;; `make-machine', you can allocate them one at a time when they are
;; first seen during assembly of the instructions.

;; machine
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; create the register if not found
(define (lookup-register name)
  (let ((val (assoc name register-table)))
    (if val
	(cadr val)
	(begin
	  (allocate-register name)
	  (lookup-register name)))))
