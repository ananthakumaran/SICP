;; *Exercise 5.18:* Modify the `make-register' procedure of section
;; *Note 5-2-1:: so that registers can be traced.  Registers should
;; accept messages that turn tracing on and off.  When a register is
;; traced, assigning a value to the register should print the name of
;; the register, the old contents of the register, and the new
;; contents being assigned.  Extend the interface to the machine
;; model to permit you to turn tracing on and off for designated
;; machine registers.

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace 'off))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'trace-on) (set! trace 'on))
	    ((eq? message 'trace-off) (set! trace 'off))
            ((eq? message 'set)
             (lambda (value)
               (if (eq? trace 'on)
                   (begin
                     (newline)
		     (display (list 'register name))
                     (display (list 'old-content '= contents))
                     (display (list 'new-content '= value))))
               (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (trace-register machine register-name)
  (((machine 'get-register) register-name) 'trace-on))

(define (un-trace-register machine register-name)
  (((machine 'get-register) register-name) 'trace-off))
