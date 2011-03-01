;; *Exercise 5.16:* Augment the simulator to provide for "instruction
;; tracing".  That is, before each instruction is executed, the
;; simulator should print the text of the instruction.  Make the
;; machine model accept `trace-on' and `trace-off' messages to turn
;; tracing on and off.

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-executed 0)
	(trace 'off))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin
                (allocate-register name)
                (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
		(if (eq? trace 'on)
		    (begin
		      (newline)
		      (display (caar insts))))
                ((instruction-execution-proc (car insts)))
                (set! instruction-executed (+ 1 instruction-executed))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (stack 'initialize)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
	      ((eq? message 'trace-on) (set! trace 'on))
	      ((eq? message 'trace-off) (set! trace 'off))
              ((eq? message 'instruction-executed)
	       (begin
		 (newline)
		 (display (list 'instruction-executed '= instruction-executed))
                 (set! instruction-executed 0)))
	       (else (error "Unknow request -- MACHINE" message))))
	dispatch)))
