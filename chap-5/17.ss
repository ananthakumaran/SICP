;; *Exercise 5.17:* Extend the instruction tracing of *Note Exercise
;; 5-16:: so that before printing an instruction, the simulator
;; prints any labels that immediately precede that instruction in the
;; controller sequence.  Be careful to do this in a way that does not
;; interfere with instruction counting (*Note Exercise 5-15::).  You
;; will have to make the simulator retain the necessary label
;; information.

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
		(if (not (label? (caar insts)))
		    (set! instruction-executed (+ 1 instruction-executed)))
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

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text)))
			  (if (symbol? next-inst)
			      (receive (cons (make-instruction next-inst)
					     insts)
				       (cons (make-label-entry next-inst
							       insts)
					     labels))
			      (receive (cons (make-instruction next-inst)
					     insts)
				       labels)))))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((label? inst)
	 (make-label inst machine pc))
	((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto inst machine labels pc))
	((eq? (car inst) 'save)
	 (make-save inst machine stack pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine stack pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	(else (error "Unknown instruction type -- ASSEMBLE"
		     list))))


(define (make-label inst machine pc)
  (lambda ()
    (advance-pc pc)))

(define (label? inst)
  (symbol? inst))
