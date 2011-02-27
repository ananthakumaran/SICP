;; *Exercise 5.11:* When we introduced `save' and `restore' in
;; section *Note 5-1-4::, we didn't specify what would happen if you
;; tried to restore a register that was not the last one saved, as in
;; the sequence

;; (save y)
;; (save x)
;; (restore y)

;; There are several reasonable possibilities for the meaning of
;; `restore':

;; a. `(restore y)' puts into `y' the last value saved on the stack,
;; regardless of what register that value came from.  This is
;; the way our simulator behaves.  Show how to take advantage of
;; this behavior to eliminate one instruction from the Fibonacci
;; machine of section *Note 5-1-4:: (*Note Figure 5-12::).

........
afterfib-n-2   ; upon return, `val' contains _Fib_(n - 2)

;; previous
;; (assign n (reg val))               ; `n' now contains _Fib_(n - 2)
;; (restore val)                      ; `val' now contains _Fib_(n - 1)

(restore n) ; now n contains fib(n - 1) and val contains fib(n - 2)

(restore continue)
(assign val                        ;  _Fib_(n - 1) +  _Fib_(n - 2)
	(op +) (reg val) (reg n))
(goto (reg continue))              ; return to caller, answer is in `val'
.......

;; b. `(restore y)' puts into `y' the last value saved on the
;; stack, but only if that value was saved from `y'; otherwise,
;; it signals an error.  Modify the simulator to behave this
;; way.  You will have to change `save' to put the register name
;; on the stack along with the value.

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value) (set! contents value)))
	    ((eq? message 'name) name)
	    (else
	     (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-name register)
  (register 'name))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (stack-entry (get-name reg) (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (let ((entry (pop stack)))
	(if (eq? (get-name reg) (stack-entry-reg entry))
	    (begin
	      (set-contents! reg (stack-entry-value entry))
	      (advance-pc pc))
	    (error "Last value saved by different register" reg))))))

(define (stack-entry reg value)
  (cons reg value))

(define (stack-entry-reg entry)
  (car entry))

(define (stack-entry-value entry)
  (cdr entry))

(define test
  (make-machine
   '(a b)
   '()
   '((save a)
     (restore b))))

(start test)

;; error
;; Last value saved by different register


;; c. `(restore y)' puts into `y' the last value saved from `y'
;; regardless of what other registers were saved after `y' and
;; not restored.  Modify the simulator to behave this way.  You
;; will have to associate a separate stack with each register.
;; You should make the `initialize-stack' operation initialize
;; all the register stacks.

;; machine
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
		((machine 'allocate-register) register-name))
	      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; registers
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value) (set! contents value)))
	    ((eq? message 'name) name)
	    (else
	     (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-name register)
  (register 'name))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; stack
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
	  (error "Empty stack -- POP")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) (pop))
	    ((eq? message 'initialize) (initialize))
	    (else (error "Unknow request -- STACK"
			 message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;; machine
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack-table '())
	(the-instruction-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda ()
			 (foreach (lambda (entry)
				    ((stack-entry-stack entry) 'initialize))
				  stack-table)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (begin
	      (set! register-table
		    (cons (list name (make-register name))
			  register-table))
	      (set! stack-table
		    (cons (list name (make-stack))
			  stack-table))))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq) (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register) allocate-register)
	      ((eq? message 'get-register) lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack)
	       (lambda (reg) (stack-entry-stack (assoc reg stack-table))))
	      ((eq? message 'operations) the-ops)
	      (else (error "Unknow request -- MACHINE" message))))
      dispatch)))

(define (get-register machine register-name)
  ((machine 'get-register) register-name))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (stack-entry-stack entry)
  (cadr entry))

(define (stack-entry-name entry)
  (car entry))

(define (get-stack machine register-name)
  ((machine 'stack) register-name))

;; assembler

(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts labels machine)
		    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text)))
			  (if (symbol? next-inst)
			      (receive insts
				       (cons (make-label-entry next-inst
							       insts)
					     labels))
			      (receive (cons (make-instruction next-inst)
					     insts)
				       labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
	inst
	(make-execution-procedure
	 (instruction-text inst) labels machine pc flag ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))


(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
	(cdr val)
	(error "Undefined label -- ASSEMBLE" label-name))))


(define (make-execution-procedure inst labels machine pc flag ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto inst machine labels pc))
	((eq? (car inst) 'save)
	 (make-save inst machine pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	(else (error "Unknown instruction type -- ASSEMBLE"
		     list))))

(define (make-assign inst machine labels ops pc)
  (let ((target (get-register machine (assign-reg-name inst)))
	(value-exp (assign-value-exp inst)))
    (let ((value-proc
	   (if (operation-exp? value-exp)
	       (make-operation-exp
		value-exp machine labels ops)
	       (make-primitive-exp
		(car value-exp) machine labels))))
      (lambda ()
	(set-contents! target (value-proc))
	(advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))


(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(let ((condition-proc
	       (make-operation-exp
		condition machine labels operations)))
	  (lambda ()
	    (set-contents! flag (condition-proc))
	    (advance-pc pc)))
	(error "Bad Test instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts (lookup-label labels (label-exp-label dest))))
	  (lambda ()
	    (if (get-contents flag)
		(set-contents! pc insts)
		(advance-pc pc))))
	(error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts (lookup-label labels (label-exp-label dest))))
	     (lambda ()
	       (set-contents! pc insts))))
	  ((register-exp? dest)
	   (let ((reg (get-register machine (register-exp-reg dest))))
	     (lambda () (set-contents! pc (get-contents reg)))))
	  (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (let ((stack (get-stack machine (get-name reg))))
      (lambda ()
       (push stack (get-contents reg))
       (advance-pc pc)))))

(define (make-restore inst machine pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (let ((stack (get-stack machine (get-name reg))))
      (lambda ()
	(set-contents! reg (pop stack))
	(advance-pc pc)))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
	(let ((action-proc
	       (make-operation-exp
		action machine labels operations)))
	  (lambda ()
	    (action-proc)
	    (advance-pc pc)))
	(error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform inst)
  (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda () c)))
	((label-exp? exp)
	 (let ((insts
		(lookup-label labels
			      (label-exp-label exp))))
	   (lambda () insts)))
	((register-exp? exp)
	 (let ((r (get-register machine (register-exp-reg exp))))
	   (lambda () (get-contents r))))
	(else
	 (error "Unknown expression type -- ASSEMBLE" exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs
	 (map (lambda (e)
		(make-primitive-exp e machine labels))
	      (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operation)
  (let ((val (assoc symbol operation)))
    (if val
	(cadr val)
	(error "Unknown operation -- ASSEMBLE" symbol))))

;; tests
(define (assert condition)
  (if (not condition)
      (error "Assertion failed")
      'ok))

(define expt-machine
  (make-machine
   '(n b val continue)
   (list (list '= =) (list '- -) (list '* *))
   '(expt
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))

(define (expt b n)
  (set-register-contents! expt-machine 'b b)
  (set-register-contents! expt-machine 'n n)
  (start expt-machine)
  (get-register-contents expt-machine 'val))

(assert (eq? (expt 10 2) 100))
(assert (eq? (expt 10 3) 1000))
