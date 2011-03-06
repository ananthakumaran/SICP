;; *Exercise 5.21:* Implement register machines for the following
;; procedures.  Assume that the list-structure memory operations are
;; available as machine primitives.

;; a. Recursive `count-leaves':

;; (define (count-leaves tree)
;;   (cond ((null? tree) 0)
;; 	((not (pair? tree)) 1)
;; 	(else (+ (count-leaves (car tree))
;; 		 (count-leaves (cdr tree))))))

(define count-leaves-machine
  (make-machine
   (list (list 'car car) (list 'cdr cdr) (list '= eq?)
	 (list 'pair? pair?) (list '+ +))
   '(count-leaves
     (assign count (const 0))
     (assign continue (label count-leaves-done))
     count-loop
     (test (op =) (reg tree) (const ()))
     (branch (label expt-zero))
     (test (op pair?) (reg tree))
     (branch (label add-car-leaves))
     (assign count (const 1))
     (goto (reg continue))
     expt-zero
     (assign count (const 0))
     (goto (reg continue))
     add-car-leaves
     (save continue)
     (save tree)
     (assign continue (label add-cdr-leaves))
     (assign tree (op car) (reg tree))
     (goto (label count-loop))
     add-cdr-leaves
     (restore tree)
     (save count)
     (assign continue (label add-leaves))
     (assign tree (op cdr) (reg tree))
     (goto (label count-loop))
     add-leaves
     (assign t (reg count)) ;; cdr count
     (restore count)
     (restore continue)
     (assign count (op +) (reg t) (reg count))
     (goto (reg continue))
     count-leaves-done)))

(define (count-leaves tree)
  (set-register-contents! count-leaves-machine 'tree tree)
  (start count-leaves-machine)
  (get-register-contents count-leaves-machine 'count))

(count-leaves '((1 . (2 . 3)) . (4 . (5 . 6))))

;; b. Recursive `count-leaves' with explicit counter:

;; (define (count-leaves tree)
;;   (define (count-iter tree n)
;;     (cond ((null? tree) n)
;; 	  ((not (pair? tree)) (+ n 1))
;; 	  (else (count-iter (cdr tree)
;; 			    (count-iter (car tree) n)))))
;;   (count-iter tree 0))


(define count-leaves-machine
  (make-machine
   (list (list 'car car) (list 'cdr cdr) (list '= eq?)
	 (list 'pair? pair?) (list '+ +) (list 'print display))
   '(count-leaves
     (assign n (const 0))
     (assign continue (label count-leaves-done))
     count-loop
     (test (op =) (reg tree) (const ()))
     (branch (label continue))
     (test (op pair?) (reg tree))
     (branch (label count-iter))
     (assign n (op +) (reg n) (const 1))
     (goto (reg continue))
     count-iter
     (save continue)
     (save tree)
     (assign tree (op car) (reg tree))
     (assign continue (label count-iter-after))
     (goto (label count-loop))
     count-iter-after
     (restore tree)
     (assign continue (label count-iter-result))
     (assign tree (op cdr) (reg tree))
     (goto (label count-loop))
     count-iter-result
     (restore continue)
     (goto (reg continue))
     continue
     (goto (reg continue))
     count-leaves-done)))

(define (count-leaves tree)
  (set-register-contents! count-leaves-machine 'tree tree)
  (start count-leaves-machine)
  (get-register-contents count-leaves-machine 'n))

(count-leaves '(1 . (2 . 3)))
