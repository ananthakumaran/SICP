;; *Exercise 3.22:* Instead of representing a queue as a pair of
;; pointers, we can build a queue as a procedure with local state.
;; The local state will consist of pointers to the beginning and the
;; end of an ordinary list.  Thus, the `make-queue' procedure will
;; have the form

;; (define (make-queue)
;;   (let ((front-ptr ... )
;; 	(rear-ptr ... ))
;;     <DEFINITIONS OF INTERNAL PROCEDURES>
;;     (define (dispatch m) ...)
;;     dispatch))

;; Complete the definition of `make-queue' and provide
;; implementations of the queue operations using this representation.


(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (if (empty?)
	  (error "FRONT called on an empty queue")
	  (car front-ptr)))
    (define (insert! item)
      (let ((new-pair (cons item '())))
	(cond ((empty?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))
    (define (delete!)
      (cond ((empty?)
	     (error "DELETE! called on an empty queue"))
	    (else
	     (set! front-ptr (cdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'front) front)
	    ((eq? m 'insert!) insert!)
	    ((eq? m 'delete!) delete!)
	    ((eq? m 'print) front-ptr)))
    dispatch))


;; test	    
(define q1 (make-queue))
((q1 'insert!) 'a)
(q1 'print)
;;(a)
((q1 'insert!) 'b)
(q1 'print)
;;(a b)
((q1 'delete!))
(q1 'print)
;;(b)
((q1 'delete!))
(q1 'print)
;;()
((q1 'delete!))
;;DELETE! called on an empty queue