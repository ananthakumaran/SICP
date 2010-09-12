;; *Exercise 3.23:* A "deque" ("double-ended queue") is a sequence in
;; which items can be inserted and deleted at either the front or the
;; rear.  Operations on deques are the constructor `make-deque', the
;; predicate `empty-deque?', selectors `front-deque' and
;; `rear-deque', and mutators `front-insert-deque!',
;; `rear-insert-deque!', `front-delete-deque!', and
;; `rear-delete-deque!'.  Show how to represent deques using pairs,
;; and give implementations of the operations.(2)  All operations
;; should be accomplished in [theta](1) steps.

(define (make-node next prev item)
  (list next prev item))
(define (item node) (caddr node))
(define (next node) (car node))
(define (prev node) (cadr node))
(define (set-next! node next) (set-car! node next))
(define (set-prev! node prev) (set-car! (cdr node) prev))


(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque node) (set-car! deque node))
(define (set-rear-ptr! deque node) (set-cdr! deque node))

(define (make-deque) (cons '() '()))
(define (empty-deque? deque) (null? (front-ptr deque)))


(define (front-deque deque) 
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (item (front-ptr deque))))

(define (rear-deque deque) 
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (item (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-node (make-node (front-ptr deque) '() item)))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-node)
	   (set-rear-ptr! deque new-node)
	   deque)
	  (else
	   (set-prev! (front-ptr deque) new-node)
	   (set-front-ptr! deque new-node)))))


(define (rear-insert-deque! deque item)
  (let ((new-node (make-node '() (rear-ptr deque) item)))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-node)
	   (set-rear-ptr! deque new-node)
	   deque)
	  (else
	   (set-next! (rear-ptr deque) new-node)
	   (set-rear-ptr! deque new-node)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	 (set-front-ptr! deque (next (front-ptr deque)))
	 (if (not (null? (front-ptr deque)))
	     (set-prev! (front-ptr deque) '())
	     (set-rear-ptr! deque '())))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	 (set-rear-ptr! deque (prev (rear-ptr deque)))
	 (if (not (null? (rear-ptr deque)))
	     (set-next! (rear-ptr deque) '())
	     (set-front-ptr! deque '())))))

