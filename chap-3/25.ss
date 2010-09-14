;; *Exercise 3.25:* Generalizing one- and two-dimzensional tables,
;; show how to implement a table in which values are stored under an
;; arbitrary number of keys and different values may be stored under
;; different numbers of keys.  The `lookup' and `insert!' procedures
;; should take as input a list of keys used to access the table.

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((equal? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))

    (define (lookup keys)
      (define (loop keys table)
	(if (null? keys)
	    false
	    (let ((record (assoc (car keys) (cdr table))))
	      (if record
		  ;; if this is the last key return the value
		  (if (null? (cdr keys))
		      (cdr record)
		      (loop (cdr keys) record))
		  false))))
    (loop keys local-table))

    (define (insert! keys value)
      (define (loop keys table)
	;; last key
	(if (null? (cdr keys))
	    (let ((record (assoc (car keys) (cdr table))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! table
			    (cons (cons (car keys) value)
				  (cdr table)))))
	    (let ((subtable (assoc (car keys) (cdr table))))
	      (if subtable
		  (loop (cdr keys) subtable)
		  (let ((new-table (cons (car keys) '())))
		    (set-cdr! table
			      (cons new-table
				    (cdr table)))
		    (loop (cdr keys) new-table))))))
      (loop keys local-table)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'table) local-table) ;; debug
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; one dimension
(define q (make-table))
((q 'lookup-proc) '(a))
((q 'insert-proc!) '(a) 'A)
(q 'table)
((q 'lookup-proc) '(a))
((q 'insert-proc!) '(a) 'b)
((q 'insert-proc!) '(b) 'b)
((q 'lookup-proc) '(a))
((q 'lookup-proc) '(b))
(q 'table)

;; two dimension
(define q (make-table))
((q 'lookup-proc) '(a b))
((q 'insert-proc!) '(math a) 'A)
(q 'table)
((q 'lookup-proc) '(math a))
((q 'insert-proc!) '(math a) 'b)
((q 'insert-proc!) '(math b) 'b)
((q 'lookup-proc) '(math a))
((q 'lookup-proc) '(math b))
(q 'table)

;; three dimension
(define q (make-table))
((q 'lookup-proc) '(a b c))
((q 'insert-proc!) '(math new a) 'A)
(q 'table)
((q 'lookup-proc) '(math new a))
((q 'insert-proc!) '(math old a) 'A)
((q 'insert-proc!) '(old math a) 'A)
((q 'lookup-proc) '(math old a))
((q 'insert-proc!) '(old math a) 'b)
((q 'lookup-proc) '(old math a))
(q 'table)
