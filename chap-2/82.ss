;; Exercise 2.82. Show how to generalize apply-generic to handle coercion
;; in the general case of multiple arguments. One strategy is to attempt
;; to coerce all the arguments to the type of the first argument, then to
;; the type of the second argument, and so on. Give an example of a
;; situation where this strategy (and likewise the two-argument version
;; given above) is not sufficiently general. (Hint: Consider the case
;; where there are some suitable mixed-type operations present in the
;; table that will not be tried.)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (try-each-type untried-tags)
      (if (null? untried-tags)
	  (error "No method for these types"
		 (list op type-tags))
	  (let ((coerced-types (try-coercion type-tags '() (car untried-tags))))
	    (if coerced-types
		(apply-generic generic op coerced-types)
		(try-type (cdr untried-tags))))))
    (define (try-coercion uncoerced-list result target)
      (if (null? uncoerced-list)
	  result
	  ;; try to coerce the current type and return null
	  ;; if we cannot coerce a type to other type
	  (let ((coerced (get-coercion (car uncoerced-list) target)))
	    (if coerced
		(try-coercion (cdr uncoerced-list) (cons result coerced) target)
		'()))))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (try-each-type type-tags)))))
