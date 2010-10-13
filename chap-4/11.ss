;; *Exercise 4.11:* Instead of representing a frame as a pair of
;; lists, we can represent a frame as a list of bindings, where each
;; binding is a name-value pair.  Rewrite the environment operations
;; to use this alternative representation.

(define (make-frame variables values)
  (map (lambda (x y) (cons x y)) variables values))

(define x (make-frame '(x y) '(1 2)))

(define (frame-variables frame) (map (lambda (x) (car x)) frame))
(define (frame-values frame) (map (lambda (x) (cdr x)) frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))
