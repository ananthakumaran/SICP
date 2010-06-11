;;  Exercise 2.47. Here are two possible constructors for frames:

;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))
;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

;; For each constructor supply the appropriate selectors to produce an
;; implementation for frames.

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
;; type 1
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)


;; type 2
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

;; test
(define (make-vect x y)
  (cons x y))

(define a (make-vect 3 4))
(define b (make-vect 0 0))
(define c (make-vect 4 4))

(define frame (make-frame a b c))

(origin-frame frame)
(edge1-frame frame)
(edge2-frame frame)


