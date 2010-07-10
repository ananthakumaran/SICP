;; Exercise 2.70. The following eight-symbol alphabet with associated
;; relative frequencies was designed to efficiently encode the lyrics of
;; 1950s rock songs. (Note that the ``symbols'' of an ``alphabet'' need
;; not be individual letters.)

;;  A        2 NA     16
;;  BOOM     1 SHA    3
;;  GET      2 YIP    9
;;  JOB      2 WAH    1

;; Use generate-huffman-tree (exercise 2.69) to generate a corresponding
;; Huffman tree, and use encode (exercise 2.68) to encode the following
;; message:

;; Get a job
;; Sha na na na na na na na na
;; Get a job
;; Sha na na na na na na na na
;; Wah yip yip yip yip yip yip yip yip yip
;; Sha boom

;; How many bits are required for the encoding? What is the smallest
;; number of bits that would be needed to encode this song if we used a
;; fixed-length code for the eight-symbol alphabet?

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))


;; encode 
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	    (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
	    (right (right-branch tree)))
	(cond ((member symbol (symbols left)) (cons 0 (encode-symbol symbol left)))
	      ((member symbol (symbols right)) (cons 1 (encode-symbol symbol right)))
	      (else (error " Error symbol not found " symbol))))))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
       (adjoin-set (make-code-tree (car leaf-set)
				   (cadr leaf-set))
		   (cddr leaf-set)))))


(define h-tree (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))
(define message (encode '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom) h-tree))

;; Huffman code
(length message)
;; 84
;; Fixed-length code 
(* 3 (length '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom)))
;; 108
