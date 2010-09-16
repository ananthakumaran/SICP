;; *Exercise 3.30:* *Note Figure 3-27:: shows a "ripple-carry adder"
;; formed by stringing together n full-adders.  This is the simplest
;; form of parallel adder for adding two n-bit binary numbers.  The
;; inputs A_1, A_2, A_3, ..., A_n and B_1, B_2, B_3, ..., B_n are the
;; two binary numbers to be added (each A_k and B_k is a 0 or a 1).
;; The circuit generates S_1, S_2, S_3, ..., S_n, the n bits of the
;; sum, and C, the carry from the addition.  Write a procedure
;; `ripple-carry-adder' that generates this circuit.  The procedure
;; should take as arguments three lists of n wires each--the A_k, the
;; B_k, and the S_k--and also another wire C.  The major drawback of
;; the ripple-carry adder is the need to wait for the carry signals
;; to propagate.  What is the delay needed to obtain the complete
;; output from an n-bit ripple-carry adder, expressed in terms of the
;; delays for and-gates, or-gates, and inverters?

;; *Figure 3.27:* A ripple-carry adder for n-bit numbers.

;;        :                                              :   :
;;        : A_1 B_1   C_1   A_2 B_2   C_2   A_3 B_3   C_3:   : A_n B_n C_n=0
;;        :  |   |   +---+   |   |   +---+   |   |   +-----  :  |   |   +-
;;        |  |   |   |   |   |   |   |   |   |   |   |   :   :  |   |   |
;;        : ++---+---++  |  ++---+---++  |  ++---+---++  :   : ++---+---++
;;        : |   FA    |  |  |   FA    |  |  |   FA    |  :   : |   FA    |
;;        : +--+---+--+  |  +--+---+--+  |  +--+---+--+  :   : +--+---+--+
;;        :    |   |     |     |   |     |     |   |     :   :    |   |
;;     C ------+   |     +-----+   |     +-----+   |     :  ------+   |
;;        :        |       C_1     |       C_2     |     :   :C_(n-1) |
;;        :        |               |               |     :   :        |
;;                S_1             S_2             S_3                S_n

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (zero-wire)
  (let ((z (make-wire)))
    (set-signal! z 0)
    z))

(define (ripple-carry-adder a b s c-out)
  (define (loop a b s c)
    (let ((tmp-c (make-wire)))
      (if (null? (cdr a))
	  (full-adder (car a) (car b) c (car s) c-out)
	  (begin
	    (full-adder (car a) (car b) c (car s) tmp-c)
	    (loop (cdr a) (cdr b) (cdr s) tmp-c)))))
  (loop (reverse a) (reverse b) (reverse s) (zero-wire)))

