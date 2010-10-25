;; *Exercise 4.26:* Ben Bitdiddle and Alyssa P. Hacker disagree over
;; the importance of lazy evaluation for implementing things such as
;; `unless'.  Ben points out that it's possible to implement `unless'
;; in applicative order as a special form.  Alyssa counters that, if
;; one did that, `unless' would be merely syntax, not a procedure
;; that could be used in conjunction with higher-order procedures.
;; Fill in the details on both sides of the argument.  Show how to
;; implement `unless' as a derived expression (like `cond' or `let'),
;; and give an example of a situation where it might be useful to
;; have `unless' available as a procedure, rather than as a special
;; form.

(define (unless-predicate exp) (cadr exp))
(define (unless-consequent exp) (caddr exp))
(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (unless->if exp)
  (make-if (unless-predicate exp) (unless-alternate exp) (unless-consequent exp)))
