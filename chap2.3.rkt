#lang planet neil/sicp

;;; Exercise 2.53
;> (list 'a 'b 'c)
;(a b c)
;> (list (list 'george))
;((george))
;> (cdr '((x1 x2) (y1 y2)))
;((y1 y2))
;> (cadr '((x1 x2) (y1 y2)))
;(y1 y2)
;> (pair? (car '(a short list)))
;#f
;> (memq 'red '((red shoes) (blue socks)))
;#f
;> (memq 'red '(red shoes blue socks))
;(red shoes blue socks)

;;; Exercise 2.54
(define (equal? a b)
  (cond ((and (pair? a) (pair? b)) 
         (if (eq? (car a) (car b))
             (equal? (cdr a) (cdr b))
             #f))
        ((pair? a) #f)
        ((pair? b) #f)
        (else (eq? a b))))

;;; Exercise 2.55

; 'x is equivalent to (quote x) as explined in a footnote.
; thus ''x is (quote (quote x)) which evaluates to (quote x)

;;; Exercise 2.56

(define (make-exponent base exponent)
  (cond ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        (else (list '** base exponent))))

(define (exponent? x) (and (pair? x) (eq? (car x) '**)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))
  
(define (deriv-exponent exp var)
  ;; NB. this does not handle when x is in the exponent as in e^x
  (make-product 
          (make-product (exponent exp)
                        (make-exponent (base exp) 
                                       (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))

;;; Exercise 2.57

;; possibly overcomplicated on simplifying...
; e.g. '(+ a b c (+ d (* e (+ f g))) (+ e f))
; => '(+ a b c d (* e (+ f g)) e f)
(define (merge-cdrs-when test? seq)
  (if (pair? seq) 
      (if (test? (car seq))
          (append (merge-cdrs-when test? (cdar seq))
                  (merge-cdrs-when test? (cdr seq)))
          (cons (car seq) (merge-cdrs-when test? (cdr seq))))
      nil))

(define (make-sum . terms)
  (define (make-sum-list terms)
    (cond ((= (length terms) 0) nil)
          ((=number? (car terms) 0) 
           (make-sum-list (cdr terms)))
          ((= (length terms) 1) (list (car terms)))
          ((and (number? (car terms)) (number? (cadr terms)))
           (make-sum-list (cons (+ (car terms) (cadr terms)) (cddr terms))))
          (else (cons (car terms) (make-sum-list (cdr terms))))))
  (let ((terms (make-sum-list (merge-cdrs-when sum? terms))))
    (cond ((= (length terms) 0) 0)
          ((= (length terms) 1) (car terms))
          (else (cons '+ terms)))))

(define (augend s) 
  (if (> (length (cdr s)) 1)
      (apply make-sum (cddr s))
      (caddr s)))

(define (ncdr x) (if (null? x) nil (cdr x)))
(define (ncar x) (if (null? x) nil (car x)))

(define (make-product . terms)
  (define (make-product-list first second rest)
    (cond ((null? first) nil)
          ((=number? first 1) (make-product-list second (ncar rest) (ncdr rest)))
          ((=number? first 0) (list 0))
          ((and (number? first) (number? second))
           (make-product-list (* first second) (ncar rest) (ncdr rest)))
          (else 
           (cons first (make-product-list second (ncar rest) (ncdr rest))))))
  (let* ((mterms (merge-cdrs-when product? terms))
         (tlist (make-product-list 
                 (ncar mterms) (ncar (ncdr mterms)) (ncdr (ncdr mterms)))))
    (cond ((= (length tlist) 0) 1)
          ((= (length tlist) 1) (car tlist))
          (else (cons '* tlist)))))
  
(define (multiplicand s)
  (if (> (length (cdr s)) 1)
      (apply make-product (cddr s))
      (caddr s)))

;;; Exercise 2.58

;a can be done with different selectors
(define (make-infix-product x y) (list x '* y))
(define (infix-product? x)
  (and (pair? x) (>= (length x) 2) (eq? (cadr x) '*)))
(define infix-multiplier car)
(define infix-multiplicand caddr)
  
(define (make-infix-sum x y) (list x '+ y))
(define (infix-sum? x)
  (and (pair? x) (>= (length x) 2) (eq? (cadr x) '+)))
(define infix-addend car)
(define infix-augend caddr)

;Part b: see file "chap2.3.58b.rkt"

;;; derivative from book with extracted methods

(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (deriv-product exp var)
  (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (deriv-sum exp var))
        ((product? exp) (deriv-product exp var))
        ((exponent? exp) (deriv-exponent exp var))
        (else
         (error "unknown expression type: DERIV" exp))))

;;; pasted from book

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))