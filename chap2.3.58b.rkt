#lang planet neil/sicp

;;; solution for Exercise 2.59b.
;;; We classify infix expressions based on the lowest 
;;; precedence operator they have at root level. 
;;; Thus '(3 * (4 + 5)) is a sum, '(5 * 2 ^ 3) is a product, 
;;; '(3 * (4 + 5)) is a product, '((2 + 3) ^ (3 * 4)) is an exponent,
;;; etc.
;;; All operators evaluate left to right.

(define precedence '(+ * ^))

(define (index-of-eq list op) 
  (define (iter list count)
    (cond ((null? list) count)
          ((eq? op (car list)) count)
          (else (iter (cdr list) (inc count)))))
  (iter list 0))

(define (precedence-of op) (index-of-eq precedence op))

(define (lowest-precedence-operator exp)
  (if (pair? exp)
      (fold-left (lambda (x y)
                   (if (< (precedence-of x) (precedence-of y)) x y))
                 nil exp)
      nil))

(define (sum? exp) (eq? (lowest-precedence-operator exp) '+))
(define (product? exp) (eq? (lowest-precedence-operator exp) '*))
(define (exponent? exp) (eq? (lowest-precedence-operator exp) '^))

(define (list-head list n)
  (define (iter list n accum)
    (if (<= n 0) accum (iter (cdr list) (dec n) (cons (car list) accum))))
  (reverse (iter list n nil)))

(define (left op)
  (lambda (exp)
    (let ((ix (index-of-eq exp op)))
      (list-head exp ix))))

(define (right op)
  (lambda (exp) (list-tail exp (inc (index-of-eq exp op)))))

(define addend (left '+))
(define augend (right '+))
(define multiplier (left '*))
(define multiplicand (right '*))
(define base (left '^))
(define exponent (right '^))

; wrap parens if non-list, or if lower precedence than given op:
(define (wrap-if-precedence exp cmp op)
  (cond ((not (pair? exp)) (list exp))
        ((cmp (precedence-of (lowest-precedence-operator exp))
              (precedence-of op))
         (list exp))
        (else exp)))

(define (lnumber? what) ;test singleton list whose head is a number...
  (and (pair? what) (= (length what) 1) (number? (car what))))
(define (l= what to) ;test singleton list whose head is numerically eq? to...
  (and (pair? what) 
       (= (length what) 1) 
       (number? (car what))
       (= (car what) to)))

;generic operator-maker strips or adds parens when appropriate,
;as well as collapses identities, absorptions and simple arithmetic.
(define (make-op op combiner identity absorbing)
  (lambda (left right)
    (let ((wleft (wrap-if-precedence left < op))
          (wright (wrap-if-precedence right < op)))
      (cond ((and (lnumber? wleft) (lnumber? wright))
             (combiner (car wleft) (car wright)))
            ((l= wleft identity) right)
            ((l= wright identity) left)
            ((l= wright absorbing) absorbing)
            ((l= wleft absorbing) absorbing)
            (else (append wleft (list op) wright))))))

(define make-product (make-op  '* * 1 0))
(define make-sum (make-op      '+ + 0 +nan.0))

(define (make-exponent left right)
  (let ((wleft (wrap-if-precedence left < '^))
        (wright (wrap-if-precedence right <= '^))) ; a^(b^c) != a^b^c
    (cond ((and (lnumber? wleft) (lnumber? wright))
           (expt (car wleft) (car wright)))
          ((l= wright 0) 1)
          ((l= wleft 0) 0)
          ((l= wleft 1) 1)
          ((l= wright 1) left)
          (else (append wleft (list '^) wright)))))

;;; pasted from book / earlier exercises

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (deriv-product exp var)
  (make-sum
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))

(define (deriv-exponent exp var)
  ;; NB. this does not handle when x is in the exponent as in e^x
  (make-product 
   (make-product (exponent exp)
                 (make-exponent (base exp) 
                                (make-sum (exponent exp) -1)))
   (deriv (base exp) var)))

; as (deriv '(x) 'x) needs to mean something...
(define (singleton? x) (and (pair? x) (= (length x) 1)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (deriv-sum exp var))
        ((product? exp) (deriv-product exp var))
        ((exponent? exp) (deriv-exponent exp var))
        ((singleton? exp) (deriv (car exp) var))
        (else
         (error "unknown expression type: DERIV" exp))))
 
