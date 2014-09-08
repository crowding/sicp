#lang planet neil/sicp

;;; Exercise 2.73

;; A. For operators, we look up teh operator in a table and call its
;; generic 'deriv method. There are an inifnite number of distinct
;; values for which (number?) or (variable?) is true, so numbers and
;; variables cannot be looked up in a table.

;; B.

(define (make-sum . terms)
  (apply (get 'make '+) terms))

(define (make-product . terms)
  (apply (get 'make '*) terms))

(define (install-sums-package)
  (define (make-sum . terms)
    (cond ((= (length terms) 0) 0)
          ((= (length terms) 1) (car terms))
          (else (attach-tag '+ terms))))
  (define (deriv terms var)
    (make-sum (list (deriv (car terms) var)
                    (deriv (apply 'make-sum (cdr terms)) var))))
  (put 'make '(+) make-sum)
  (put 'deriv '(+) deriv)
  'done)

(define (install-products-package)
  (define (make-sum . terms)
    (cond ((= (length terms) 0) 1)
          ((= (length terms) 1) (car terms))
          ('t (attach-tag '* terms))))
  (put 'make '(*) make-sum)
  'done)

;; C.
(define (make-exp base exp)
  ((get 'make '**) base exp))

(define (install-exp-package)
  (define (make-exp base exp)
    (cond ((=number? exp 1) base)
          ((=number? exp 0) 1)
          (else (list ('** base exp)))))
  (define (deriv basexp var)
    (let ((base (car basexp))
          (exp (cdr basexp)))
      (make-product
       (make-product exp (make-exp base (make-sum exp -1)))
       (deriv (base exp) var))))
  (put 'deriv '(**) make-sum))

;; D.
; This is just transposing the tables -- the arguments to 'put would
; have to be transposed to correspond.


;;; pasted from book or earlier HWs

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (square x) (* x x))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

;;; type tagging
(define (attach-tag type-tag contents)
     (cons type-tag contents))
   (define (type-tag datum)
     (if (pair? datum)
(car datum)
         (error "Bad tagged datum: TYPE-TAG" datum)))
   (define (contents datum)
     (if (pair? datum)
         (cdr datum)
         (error "Bad tagged datum: CONTENTS" datum)))

;;; constructors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;; packaged rectangular complex
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
        (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
        (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
        (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
        (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (put op type item) 'nope)
(define (get op type item) 'nope)
