#lang planet neil/sicp

;;; This is the first problem set that asks for you to assume the
;;; presence of significant components (rather than building them up.)
;;; So you can't actually run the code for these solutions....

;;; Exercise 2.73

;; A. For operators, we look up the operator in a table and call its
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
;;
;; This is just transposing the tables -- the arguments to "put" would
;; have to be transposed to correspond. Alternately, this is
;; message-passing as described below, but with tables instead of
;; closures.

;;; Exercise 2.74
;;
;; (not runnable because Set and get and set primitives not there
;; yet. "Suppose that" means I'm not being tasked with implementing
;; the complete thing.)

;; For each department, we will have written something like this:
(define (install-accounts-payable-department)
  (define (get-record data name)
    (let (record (get data name))
      (if (null? data) nil)
      (attach-tag 'accounts-payable-record rec)))
  (define (get-salary record)
    ;; do stuff to get the salary
    'salary)
  (put 'get-record '(accounts-payable-record) get-record)
  (put 'get-salary '(accounts-payable-department) get-salary)
  'done)

;; A.
(define (get-record file name)
  (get (type-tag file) 'get-record) (contents file) name)
;; In this case the "file" should be type-tagged with its
;; <department> name.  The return value will be type-tagged with
;; <department>-record.
;; Methods for get-record and get-salary need to be made for each
;; <department>-file type.

;; B.
(define (get-salary record)
  ((get (type-tag record) 'get-salary) (contents record)))
;; As stated above the record will be type-tagged with <department>-record,
;; and there will be get-salary methods for each record-type.

;; C.
(define (find-employee-record files name)
  (if (null? files) null
      (let ((rec (get-record (car files) name)))
        (if (null? rec) (find-employee-record (cdr files) name)
            (get-salary rec)))))


;; D.
;; Interfacing to a new department requires interfacing methods be
;; written for get-record and get-salary.

;;; Exercise 2.75.

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          ((else (error "Unknown op: MAKE-FROM-MAG-ANG" op)))))
  dispatch)

;;; Exercise 2.76.

;; Message-pasing style facilitates the addition of new types (since
;; everything you need to write for a new type is in one place.) You
;; write a closure for each type (or, using tables, write the
;; "install" gathering all methods for a type together.)
;; Data-directed style facilitates addition of new operations (since
;; you can put similar operations for different types together.)
;; Tables can be used for both, mind.

;;; pasted from book or earlier HWs:

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
