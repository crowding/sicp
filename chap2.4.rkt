#lang planet neil/sicp

;;; This is the first problem set that asks for you to assume the
;;; presence of significant components (rather than building them up.)
;;; So you can't actually run the code for these solutions....

;;; But I have no way to check if my notions are on track unless I
;;; actually go ahead and write a 'get' and 'put,' as a simple alist
;;; type thing. So I will do that.

(define (update table key value)
  (cond ((null? table)
         (cons (cons key value) nil))
        ((equal? (caar table) key)
         (cons (cons (caar table) value)
               (cdr table)))
        (else (cons (car table) (update (cdr table) key value)))))

(define (lookup table key)
  (cond ((null? table)
         nil)
        ((equal? (caar table) key)
         (cdar table))
        (else
         (lookup (cdr table) key))))

(define type-tables '())

(define (get table key)
  (lookup (lookup type-tables table) key))

(define (put table key value)
  (set! type-tables
        (update type-tables table
                (update (lookup type-tables table) key value))))


;;; Exercise 2.73

;; A. For operators, we look up the operator in a table and call its
;; generic 'deriv method. There are an inifnite number of distinct
;; values for which (number?) or (variable?) is true, so numbers and
;; variables cannot be looked up in a table.

;; B.

;; One thing I nearned here is to use a prefix to distinguish the
;; function in hte package from the generic.

(define (make-sum . terms)
  (apply (get 'make '(+)) terms))

(define (make-product . terms)
  (apply (get 'make '(*)) terms))

(define (install-sums-package)
  (define (my-make . terms)
    (cond ((= (length terms) 0) 0)
          ((= (length terms) 1) (car terms))
          (else (attach-tag '+ terms))))
  (put 'make '(+) my-make)
  
  (define (my-deriv terms var)
    (apply make-sum
           (map (lambda (term) (deriv term var)) terms)))
  (put 'deriv '(+) my-deriv)
  
  'done)

(define (install-products-package)

  (define (my-make . terms)
    (cond ((= (length terms) 0) 1)
          ((= (length terms) 1) (car terms))
          ('t (attach-tag '* terms))))
  (put 'make '(*) my-make)
  
  (define (my-deriv terms var)
    (make-sum (make-product (deriv (car terms) var)
                            (apply make-product (cdr terms)))
              (make-product (car terms)
                            (deriv (apply make-product (cdr terms)) var))))
  (put 'deriv '(*) my-deriv)
  
  'done)

(install-sums-package)
(install-products-package)

;; C.
(define (make-exp base exp)
  ((get 'make '(**)) base exp))

(define (install-exp-package)
  (define (my-make base exp)
    (cond ((=number? exp 1) base)
          ((=number? exp 0) 1)
          (else (list ('** base exp)))))
  (put 'make '(**) my-make)
  
  (define (my-deriv basexp var)
    (let ((base (car basexp))
          (exp (cadr basexp)))
      (make-product exp (make-exp base (-1 exp)) ;nb. var in exponent not supported
       (deriv (base exp) var))))
  (put 'deriv '(**) my-deriv))

(install-exp-package)

;; D.
;;
;; This is just transposing the tables -- the arguments to "put" would
;; have to be transposed to correspond. Alternately, this is
;; message-passing as described below, but with tables instead of
;; closures.


;;; Exercise 2.74

;; Before we begin, let's go ahead and mock up two department data
;; sets with different formats.

(define engineering
  '(engineering
    ((name . "Jim") (salary . 12345))
    ((salary . 21345) (name . "Betty"))))

(define hr
  '(hr
    ("Edgar" 31338) ("Walter" 23877) ("Samantha" 23234)))

;;

;; Now here's an interface to accounts payable:

(define (install-engineering-department)
  
  (define (my-get-field rec field)
    (cond ((null? rec) nil)
          ((equal? (caar rec) field) (cdar rec))
          (else (my-get-field (cdr rec) field))))
  
  (define (my-get-salary rec)
    (my-get-field rec 'salary))
  (put 'get-salary '(engineering-record) my-get-salary)

  (define (my-get-record data name)
    (cond ((null? data) nil)
          ((equal? (my-get-field (car data) 'name) name)
           (attach-tag 'engineering-record (car data)))
          (else (my-get-record (cdr data) name))))
  (put 'get-record '(engineering) my-get-record)

  'done)
(install-engineering-department)

;; and an interface for HR:

(define (install-hr-department)
  (define (my-get-record data name)
    (cond ((null? data) nil)
          ((equal? (caar data) name)
           (attach-tag 'hr-record (car data)))
          (else (my-get-record (cdr data) name))))
  (put 'get-record '(hr) my-get-record)

  (define my-get-salary cadr)
  (put 'get-salary '(hr) my-get-salary)
  
  'done)
(install-hr-department)

;; A.

(define (get-record file name)
  ((get 'get-record (list (type-tag file))) (contents file) name))
;; In this case the "file" should be type-tagged with its <department>
;; name, as illustrated above.  The return value should be type-tagged
;; with <department>-record.  Methods for get-record and get-salary
;; need to be made for each <department>-file type.

;; B.
(define (get-salary record)
  ((get 'get-salary (list (type-tag record))) (contents record)))
;; As stated above the record will be type-tagged with <department>-record,
;; and there will be get-salary methods for each record type.

;; C.
;; e.g. (find-employee-record (list hr engineering) "Betty")
(define (find-employee-record files name)
  (if (null? files) '()
      (let ((rec (get-record (car files) name)))
        (if (null? rec)
            (find-employee-record (cdr files) name)
            rec))))

;; D.

;; Interfacing to a new department requires a new package with
;; 'get-record and get-salary methods be written, and a type tag
;; attached to the department's file.


;;; Exercise 2.75.

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;; Exercise 2.76.

;; Message-passing style facilitates the addition of new types (since
;; everything you need to write for a new type is in one place.) You
;; write a closure for each type (or, using tables, write the
;; "install" gathering all methods for a type together.)
;; Using closures, it also effectively "hides" data.
;;
;; Data-directed style facilitates addition of new operations (since
;; you can put similar operations for different types together.)
;; Tables can be used for both row-wise and col-wise organization.

;;; pasted from book or earlier HWs:

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (write "type-tags: ")
    (write type-tags)
    (write "\n")
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (list (car exp)))
(define (operands exp) (cdr exp))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

;;; constructors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
