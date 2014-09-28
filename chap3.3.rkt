#lang planet neil/sicp


;;; Exercise 3.13

;; On paper.



;;; Exercise 3.14

;; "Mystery" reverses a list "in place." After running

;; (define v (list 'a 'b 'c 'd))
;; (define w (mystery v))

;; we would have

;; > v
;; (a)
;; > w
;; (d c b a)

;; Note that "v" points to the same element (which is moved from the
;; first to the last in the list.)



;;; Exercise 3.15

;; On paper.



;;; Exercise 3.16

;; More paper.

(define (bad-count-pairs obj)
  (if (pair? obj)
      (+ (bad-count-pairs (car obj))
         (bad-count-pairs (cdr obj))
         1)
      0))

;;; e.g. (demo-17 bad-count-pairs)
(define (demo-17 counter)
  (define a (cons 1 (cons 2 (cons 3 nil)))) ;;3
  (define b ;;4
    (let* ((a (cons 2 nil))
           (b (cons a a))
           (c (cons 1 b)))
      c))
  (define c ;; 7
    (let* ((a (cons 1 nil))
           (b (cons a a))
           (c (cons b b)))
      c))
  (define d ;; Inf
    (let ((a (cons 1 (cons 2 (cons 3 nil)))))
      (set-cdr! (cddr a) a)
      a))
  (write a) (display ": ") (write (counter a)) (display "\n")
  (write b) (display ": ") (write (counter b)) (display "\n")
  (write c) (display ": ") (write (counter c)) (display "\n")
  (write d) (display ": ") (write (counter d)) (display "\n"))

;;; Exercise 3.17

;; We could keep a list of things we've visited, or, more efficiently,
;; replace each car/cdr with "visited" remembering the structure on
;; the stack. Either way we need some memory.

;; I'll keep values in a list, even though it's expensive searching the list.

(define (member-eq? obj list)
  (if (null? list) #f
      (if (eq? obj (car list)) #t
          (member-eq? obj (cdr list)))))

(define (count-pairs obj)
  (let ((count 0)
        (seen '()))
    (define (iter obj)
      (cond ((and (pair? obj)
                  (not (member-eq? obj seen)))
             (set! seen (cons obj seen))
             (set! count (+ 1 count))
             (iter (car obj))
             (iter (cdr obj)))))
    (iter obj)
    count))

;;; Exercise 3.18

;; Note that this is for arbitrary structures not just lists.

(define (contains-cycle? obj)
  (let ((seen '()))
    (define (iter obj)
      (if (pair? obj)
          (if (member-eq? obj seen)
              #t
              (begin
                (set! seen (cons obj seen))
                (iter (car obj))
                (iter (cdr obj))))
          #f))
    (iter obj)))
;; e.g. (contains-cycle? z)


;;; Exercise 3.19

;; Doing this for a list is a well-known interview problem, of course;
;; one needs to advance the "head" twice, tail once, check for eq?.
;; Once both head and tail are in the loop, they will land on the same
;; object sooner or later. Instead of lists, I'll do it for arbitrary
;; structures. I'm using the tree-iteration technique of chapter
;; 1.something and obstinately not using "set".

(define (pair-visitor obj next)
  (define (dispatch cmd)
    (case cmd
      ((get) obj)
      ((finished?) (not (or (pair? obj) (pair? next))))
      ((next)
       (if (pair? obj) ;; always put pairs in the obj slot...
           (pair-visitor (car obj) (cons (cdr obj) next))
           (if (pair? next)
               (pair-visitor (car next) (cdr next))
               (pair-visitor next nil))))
      (else (cmd obj))))

  (if (and (not (pair? obj)) (pair? next)) ;; skip ahead to when obj is a pair
      (pair-visitor (car next) (cdr next))
      dispatch))

(define (contains-cycle-small? obj)
  (define (iter head tail)
    (if (head pair?)
        (if (eq? (head 'get) (tail 'get))
            #t
            (iter ((head 'next) 'next) (tail 'next)))
        #f))

  (iter ((pair-visitor obj nil) 'next)
        (pair-visitor obj nil)))



;;; Exercise 3.20

;; On paper.

;; The relations among the dispatch functions and their environment
;; variables "car" and "cdr" mirror 3.12's relations among pairs and
;; their car and cdr slots.


;;; Exercise 3.21

;; On paper I drew a boxes-and-arrows tracing of the four calls.

;; In the case where head-ptr points to nil and tail-ptr still has a
;; object, the next call to insert-queue will reset it, so the
;; dangling value ultimately has no effect. However it does hang on to
;; the reference.

;; The fact that the last element is printed twice reflects that the
;; Lisp printer will go down both "car" and "cdr" of a pair even if
;; both ultimately go to the same place. "b" is the element following
;; "a" and it is also the element at the end of the queue; the data
;; structure represents both facts.

;; A visual representation of the queue would more simply be just to
;; print its head pointer.

(define (print-queue q)
  (display "queue:")
  (write (front-ptr q))
  (display "\n"))

;;; Exercise 3.22

(define (make-queue-obj)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?) (null? front-ptr))
    (define (front) (if (empty?)
                        (error "FRONT called with an empty queue")
                        (car front-ptr)))
    (define (insert item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (delete)
      (cond ((empty?)
             (error "DELETE! called with an empty queue"))
            (else (set! front-ptr (cdr front-ptr)))))
    (define (print)
      (display "queue:")
      (write front-ptr)
      (display "\n"))
    (define (dispatch m)
      (case m
        ((empty?) empty?)
        ((front) front)
        ((insert) insert)
        ((delete) delete)))
    dispatch))


;;; Exercise 3.23

;; A deque can be implemented by using a doubly linked list, where
;; each node is two pairs, one pointing forward and onw pointing
;; backward.

;; this is a lot of renaming 'car' 'cdr' 'set-car' and so on.

(define (make-node prev item next)
  (cons (cons prev item) next))

(define (node-next n) (cdr n))
(define (node-prev n) (caar n))
(define (node-item n) (cdar n))

(define (node-set-prev! n p) (set-car! (car n) p))
(define (node-set-next! n x) (set-cdr! n x))

(define (make-deque) (cons nil nil))
(define (deque-front-node d) (car d))
(define (deque-rear-node d) (cdr d))
(define (deque-set-front! d n) (set-car! d n))
(define (deque-set-rear! d n) (set-cdr! d n))


(define (empty-deque? d)
  (or (null? (deque-front-node d))
      (null? (deque-rear-node d))))

(define (front-deque d)
  (if (empty-deque? d)
      (error "FRONT-DEQUE called with empty deque" d)
      (node-item (deque-front-node d))))

(define (rear-deque d)
  (if (empty-deque? d)
      (error "REAR-DEQUE called with empty deque" d)
      (node-item (deque-rear-node d))))

(define (front-insert-deque! d i)
  (let ((new-node (make-node nil i (deque-front-node d))))
    (if (empty-deque? d)
        (deque-set-rear! d new-node)
        (node-set-prev! (deque-front-node d) new-node))
    (deque-set-front! d new-node)))

(define (rear-insert-deque! d i)
  (let ((new-node (make-node (deque-rear-node d) i nil)))
    (if (empty-deque? d)
        (deque-set-front! d new-node)
        (node-set-next! (deque-rear-node d) new-node))
    (deque-set-rear! d new-node)))

(define (front-delete-deque! d)
  (if (empty-deque? d)
      (error "FRONT-DELETE-DEQUE! called with empty deque")
      (begin (deque-set-front! d (node-next (deque-front-node d)))
             (if (empty-deque? d)
                 (deque-set-rear! d nil)
                 (node-set-prev! (deque-front-node d) nil)))))

(define (rear-delete-deque! d)
  (if (empty-deque? d)
      (error "REAR-DELETE-DEQUE! called with empty deque")
      (begin (deque-set-rear! d (node-prev (deque-rear-node d)))
             (if (empty-deque? d)
                 (deque-set-front! d nil)
                 (node-set-next! (deque-rear-node d) nil)))))

(define (print-deque D)
  (define (iter-rear node list)
    (if (null? node) list
        (iter-rear (node-prev node) (cons (node-item node) list))))
  (define (iter-front node list)
    (if (null? node) list
        (iter-front (node-next node) (cons (node-item node) list))))
  (write (iter-rear (deque-rear-node D) nil)) (display "\n")
  (write (reverse (iter-front (deque-front-node D) nil))) (display "\n"))

;;; Exercise 3.24

;; Just have a local deifnition of assoc that uses the provided
;; comparison.

(define (make-table-eq equality?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equality? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;;; Exercise 3.25

;; Tables within tables within tables. I will use "nil" as a key to store the
;; actual values (that way, you can store under '(foo) as well as '(foo bar).)

(define (make-table) (list '*table*))

(define (lookup keylist table)
  (cond ((null? table) false)
        ((null? keylist) (assoc nil (cdr table)))
        ((pair? keylist)
         (lookup (cdr keylist) (assoc (car keylist) (cdr table))))
        (else (error "keylist must be a list: INSERT" keylist))))

(define (insert! keylist value table)
  (cond ((null? table) (error "Table must not be null"))
        ((null? keylist)
         (let ((record (assoc nil (cdr table))))
           (if record
               (set-cdr! record value)
               (set-cdr! table
                         (cons (cons nil value)
                               (cdr table))))))
        ((pair? keylist)
         (let ((subtable (assoc (car keylist) (cdr table))))
           (if subtable
               (insert! (cdr keylist) value (cdr subtable))
               (let ((new-table (make-table)))
                 (set-cdr! table
                           (cons (cons (car keylist)
                                       new-table)
                                 (cdr table)))
                 (insert! (cdr keylist) value new-table)))))
        (else (error "keylist must be a list: INSERT" keylist)))
  'ok)

;;; Exercise 3.26

;; This is much like the tree exercises from chapter 2. In this case
;; "entries" are structured as key/value pairs, instead of just
;; values, and are sorted on the key.

;; The procedure for inserting an element becomes to first do the same
;; as lookup; if lookup stops on a matching key, update the
;; entry with the new value. If not, attach a new node with the new entry.

;; The tricky part is keeping the tree balanced, which this text has
;; not discussed.



;;; Exercise 3.27

;; Computing Fibbonacci coefficients in O(n) time depends on insert!
;; inserting new values at the beginning of the list, rather than the
;; end. Then each lookup takes at most 2 steps. (until fib is called
;; with a smaller starting value...)
;;
;; Defining (memo-fib) to be (memoize fib) would not work because
;; "fib" will not call memo-fib when it recurses.



                                        ;                     DEBUGGING/UTIL
(define (show . args)
  ;; write a debugging message.
  (cond ((null? args)
         (display "\n"))
        ((string? (car args))
         (display (car args))
         (display " ")
         (apply show (cdr args)))
        (else
         (write (car args))
         (display " ")
         (apply show (cdr args)))))


                                        ;                    PASTES
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
