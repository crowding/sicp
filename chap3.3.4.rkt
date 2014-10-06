#lang planet neil/sicp

;;; Exercise 3.28
(define (or-gate a1 a2 output)
  (assert-wire a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (fire-after-delay output or-gate-delay new-value)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))

(define (logical-and s1 s2)
  (cond ((= s1 1)
         (cond ((= s2 1) 1)
               ((= s2 0) 0)
               (else (error "Invalid signal" s2))))
        ((= s1 0)
         (cond ((= s2 1) 0)
               ((= s2 0) 0)
               (else (error "Invalid signal" s2))))
        (else (error "Invalid signal" s1))))

(define (logical-or s1 s2)
  (logical-not (logical-and (logical-not s1)
                            (logical-not s2))))

(define (demo-or or-gate)
  (set! the-agenda (make-agenda))
  (let
      ((a (make-wire))
       (b (make-wire))
       (o (make-wire)))
    (define (status) (show "-----" "a" (get-signal a)
                           "b" (get-signal b) "o" (get-signal o)))
    (probe 'o o)
    (probe 'a a)
    (probe 'b b)
    (or-gate a b o)
    (propagate) ;;;; WTF: this call to propagate seems to determine
                ;;;; whether output transitions pack to 0
    (status)
    (set-signal! a 1)
    (propagate)
    (status)
    (set-signal! b 1)
    (propagate)
    (status)
    (set-signal! a 0)
    (propagate)
    (status)
    (set-signal! b 0)
    (propagate)
    (status)))


;;; Exercise 3.29

;;; e.g. (demo-or composite-or-gate)

(define (composite-or-gate a1 a2 output)
  (assert-wire a1 a2 output)
  (let ((na1 (make-wire))
        (na2 (make-wire))
        (no (make-wire)))
    (inverter a1 na1)
    (inverter a2 na2)
    (and-gate na1 na2 no)
    (inverter no output)
    'ok))



;;; Exercise 3.30
(define and-gate-delay 3)
(define or-gate-delay 5)
(define inverter-delay 2)

;; Since we are given a wire for "carry-out", we must be building from
;; high bit to low bit.

(define (ripple-carry-adder Ak Bk Sk C)
  (apply assert-wire (append Ak Bk Sk (list C)))
  (cond ((null? Ak)                     ; carry-in gets 0
         (set-signal! C 0)
         'ok)
        ((or (null? Bk)
             (null? Sk))
         (error "lists were not the same length"))
        (else
         (let ((carry-in (make-wire)))
           (full-adder (car Ak) (car Bk) carry-in (car Sk) C)
           (ripple-carry-adder (cdr Ak) (cdr Bk) (cdr Sk) carry-in)))))

;; The delay depends on potentially taking the "carry" signal through
;; all the full adders. Looking at the half adder circuit, the worst
;; case is that B changes causing S and C to both change, and this
;; happens in all adders. This is the case when adding (2^n-1) to 1.
;;
;; Reading the diagrams of the adders, I define:
;;
(define half-adder-carry-delay and-gate-delay)
(define half-adder-sum-delay (+ (max or-gate-delay
                                     (+ and-gate-delay inverter-delay))
                                and-gate-delay))
;;
(define full-adder-sum-delay
  (* 2 half-adder-sum-delay)) ;;
;;
(define full-adder-carry-delay
  (max
   (+ half-adder-sum-delay half-adder-carry-delay or-gate-delay)
   (+ half-adder-carry-delay or-gate-delay)))
;;
;; Then:
;;
;; ripple-adder-delay = N-1 * full-adder-carry-delay + full-adder-sum-delay.
(define (ripple-adder-delay n)
  (+ (* (- n 1)
        full-adder-carry-delay)
     full-adder-sum-delay))
;;
;; For the given values, the calculations give 16 time steps per adder bit.
;;
;; The following function verifies this by running an actual addition.
;; It takes two inputs and a bit width. For the worst case delay try adding
;; 1 to 2^n-1:
;;
;; (let ((n 16)) (demo-ripple-adder (- (expt 2 n) 1) n n))

(define (demo-ripple-adder a b n)
  (set! the-agenda (make-agenda))

  (let ((Ak (make-n-wires n))
        (Bk (make-n-wires n))
        (Sk (make-n-wires n))
        (C (make-wire)))
    (ripple-carry-adder Ak Bk Sk C)
    (let ((Abits (num->base a 2 n))
          (Bbits (num->base b 2 n)))
      (show abits)
      (show bbits)
      (set-signals! Ak Abits)
      (set-signals! Bk Bbits)
      (probe-each 'sum Sk)
      (probe '*carry* C)
      (propagate)
      (let ((outbits (cons (get-signal C) (get-signals Sk))))
        (show outbits)
        (base->num outbits 2)))))

(define (make-n-wires n)
  (define (iter n wires)
    (if (= n 0)
        wires
        (iter (- n 1)
              (cons (make-wire) wires))))
  (iter n '()))

(define (probe-each name L)
  (define (iter n L)
    (if (null? L)
        'done
        (begin
          (probe (list name n) (car L))
          (iter (+ n 1) (cdr L)))))
  (iter 0 L))

(define (set-signals! L signals) (map set-signal! L signals))

(define (get-signals L) (map get-signal L))

(define (num->base value base digits)
  (define (iter value place result)
    (if (<= place 0)
        result
        (let* ((ones (modulo value base))
               (rest (/ (- value ones) base)))
          (iter rest (- place 1) (cons ones result)))))
  (iter value digits '()))

(define (base->num values base)
  (define (iter accum values)
    (if (null? values) accum
        (iter (+ (car values)
                 (* base accum))
              (cdr values))))
  (iter 0 values))

;;; Exercise 3.31

;; First consider why it takes 8 time units to output the sum?
;; Consider the half adder circuit that is shown in Fig. 3.25. I will
;; make a timing diagram explaining the output. Note that the problem
;; construction simulates an initial value of 0 on all wires, with a
;; 0-1 transition occurring at time coordinate 0.
;;
;;      +-----------------------------------+
;;      |                                   |
;;  A --+-----+---+--\  D                   |
;;      |     |   |OR+---------+---\        |
;;      |  +--+---+--/       E |AND+--------+-- S
;;      |  |  |         +-NOT--+---/        |
;;      |  |  |         |                   |
;;      |  |  +---+---\ |                   |
;;      |  |      |AND+-+-------------------+-- C
;;  B --+--+------+---/                     |
;;      |                                   |
;;      +-----------------------------------+
;;
;;      +--------------------------+
;;     A|xxx/^^^^^^^^^^^^^^^^^^^^^^|  +------------------+
;;     B|xxx\____________/^^^^^^^^^|  |Key:              |
;;     C|xxx\______________/^^^^^^^|  |_/^ 0-1 transition|
;;     D|xxx\____/^^^^^^^^^^^^^^^^^|  |^\_ 1-0 transition|
;;     E|xxx\_/^^^^^^^^^^^^^^^\____|  |xxx indeterminate |
;;     S|xxx\________/^^^^^^^^^^^\_|  +------------------+
;;      +---+-+--+---+--+--+--+--+-+
;;  time    0 2  5   8  8 11 13 16
;;                   v  ^        v (when control enters/exits top level)
;;
;; Now, if the update procedures did not run when objects were first
;; attached to wires, we would not have the inverter being informed of
;; its input at t=0, and it would then not schedule 0-1 transition at
;; time 2, and things generally fall apart. Here's my predicted timing
;; diagram in that case:
;;
;;      +--------------------+
;;     A|xxx/^^^^^^^^^^^^^^^^|
;;     B|xxx_________/^^^^^^^|
;;     C|xxx____________/^^^^|
;;     D|xxx_____/^^^^^^^^^^^|
;;     E|xxx_________________|
;;     S|xxx_________________|
;;      +---+----+---+--+--+-+
;;  time    0    5   5  8 10
;;               v   ^     v
;;
;; The "sum" bit never gets set, because the inverter does not put its
;; output at the beginning.


;;; Exercise 3.32

;;
;;  A --+---\
;;      |AND|+-- C
;;  B --+---/
;;
;;   +-------------------+
;;  A|^^\                |
;;  B|__/                |
;;  C|_____?             |
;;   +--+--+-------------+
;;      0  3
;;
;; Suppose that the AND gate processed the B:0-1 transition before the
;; A:1-0 transition. Then the AND gate would try to lift its output,
;; placing (set-signal! output <1>) on the queue. But on responding to
;; the A transition, it would drop its output, placing (set-signal!
;; output <0> on the queue. Now the correct thing is for the output to
;; be zero after both updates have been processed. If the queue is
;; instead a list, though, the procedure to set 0 will execute before
;; the procedure to set 1, and the persisting state will be 1. So
;; tasks scheduled for the same time must be executed in the order
;; they are scheduled.


                                        ;                    PASTES FROM CHAPTER

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (fire-after-delay output and-gate-delay new-value)))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (fire-after-delay output inverter-delay new-value)))
  (add-action! input invert-input)
  'ok)

(define (fire-after-delay wire delay new-value)
  (assert-wire wire)
  (after-delay delay
               (lambda () (set-signal! wire new-value))))

(define (wire? wire)
  (and (procedure? wire) (eq? (wire 'class) 'wire)))

(define (wires? wires)
  (all wire? wires))

(define (assert-wire . wires)
  (if (wires? wires)
      'ok
      (error "not a wire:" wires)))

                                        ;                    WIRES
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures)))
      'done)
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            ((eq? m 'class) 'wire)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))

(define (get-name wire) (wire 'get-name))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))



                                        ;                    AGENDAS

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
       (let ((first-item (first-agenda-item the-agenda)))
         (first-item)
         (remove-first-agenda-item! the-agenda)
         (propagate))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))



                                        ;                    QUEUES

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


(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display (current-time the-agenda))
                 (display " ")
                 (display name)
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline))))


(define (trace name fun)
  ;; Wrap a function with a tracer.
  (lambda args
    (display "(")
    (display (symbol->string name))
    (display " ")
    (map (lambda (x) (write x) (display " ")) args)
    (display "\n")
    (let ((result (apply fun args)))
      (display " => ")
      (write result)
      (display ")\n")
      result)))

(define (all pred list)
  ;; apply predicate to each item; return #t if all results are non-#f.
  (if (null? list)
      #t
      (and (pred (car list)) (all pred (cdr list)))))



;;                                             UNMENTIONED BY BOOK BUT ESSENTIAL

(define the-agenda (make-agenda))