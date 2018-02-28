#lang racket

;;; * utilities
(define (create-interval start-time end-time)
  (cons start-time end-time))

(define (hm->min hm)
  (let* ((hma (regexp-match #px"(.+):([0-9]+)(..)" hm))
         (h   (string->number (list-ref hma 1)))
         (h   (modulo h 12))
         (m   (string->number (list-ref hma 2)))
         (am? (regexp-match? #px"[aA]" (list-ref hma 3)))
         (h   (+ h (if am? 0 12))))
    (+ (* 60 h) m)))

(define (interval-start time-interval) (car time-interval))
(define (interval-end   time-interval) (cdr time-interval))

(define (time-collision? interval list-of-intervals)
  (define (to-min interval)
    (create-interval (hm->min (interval-start interval))
                     (hm->min (interval-end   interval))))
  (define (collision? i1 i2)
    (let* ((i1 (to-min i1))
           (i2 (to-min i2))
           (j2 #f))
      ;; Ensure that interval i1 starts first.
      (when (> (interval-start i1) (interval-start i2))
        (set! j2 i2)
        (set! i2 i1)
        (set! i1 j2))
      (>= (interval-end i1) (interval-start i2))))
  (cond
   ((null? list-of-intervals) #f)
   ((collision? interval (car list-of-intervals)) #t)
   (else (time-collision? interval (cdr list-of-intervals)))))

;;; * data

(define jobs
  '((job1 (( "8:30am" .  "9:30am")))
    (job2 (("12:30pm" .  "1:30pm")
           ( "5:30pm" .  "6:30pm")))
    (job3 (( "9:30pm" . "10:30pm")))))

(define showers
  '(("Shower SEB 1" . 1)
    ("Shower SEB 2" . 1)
    ("Shower SEB 3" . 1)))

(define shower-times
  '(( "8:30am" . "8:50am")
    ( "8:50am" . "9:10am")
    ("12:50pm" . "1:10pm")))

(define roster
  '(("Fred Flintstone"          job1)
    ("Vladimir von Zuckerstein" job2)
    ("Fabian Snodgrass"         job3)))

;;; * showers
;;; Assign showers and shower times to the individuals that do not
;;; interfere with their jobs.

(define (job-for name) (cadr (assoc name roster)))
(define (job-times-for job [jtimes jobs])
  (cond
   ((null? jtimes) 'unknown-job)
   ((eq? job (caar jtimes)) (cadar jtimes))
   (else (job-times-for job (cdr jtimes)))))

;;; Find a shower time for name from among shower-times.
(define (shower-time-for name shower-times)
  (cond
   ((null? shower-times) 'no-shower-time)
   ((time-collision? (car shower-times)
                     (job-times-for (job-for name)))
    (shower-time-for name (cdr shower-times)))
   (else (car shower-times))))

;;; Return the number of people using the shower at the shower time.
(define (n-shower-users shower shower-time)
  (define (match? shower-assignment)
    (and (string=? shower (cadr shower-assignment))
         (equal? shower-time (caddr shower-assignment))))
  (length (filter match? shower-assignments)))

(define (shower-capacity shower) (cdr (assoc shower showers)))

;;; Allocate a shower at time shower-time from among showers.
(define (find-shower shower-time showers)
  (if (null? showers)
      'no-shower
      (let ((shower  (caar showers))
            (showers (cdr showers)))
        (if (< (n-shower-users shower shower-time)
               (shower-capacity shower))
            shower
            (find-shower shower-time showers)))))

(define (create-shower-assignments)
  (let loop ((names (map car roster))
             (showers showers)
             (times shower-times)
             (assign '()))  ;(e ...), e is (name shower time)
    (if (null? names)
        assign
        (let ((st (shower-time-for (car names) times)))
          ))))

;;; (remove v lst) works for shower times
