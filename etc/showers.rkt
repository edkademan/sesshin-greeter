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

;; (define showers
;;   '(("Shower SEB 1" . 1)
;;     ("Shower SEB 2" . 1)
;;     ("Shower SEB 3" . 1)))

;; (define shower-times
;;   '(( "8:30am" . "8:50am")
;;     ( "8:50am" . "9:10am")
;;     ("12:50pm" . "1:10pm")))

(define showers
  '(("Shower SEB 1" . 1)))

(define shower-times
  '(("12:50pm" . "1:10pm")))

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
   ((null? jtimes) #f)
   ((eq? job (caar jtimes)) (cadar jtimes))
   (else (job-times-for job (cdr jtimes)))))

;;; Find a shower time for name from among shower-times.
(define (shower-time-for name shower-times)
  (cond
   ((null? shower-times) #f)
   ((time-collision? (car shower-times)
                     (job-times-for (job-for name)))
    (shower-time-for name (cdr shower-times)))
   (else (car shower-times))))

;;; Return the number of people using the shower at the shower time.
(define (n-shower-users shower shower-time shower-assignments)
  (define (match? shower-assignment)
    (and (string=? shower (cadr shower-assignment))
         (equal? shower-time (caddr shower-assignment))))
  (length (filter match? shower-assignments)))

(define (shower-capacity shower) (cdr (assoc shower showers)))

;;; Allocate a shower at time shower-time from among showers.
(define (find-shower shower-time showers shower-assignments)
  (if (null? showers)
      #f
      (let ((shower  (caar showers))
            (showers (cdr showers)))
        (if (< (n-shower-users shower shower-time shower-assignments)
               (shower-capacity shower))
            shower
            (find-shower shower-time showers shower-assignments)))))

;;; for an individual
;;; for each possible shower time
;;;   for each shower
;;;     if time/shower is available save to assign
;;; if no time/shower available
;;;   remove the shower used by the previous individual and redo the
;;;   assignment for that individual
;;;

;;; (REDO) means find the shower for the previous assignment and
;;; remove it from showers, and remove the previous assignment from
;;; assign and do over for the previous individual. Then, when you get
;;; another set of assignments, restore the shower and do over for the
;;; current individual.

(define (reassign-previous-shower/time showers times assign)
  (match-let ([(list name shower time) (car assign)])
    (let* ((shs (remove shower showers))
           (assign (assign-a-shower/time
                    name shs times (cdr assign))))
      assign)))

(define (assign-a-shower/time name showers times assign)
  (and (null? showers) (null? times) (error 'impossible))
  (let ((st (shower-time-for name times)))
    (cond
     ((not st)
      (assign-a-shower/time
       name showers times
       (reassign-previous-shower/time showers times assign)))
     (else
      (let ((sh (find-shower st showers assign)))
        (if (not sh)
            (assign-a-shower/time
             name showers times
             (reassign-previous-shower/time showers times assign))
            (cons (list name sh st) assign)))))))

(define (create-shower-assignments)
  (let loop ((names (map car roster))
             (showers showers)
             (times shower-times)
             (assign '()))  ;(e ...), e is (name shower time)
    (if (null? names)
        assign
        (loop (cdr names)
              showers times
              (call-with-composable-continuation
               (lambda (k)
                 
                 ))
              (assign-a-shower/time
               (car names) showers times assign)))))
