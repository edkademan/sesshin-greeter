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

;; (define showers
;;   '(("Shower SEB 1" . 1)))

;; (define shower-times
;;   '(("12:50pm" . "1:10pm")))

(define roster
  '(("Fred Flintstone"          job1)
    ("Vladimir von Zuckerstein" job2)
    ("Fabian Snodgrass"         job3)))

;;; * showers
;;;
;;; An assig is a location-time combination. For example
;;;   ("Shower SEB 1" . ("8:30am" . "8:50am"))
;;; and assigs are a list of such pairs:
;;;   '(("Shower SEB 1" . (" 8:30am" . "8:50am"))
;;;     ("Shower SEB 1" . (" 8:50am" . "9:10am"))
;;;     ("Shower SEB 1" . ("12:50pm" . "1:10pm")))

(define all-assigs
  (apply append
         (map (lambda (s) (map (lambda (t) (cons (car s) t))
                          shower-times))
              showers)))

;;; update assigs for individual w/all-assigs
;;; if all-assigs is empty error
;;; for each assig in all-assigs
;;;   if appropriate for individual (jobtimes & capacity)
;;;     update assigs with assig
;;;     return assigs
;;; ;nothing appropriate
;;; remove previous assig from assigs and remove temporarily from
;;; all-assigs
;;; now update assigs for previous individual w/temp all-assigs
;;; restore all-assigs to original state and redo
;;;
;;;
;;;
;;; start out with assigs empty
;;; for each individual
;;; 





(define past '())

(define (note x)
  (call-with-composable-continuation
   (lambda (k)
     (set! past (cons k past))
     x)))









(define (job-for name) (cadr (assoc name roster)))
(define (job-times-for job [jtimes jobs])
  (cond
   ((null? jtimes) #f)
   ((eq? job (caar jtimes)) (cadar jtimes))
   (else (job-times-for job (cdr jtimes)))))
