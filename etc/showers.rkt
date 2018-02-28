#lang racket

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

;;; Assign showers and shower times to the individuals that do not
;;; interfere with their jobs.

(define (assign-showers)
  (define (time-for))
  )
