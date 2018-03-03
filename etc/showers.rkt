#lang racket

;;; * utilities
(require srfi/54)

(define (create-interval start-time end-time)
  (cons start-time end-time))

(define (hm->min hm)
  (let* ((hma (regexp-match #px"(.+):([0-9]+)\\s*(..)" hm))
         (h   (string->number (list-ref hma 1)))
         (h   (modulo h 12))
         (m   (string->number (list-ref hma 2)))
         (am? (regexp-match? #px"[aA]" (list-ref hma 3)))
         (h   (+ h (if am? 0 12))))
    (+ (* 60 h) m)))

(define (min->hm min)
  (let-values ([(h m) (quotient/remainder min 60)])
    (let* ((h (modulo h 12))
           (h (if (zero? h) 12 h))
           (ampm (if (or (< min (* 12 60))
                         (= min (* 24 60)))
                     "am" "pm")))
      (format "~a:~a ~a" h m ampm))))

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

;;; must generate shower intervals

(define (table-for tab)
  (define job-times-tab
    '((("job" . "job1")
       ("intervals" . (("9:30 pm" . "10:30 pm"))))
      (("job" . "job2")
       ("intervals" . (("12:30 pm" . "1:30 pm")
                       ("5:30 pm" . "6:30 pm"))))
      (("job" . "job3")
       ("intervals" . (("12:30 pm" . "1:30 pm"))))
      (("job" . "job4")
       ("intervals" . (("7:30 am" . "8:30 am"))))))
  (define shower-times-tab
    '((("time" .  "8:30 am"))
      (("time" . "12:50 pm"))))
  (define showers-tab
    '((("room" . "Shower NEB1") ("capacity" . "1"))
      (("room" . "Shower SEB1") ("capacity" . "1"))
      (("room" . "Shower SEB2") ("capacity" . "1"))))
  (define roster-tab
    '((("name" . "Fred Flintstone")
       ("jobs" . "job1")
       ("room" . "SEB 4")
       ("bath" . "")
       ("bath time" . "")
       ("gender" . "m"))
      (("name" . "Vladimir von Zuckerstein")
       ("jobs" . "job2")
       ("room" . "SEB 5")
       ("bath" . "")
       ("bath time" . "")
       ("gender" . "m"))
      (("name" . "Fabian Snodgrass")
       ("jobs" . "job3")
       ("room" . "SEB 6")
       ("bath" . "")
       ("bath time" . "")
       ("gender" . "m"))
      (("name" . "Cadwallader Colden")
       ("jobs" . "job4")
       ("room" . "BNE 1")
       ("bath" . "")
       ("bath time" . "")
       ("gender" . "m"))))
  (case tab
    [(roster)       roster-tab]
    [(shower-times) shower-times-tab]
    [(showers)      showers-tab]
    [else 'not-found]))
;;; * showers
;;;
;;; An assig is a name-location-time combination. For example
;;;   ("Fred Flintstone" "Shower SEB 1" ("8:30am" . "8:50am"))
;;; and all-loc&time is a list each of whose elements is a list of
;;; location and time
;;;   '(("Shower SEB 1" (" 8:30am" . "8:50am"))
;;;     ("Shower SEB 1" (" 8:50am" . "9:10am"))
;;;     ("Shower SEB 1" ("12:50pm" . "1:10pm")))

;; (define all-loc&time
;;   (apply append
;;          (map (lambda (s) (map (lambda (t) (list (car s) t))
;;                           shower-times))
;;               showers)))

;; (define *stack* '())

;; (define (push k) (set! *stack* (cons k *stack*)))

;; (define (pop)
;;   (if (null? *stack*)
;;       'stack-empty
;;       (let ((k (car *stack*)))
;;         (set! *stack* (cdr *stack*))
;;         k)))

;; (define (note x) (call/cc (lambda (k) (when x (push k)) x)))

;; (define (eos? k) (eq? k 'stack-empty))
;; (define (backup)
;;   (let ((k (pop)))
;;     (if (eos? k)
;;         (error 'impossible)
;;         (k #f))))

;; (define (job-for name) (cadr (assoc name roster)))
;; (define (job-times-for job [jtimes jobs])
;;   (cond
;;    ((null? jtimes) #f)
;;    ((eq? job (caar jtimes)) (cadar jtimes))
;;    (else (job-times-for job (cdr jtimes)))))

;; (define (update-assigs name assigs)
;;   (define (used-already? loc&time)
;;     (let ((n (length
;;               (filter (lambda (a) (equal? loc&time (cdr a)))
;;                       assigs)))
;;           (c (cadr (assoc (car loc&time) showers))))
;;       (>= n c)))
;;   (define (works? loc&time)
;;     (not (or (time-collision? (cadr loc&time)
;;                               (job-times-for (job-for name)))
;;              (used-already? loc&time))))
;;   (let loop ((l&t all-loc&time))
;;     (cond
;;      ((null? l&t) (backup))
;;      ((note (works? (car l&t)))
;;       (cons (cons name (car l&t)) assigs))
;;      (else (loop (cdr l&t))))))

;; (define (create-assignments [names (map car roster)] [assigs '()])
;;   (if (null? names)
;;       assigs
;;       (create-assignments (cdr names)
;;                           (update-assigs (car names) assigs))))
