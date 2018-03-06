#lang racket

;;; * utilities
(require srfi/54)

(define (create-interval start-time end-time)
  (cons start-time end-time))

;;; Duplicate definitions for now.
(define (tsv->table tsv-path)
  (define (fields line) (regexp-split #px"\t" line))
  (define (doit in)
    (define (rl) (read-line in 'any))
    (define header (fields (rl)))
    (let loop ((l (rl))
               (r (list)))
      (if (eof-object? l)
          (reverse r)
          (loop (rl)
                (cons (map cons header (fields l)) r)))))
  (call-with-input-file tsv-path doit))

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
      (format "~a:~a ~a" h (cat m 2 #\0) ampm))))

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
(define (table-for tab)
  (define jobs-tab
    '((("job" . "job1")
       ("description" . "")
       ("department" . "")
       ("interval" . "9:30 pm 10:30 pm"))
      (("job" . "job2")
       ("description" . "")
       ("department" . "")
       ("interval" . ""))
      (("job" . "job3")
       ("description" . "")
       ("department" . "")
       ("interval" . "12:30 pm   1:30 pm"))
      (("job" . "job4")
       ("description" . "")
       ("department" . "")
       ("interval" . "8:30 am 9:30 am"))))
  (define shower-times-tab
    '(("time" .  "8:30 am")
      ("time" . "12:50 pm")))
  (define showers-tab0
    '((("room" . "Men Soak") ("capacity" . "2"))
      (("room" . "Women Soak") ("capacity" . "2"))
      (("room" . "Shower SEB2") ("capacity" . "1"))))
  (define showers-tab
    (tsv->table
     "/home/ejk/rzc/sesshin-greeter/test-data/good/showers.tsv"))
  (define roster-tab
    '((("name" . "Fred Flintstone")
       ("m/f" . "m")
       ("jobs" . "job1")
       ("room" . "SEB 4")
       ("bath" . "Shower SEB2")
       ("bath time" . ""))
      (("name" . "Vladimir von Zuckerstein")
       ("m/f" . "m")
       ("jobs" . "job2")
       ("room" . "SEB 5")
       ("bath" . "")
       ("bath time" . ""))
      (("name" . "Aphrodite Finknottle")
       ("m/f" . "f")
       ("jobs" . "job1")
       ("room" . "SE 7")
       ("bath" . "")
       ("bath time" . ""))
      (("name" . "Fabian Snodgrass")
       ("m/f" . "m")
       ("jobs" . "job3")
       ("room" . "W 6")
       ("bath" . "")
       ("bath time" . "8:30 am"))
      (("name" . "Cadwallader Colden")
       ("m/f" . "m")
       ("jobs" . "job4")
       ("room" . "BNE 1")
       ("bath" . "")
       ("bath time" . ""))))
  (case tab
    [(roster)       roster-tab]
    [(shower-times) shower-times-tab]
    [(showers)      showers-tab]
    [(jobs)         jobs-tab]
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

(define (rget key roster-entry)
  (cdr (assoc key roster-entry)))

(define (jobs-list roster-entry)
  (regexp-split #px"[,\\s]+" (rget "jobs" roster-entry)))

(define (shower-start->shower-interval start)
  (if (string=? start "")
      'undefined
      (cons start
            (min->hm (+ (hm->min start) 20)))))

(define shower-intervals
  (map (lambda (s)
         (shower-start->shower-interval (cdr s)))
       (table-for 'shower-times)))

;;; Sort shower locations by bedroom so that we assign showers that
;;; are nearby.
;;;
;;; |-----+--------------|
;;; | bed | showers      |
;;; |-----+--------------|
;;; | SE  | SE, SEB, ... |
;;; | SEB | SEB, SE, ... |
;;; | E   | SE, SEB, ... |
;;; | W   | NW, NEB, ... |
;;; | NE  | NEB, ...     |
;;; | BNE | NEB, ...     |
;;; |-----+--------------|
;;;
;;; Argument bedroomm is a string and showers is a list of strings.

(define (prioritize-showers showers bedroom)
  (define p<
    (cond
     ((regexp-match? #px"SE[^B]|^E" bedroom) ; southeast/east 1st fl.
      (lambda (a b)
        (cond
         ((regexp-match? #px"SE[^B]" a) #t)
         ((regexp-match? #px"SE[^B]" b) #f)
         ((regexp-match? #px"SE"     a) #t)
         ((regexp-match? #px"SE"     b) #f)
         (else                          #t))))
     ((regexp-match? #px"SE" bedroom)   ; southeast basement
      (lambda (a b)
        (cond
         ((regexp-match? #px"SEB" a) #t)
         ((regexp-match? #px"SEB" b) #f)
         ((regexp-match? #px"SE"  a) #t)
         ((regexp-match? #px"SE"  b) #f)
         (else                       #t))))
     ((regexp-match? #px"W" bedroom)    ; west
      (lambda (a b)
        (cond
         ((regexp-match? #px"NW"   a) #t)
         ((regexp-match? #px"NW"   b) #f)
         ((regexp-match? #px"NEB"  a) #t)
         ((regexp-match? #px"NEB"  b) #f)
         (else                       #t))))
     ((regexp-match? #px"NE" bedroom)    ; northeast
      (lambda (a b)
        (cond
         ((regexp-match? #px"NEB" a) #t)
         ((regexp-match? #px"NEB" b) #f)
         (else                       #t))))
     (else (lambda (a b) #t))))
  (sort showers p<))

(define (all-loc&time bedroom)
  (let* ((showers (map (lambda (s) (rget "room" s))
                       (table-for 'showers)))
         (showers (remove "Bath SE1" showers))
         (showers (prioritize-showers showers bedroom))
         (add-intervals (lambda (shower)
                          (map (lambda (interval)
                                 (list shower interval))
                               shower-intervals))))
    (apply append (map add-intervals showers))))

(define *stack* '())

(define (push k) (set! *stack* (cons k *stack*)))

(define (pop)
  (if (null? *stack*)
      'stack-empty
      (let ((k (car *stack*)))
        (set! *stack* (cdr *stack*))
        k)))

(define (note x) (call/cc (lambda (k) (when x (push k)) x)))

(define (eos? k) (eq? k 'stack-empty))

(define (backup)
  (let ((k (pop)))
    (if (eos? k)
        (error 'impossible)
        (k #f))))

(define (jobs-for name [r (table-for 'roster)])
  (cond
   ((null? r) #f)
   ((string=? (rget "name" (car r)) name)
    (jobs-list (car r)))
   (else (jobs-for name (cdr r)))))

(define (string-interval->time-interval string-interval)
  (if (string=? string-interval "")
      '()
      (let* ((r "[0-9]+:[0-9]{2}\\s*[ap]m")
             (r (pregexp (format "(~a)\\s*(~a)" r r)))
             (r (regexp-match r string-interval)))
        (apply create-interval (cdr r)))))

(define (job-times-for jobs)
  (define (job-times-for-a job)
    (string-interval->time-interval
     (rget "interval"
           (car (filter (lambda (jt) (string=? (rget "job" jt) job))
                        (table-for 'jobs))))))
  (remove* '(()) (map job-times-for-a jobs)))

(define (capacity-for shower)
  (string->number
   (cdadr
    (assoc (cons "room" shower) (table-for 'showers)))))

;;; The preliminary table may indicate that an individual is to use a
;;; certain bath, a certain bath time or both.
(define (initialize-assigs)
  (define (init-a-info roster-entry)
    (let* ((n (rget "name" roster-entry))
           (g (rget "m/f" roster-entry))
           (r (rget "room" roster-entry))
           (l (rget "bath" roster-entry))
           (l (if (string=? l "") 'undefined l))
           (t (shower-start->shower-interval
               (rget "bath time" roster-entry))))
      (list n g r l t)))
  (map init-a-info (table-for 'roster)))

;;; Find a bath and bath time for individual w/possible partial
;;; pre-assignment. (a-info is the preliminary
;;; name/gender/bedroom/location/time interval.)
(define (update-assigs a-info assigs)
  (match-let (((list name gender room loc time) a-info))
    (define (used-already? loc&time)
      (let ((n (length
                (filter (lambda (a) (equal? loc&time (cdr a)))
                        assigs))))
        (>= n (capacity-for (car loc&time)))))
    (define (time-conflict? loc&time)
      (time-collision? (cadr loc&time)
                       (job-times-for (jobs-for name))))
    (define (loc-ok? loc&time)
      (or (eq? loc 'undefined) (string=? loc (car loc&time))))
    (define (time-ok? loc&time)
      (or (eq? time 'undefined) (equal? time (cadr loc&time))))
    (define (good-sex? loc&time)
      (let* ((l (car loc&time))
             (g (cond
                 ((regexp-match? #px"Women" l) "f")
                 ((regexp-match? #px"Men"   l) "m")
                 (else 'undefined))))
        (or (eq? g 'undefined)
            (string=? g gender))))
    (define (works? loc&time)
      (and (not (used-already? loc&time))
           (not (time-conflict? loc&time))
           (loc-ok? loc&time)
           (time-ok? loc&time)
           (good-sex? loc&time)))
    (let loop ((l&t (all-loc&time room)))
      (cond
       ((null? l&t) (backup))
       ((note (works? (car l&t)))
        (cons (cons name (car l&t)) assigs))
       (else (loop (cdr l&t)))))))

(define (create-assignments)
  (let loop ((pre-assigs  (initialize-assigs))
             (post-assigs '()))
    (if (null? pre-assigs)
        post-assigs
        (loop (cdr pre-assigs)
              (update-assigs (car pre-assigs) post-assigs)))))
