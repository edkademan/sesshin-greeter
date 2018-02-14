#lang racket

;;; * general
;;; ** common routines
(require json srfi/54 racket/draw)

(define (text->pdf text output-file
                   #:font-size         [font-size 16]
                   #:font-face         [font-face "Courier"]
                   #:vertical-spacing  [vertical-spacing 0]
                   #:horizontal-offset [horizontal-offset 20]
                   #:vertical-offset   [vertical-offset 30]
                   #:orientation       [orientation 'portrait])
  (define font (make-font #:size font-size
                          #:size-in-pixels? #t
                          #:face font-face))
  (define (up-int x) (inexact->exact (ceiling x)))
  (define (get-dim width-or-height)
    (let ((short 592)
          (long  756))
      (case width-or-height
        [(width)  (if (eq? orientation 'portrait) short long)]
        [(height) (if (eq? orientation 'portrait) long short)])))
  (define (pages text)
    (let loop ((lines (regexp-split #px"\n" text))
               (current-page (list))
               (list-of-pages (list)))
      (cond
       ((null? lines)
        (reverse (cons (reverse current-page) list-of-pages)))
       ((regexp-match? #px"\f" (car lines)) ;new page
        (loop (cons (regexp-replace #px"\f" (car lines) "")
                    (cdr lines))
              (list)
              (cons (reverse current-page) list-of-pages)))
       (else (loop (cdr lines)
                   (cons (car lines) current-page)
                   list-of-pages)))))
  (define dc
    (new pdf-dc%
         [interactive #f]
         [use-paper-bbox #f]
         [width (get-dim 'width)]
         [height (get-dim 'height)]
         [output output-file]
         [as-eps #f]))

  ;; Argument page is a list of lines.
  (define (print-lines page [width 0] [height vertical-offset])
    (when (pair? page)
      (let-values (((w h d v) (send dc get-text-extent (car page))))
        (send dc draw-text (car page) horizontal-offset height)
        (print-lines (cdr page) (max width (up-int w))
                     (+ vertical-spacing height (up-int h))))))

  (define (print-out page)
    (send dc start-page)
    (print-lines page)
    (send dc end-page))

  (send* dc
    (start-doc "useless string")
    (set-font font))
  
  (for-each print-out (pages text))

  (send dc end-doc))

(define home-dir        (make-parameter #f))
(define input-dir       (make-parameter #f))
(define output-dir      (make-parameter #f))
(define tmp-dir         (make-parameter #f))
(define external-format (make-parameter #f))

(define (complete-source-path base ext)
  (path-add-extension (build-path (input-dir) base) ext))

(define (docx->json from to)
  (system
   (format "pandoc --from=docx --to=json --output=~a ~a" to from)))

;;; Return the `c' component of a hash.
(define (hc hash) (hash-ref hash 'c))

(define (hc-or-space hash)
  (let ((r (hc hash))) (if (string? r) r " ")))

(define (json->table json-path)
  (define (the-table json-path)
    (let* ((t (call-with-input-file json-path read-json))
           (t (hash-ref t 'blocks))
           (t (hc (car t)))
           (t (list-ref t 4)))
      t))
  (define (unpack x) (apply string-append (map hash->text x)))
  (define (unpack-e x)
    (if (not (pair? x)) "" (unpack (hc (car x)))))
  (define (extract-header t)
    (let* ((h (car t))
           (h (map (lambda (x) (hc (car x))) h))
           (h (map unpack h)))
      h))
  (define (hash->text x)
    (if (string=? (hash-ref x 't) "Space")
        " "
        (hash-ref x 'c)))
  (define (extract-entries t) (map (lambda (e) (map unpack-e e)) (cdr t)))
  (let* ((t (the-table json-path))
         (h (extract-header t))
         (e (extract-entries t))
         (e (map (lambda (x) (map cons h x)) e)))
    e))

(define (docx->table docx-path)
  (let ((json-path (build-path (tmp-dir) "table.json")))
    (docx->json docx-path json-path)
    (json->table json-path)))

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

;;; Convert the information in the source file whose basename is base
;;; to a table. For example (source-table "roster"). By the time we
;;; use this procedure the system understands what the source
;;; format---and hence the file extension---is.
(define (source->table base)
  (let* ((ext (string-append
               "."
               (symbol->string (external-format))))
         (path (complete-source-path base ext)))
    (case (external-format)
      [(docx) (docx->table path)]
      [(json) (json->table path)]
      [(tsv)  (tsv->table  path)])))

(define (compile-source)
  (define (label-for t)
    (let ((e (car t)))
      (cond
       ((assoc "origin" e)     'roster)
       ((assoc "department" e) 'jobs)
       ((assoc "capacity" e)   'showers)
       ((assoc "room" e)       'bedrooms)
       ((assoc "time" e)       'shower-times)
       (else 'unknown))))
  (define (label-table t) (cons (label-for t) t))
  (let ((s (map label-table
                (list
                 (source->table "roster")
                 (source->table "jobs")
                 (source->table "rooms")
                 (source->table "showers")
                 (source->table "shower-times")))))
    (lambda (table-label)
      (cdr (assoc table-label s)))))

;;; (define table-for (compile-source))
;;;
;;; (table-for 'roster)       ==> roster table
;;; (table-for 'jobs)         ==> jobs table
;;; (table-for 'showers)      ==> showers table
;;; (table-for 'shower-times) ==> shower times table
;;; (table-for 'bedrooms)     ==> bedrooms table

(define (table-for info) #f)

(define (simplify-characters str)
  (let* ((str (regexp-replace* #px"é" str "e"))
         (str (regexp-replace* #px"—" str "---")))
    str))

(define (rget key roster-entry)
  (simplify-characters (cdr (assoc key roster-entry))))

(define (los->str los) (apply string-append los))

(define (los->str-w/nl los)
  (string-append (los->str los) "\n"))

(define (firstlast name)
  (let ((lf (regexp-split #px"\\s*,\\s*" name)))
    (if (= (length lf) 1)
        (car lf)
        (format "~a ~a" (cadr lf) (car lf)))))

(define (publish-text text output-file-path . text2pdf-arglist)
  (define (display-text-to file-port)
    (let-values ([(p stdout stdin stderr)
                  (apply subprocess file-port #f 'stdout
                         (build-path (home-dir) "bin" "text2pdf")
                         text2pdf-arglist)])
      (display text stdin)
      (close-output-port stdin)
      (subprocess-wait p)))
   (call-with-output-file #:exists 'replace
                          output-file-path
                          display-text-to))

;;; ** Debug

(define (init-greet)
  (home-dir        (build-path "/home/ejk/rzc/sesshin-greeter"))
  (tmp-dir         (build-path "/tmp"))
  (external-format 'tsv)
  (input-dir       (build-path (home-dir) "test-data" "bad"))
  (output-dir      (build-path (home-dir) "out"))
  (set! table-for  (compile-source)))

;;; * bedrooms

(define (rooms-w/occupants)
  (define (room-occupants room-entry)
    (define (occupies? roster-entry)
      (string=? (rget "room" roster-entry)
                (rget "room" room-entry)))
    (define (extract-name roster-entry)
      (rget "name" roster-entry))
    (cons (rget "room" room-entry)
          (map extract-name
               (filter occupies? (table-for 'roster)))))
  (map room-occupants (table-for 'bedrooms)))

;;; Argument `room-w/occupants' is a list of strings whose first
;;; element is the name of the room and whose remaining elements are
;;; the names of the occupants.
(define (room-stanza room-w/occupants)
  (define initial-loo    ;initial list of occupants
    (let ((loo (cdr room-w/occupants)))
      (if (null? loo) '("") loo)))
  (let loop ((1st (car room-w/occupants))
             (occ initial-loo)
             (r (list)))
    (if (null? occ)
        (reverse r)
        (loop ""
              (cdr occ)
              (cons (string-append (cat 1st -8)
                                   (cat (car occ) -30))
                    r)))))

(define (room-region stanza)
  (case (car (regexp-split #px"\\s+" (car stanza)))
    [("SE")  "Southeast Hall"]
    [("SEB") "Southeast Hall Basement"]
    [("W")   "West Hall"]
    [("E")   "East Hall"]
    [("NE")  "Northeast Hall"]
    [("BNE") "Northeast Hall Basement"]
    [else    "Not in Retreat Center"]))

(define (blockify-rooms [rwo (rooms-w/occupants)])
  (let loop ((p      (map room-stanza rwo))
             (region "")
             (rblock (list))
             (r      (list)))
    (if (null? p)
        (cdr (reverse (cons rblock r)))
        (let* ((rm (car p))             ;list of strings
               (rg (room-region rm)))
          (when (not (string=? rg region))
            (set! r (cons rblock r))
            (set! rblock (list rg "")))
          (loop (cdr p)
                rg
                (append rblock rm)
                r)))))

(define (extend los n)
  (let loop ((m (max 0 (- n (length los))))
             (l (reverse los)))
    (if (zero? m)
        (reverse l)
        (loop (- m 1)
              (cons "" l)))))

(define (auto-determine-column-width loc)
  (define (max-string-length col)
    (apply max (map string-length col)))
  (apply max (map max-string-length loc)))

(define (pr loc [column-width (auto-determine-column-width loc)])
  (define (widen los n) (map (lambda (str) (cat str (- n))) los))
  (let* ((n (apply max (map length loc)))
         (loc (map (lambda (c) (extend c n)) loc))
         (loc (map (lambda (c) (widen c column-width)) loc)))
    (los->str (map los->str-w/nl (apply map list loc)))))

(define (pr-blocks blocks page-length)
  (let loop ((blocks blocks)
             (current-column (list))
             (result (list)))
    (define (update-current-column)
      (if (null? current-column)
          (car blocks)
          (append current-column (cons "" (car blocks)))))
    (cond
     ((null? blocks)
      (pr (reverse (cons current-column result))))
     ((> (+ 1
            (length current-column)
            (length (car blocks)))
         page-length)
      (loop (cdr blocks) (car blocks) (cons current-column result)))
     (else
      (loop (cdr blocks) (update-current-column) result)))))

(define (pr-rooms [page-length 40])
  (pr-blocks (blockify-rooms) page-length))

(define (publish-rooms [output-file-path
                        (build-path (output-dir) "rooms.pdf")])
  (text->pdf (pr-rooms) output-file-path))
;;; * showers/baths

(define (showers-w/occupants)
  (let ((roster (table-for 'roster)))
    (define (occupants-for time place)
      (define (match? roster-entry)
        (and (string=? time  (rget "bath time" roster-entry))
             (string=? place (rget "bath"      roster-entry))))
      (let* ((p (filter match? roster))
             (p (map (lambda (x) (rget "name" x)) p)))
        (list time place p)))
    (map (lambda (time)
           (map (lambda (place) (occupants-for time place))
                (map cdar (table-for 'showers))))
         (map cdar (table-for 'shower-times)))))

(define (string-shower-line time occ time-len name-len)
  (los->str-w/nl
   (cons (cat (cat time time-len) (- (+ time-len 1)))
         (map (lambda (x) (cat x (list name-len) (- (+ name-len 1))))
              occ))))

(define (string-occ-for-time time-entry time-len name-len)
  (define (extract-name shower-for-time)
    (let ((name (caddr shower-for-time)))
      (if (pair? name) (car name) ",")))
  (define (nobody-here? loo)
    (null? (filter (lambda (x) (not (string=? x ","))) loo)))
  (define (ssl time occ)
    (string-shower-line time occ time-len name-len))
  (let* ((time (caar time-entry))
         (occ (map extract-name time-entry)))
    (if (nobody-here? occ)
        (ssl time (make-list (length occ) ""))
        (let ((lo-name-pairs
               (map (lambda (x) (regexp-split #px"\\s*,\\s*" x)) occ)))
          (string-append
           (ssl time (map cadr lo-name-pairs))
           (ssl ""   (map car  lo-name-pairs)))))))

(define (bath-entry->shower swo-entry)
  (define (add-suffixes x) (map (lambda (n) (format "~a~a" x n)) '(1 2)))
  (define (double-up header) (apply append (map add-suffixes header)))
  (define (spread name-lst)
    (cond
     ((null? name-lst) (list '() '()))
     ((= (length name-lst) 1) (list name-lst '()))
     (else name-lst (map list name-lst))))
  (define (spread-names names)
    (apply append (map spread names)))
  (let ((time (caar swo-entry))
        (header (double-up (map cadr swo-entry)))
        (names (spread-names (map caddr swo-entry))))
    (map list (make-list (length header) time) header names)))

(define (string-showers-for re swo)
  (define (string-for swo)
    (let* ((time-len 8)
           (name-len 13)
           (header (map cadr (car swo))))
      (string-append
       (string-shower-line "" header time-len name-len)
       (los->str
        (map (lambda (e) (string-occ-for-time e time-len name-len)) swo)))))
  (define (extract-for re)
    (map (lambda (swo-entry)
           (filter (lambda (e) (regexp-match? re (cadr e))) swo-entry))
         swo))
  (define (bath? re) (regexp-match? re " Soak"))
  (define (bath->shower swo) (map bath-entry->shower swo))
  (let* ((s (extract-for re))
         (s (if (bath? re) (bath->shower s) s)))
    (string-for s)))

(define (string-showers [swo (showers-w/occupants)])
  (string-append
   "Southeast Building\n\n"
   (string-showers-for #px"\\s+SE" swo)
   "\n\nNorthern Buildings\n\n"
   (string-showers-for #px"\\s+N" swo)
   "\n\nSoaking Baths\n\n"
   (string-showers-for #px"\\s+Soak" swo)))

(define (publish-showers [output-file-path
                          (build-path (output-dir) "showers.pdf")])
  (text->pdf (string-showers) output-file-path #:font-size 12
             #:vertical-spacing -2))
;;; * jobs
(define (job-code job-entry)  (cdr (assoc "job" job-entry)))
(define (dept-code job-entry) (cdr (assoc "department" job-entry)))
(define (desc-code job-entry) (rget "description" job-entry))

(define (jobs-list roster-entry)
  (regexp-split #px"\\s*[,\\s]+\\s*"
                (rget "jobs" roster-entry)))

(define (jobs-w/workers)
  (let ((roster (table-for 'roster)))
    (define (workers-for job)
      (define (match? roster-entry)
        (member job (jobs-list roster-entry)))
      (map (lambda (r) (rget "name" r)) (filter match? roster)))
    (map (lambda (j) (cons j (workers-for j)))
         (map job-code (table-for 'jobs)))))

(define (departments)
  (remove-duplicates (map dept-code (table-for 'jobs))))

(define (jobs-in dept)
  (map job-code (filter (lambda (j) (string=? dept (dept-code j)))
                        (table-for 'jobs))))

(define (code->desc jcode)
  (let ((lod (filter (lambda (j) (string=? jcode (job-code j)))
                     (table-for 'jobs))))
    (if (null? lod) jcode (desc-code (car lod)))))

(define (job-para dept)
  (define (desc-len) 30)
  (define (name-len) 25)
  (define (abbrev-desc desc)
    (cond
     ((regexp-match? #px"^Water Table" desc)
      (let* ((desc (regexp-replace #px"Water Table" desc "W.Tab"))
             (desc (regexp-replace #px"Chanting" desc "Chant")))
        desc))
     (else desc)))
  (define (desc-worker->line desc worker)
    (cat (string-append (cat (abbrev-desc desc) (- (desc-len)))
                        (cat (firstlast worker) (- (name-len))))
         (- (+ (desc-len) (name-len)))))
  (let ((jobs (jobs-in dept))
        (jww (jobs-w/workers)))
      (define (lines-for job)
        (let* ((workers (cdr (assoc job jww)))
               (workers (if (null? workers) (list "") workers))
               (job-desc (code->desc job)))
          (let loop ((desc job-desc)
                     (w workers)
                     (r (list)))
            (if (null? w)
                (reverse r)
                (loop ""
                      (cdr w)
                      (cons (desc-worker->line desc (car w))
                            r))))))
      (apply append (map lines-for jobs))))

(define (collect-jobs/duties depts page-length)
  (define (job-block dept)
    (let* ((dept (symbol->string dept))
           (r (job-para dept))
           (r (cons "" r))
           (r (cons (string-titlecase dept) r)))
      r))
  (pr-blocks (map job-block depts) page-length))

(define (pr-jobs [page-length 40])
  (collect-jobs/duties '(kitchen outdoors housekeeping maintenance)
                       page-length))

(define (pr-duties [page-length 40])
  (let* ((duties (map dept-code (table-for 'jobs)))
         (duties (remove-duplicates duties))
         (duties (filter (lambda (x) (regexp-match? #px"zendo" x))
                         duties))
         (duties (map string->symbol duties)))
    (collect-jobs/duties duties page-length)))

(define (publish-jobs [output-file-path
                       (build-path (output-dir) "jobs.pdf")])
  (text->pdf (pr-jobs) output-file-path
             #:orientation 'landscape #:font-size 12))

(define (publish-zendo-jobs [output-file-path
                             (build-path (output-dir) "zendo.pdf")])
  (text->pdf (pr-duties) output-file-path
             #:orientation 'landscape #:font-size 12))
;;; * roster
(define (format-roster-entry . fields)
  (match-let (((list name room bath bath-time jobs) fields))
    (format "~a ~a ~a ~a ~a"
            (cat name -25) (cat room -8) (cat bath -12)
            (cat bath-time 10) (cat jobs -35))))

(define roster-header
  (format-roster-entry "" "Room" "Bath" "Bath Time" "Jobs"))

(define (roster-table->stanzas)
  (define (extra-job-line extra-job)
    (format-roster-entry "" "" "" "" extra-job))
  (define (jobs-desc-list roster-entry)
    (map code->desc (jobs-list roster-entry)))
  (define (create-stanza roster-entry)
    (define (r key) (rget key roster-entry))
    (let ((jobs (jobs-desc-list roster-entry)))
      (cons (format-roster-entry
             (r "name") (r "room") (r "bath") (r "bath time")
             (car jobs))
            (map extra-job-line (cdr jobs)))))
  (let loop ((ros (table-for 'roster))
             (res (list)))
    (if (null? ros)
        (reverse res)
        (loop (cdr ros) (cons (create-stanza (car ros)) res)))))

(define (roster-table->text page-length)
  (define (add-nl str) (string-append str "\n"))
  (define (add-header stanzas) (cons (list roster-header "") stanzas))
  (let loop ((stanzas (add-header (roster-table->stanzas)))
             (lines-so-far 0)
             (r (list)))
    (cond
     ((null? stanzas) (apply string-append r))
     ((> (+ lines-so-far (length (car stanzas)))
         page-length)
      (loop (add-header stanzas)
            0
            (append r (list "\f"))))
     (else
      (loop (cdr stanzas)
            (+ lines-so-far (length (car stanzas)))
            (append r (map add-nl (car stanzas))))))))

(define (publish-roster [output-file-path
                         (build-path (output-dir) "roster.pdf")])
  (text->pdf (roster-table->text 55) output-file-path #:font-size 11))
;;; * publish
(define (publish-all)
  (publish-roster)
  (publish-rooms)
  (publish-showers)
  (publish-jobs)
  (publish-zendo-jobs))
;;; * diagnostics

;;; People without valid jobs. 
(define (people-w/bad-jobs)
  (let* ((j (table-for 'jobs)))
    (define (bad-job? job)
      (not (member job (map (lambda (j) (rget "job" j)) j))))
    (define (bad-jobs-for roster-entry)
      (let ((jobs (jobs-list roster-entry)))
        (cons (rget "name" roster-entry)
              (filter bad-job? jobs))))
    (filter (lambda (b) (not (null? (cdr b))))
            (map bad-jobs-for (table-for 'roster)))))

;;; Jobs without people
(define (jobs-wo/people)
  (define (job-unfilled? job)
    (null? (filter
            identity
            (map (lambda (roster-entry)
                   (member job (jobs-list roster-entry)))
                 (table-for 'roster)))))
  (filter job-unfilled?
          (map (lambda (x) (rget "job" x)) (table-for 'jobs))))

;;; People without valid rooms.
(define (people-w/bad-rooms)
  (let* ((m (table-for 'bedrooms)))
    (define (bad-room? room)
      (not (member room (map (lambda (m) (rget "room" m)) m))))
    (define (bad-room-for roster-entry)
      (let ((room (rget "room" roster-entry)))
        (cons (rget "name" roster-entry)
              (and (bad-room? room) room))))
    (filter cdr (map bad-room-for (table-for 'roster)))))

;;; People without valid showers.
(define (people-w/bad-showers)
  (let* ((s (table-for 'showers)))
    (define (bad-shower? shower)
      (not (member shower (map (lambda (s) (rget "room" s)) s))))
    (define (bad-shower-for roster-entry)
      (let ((shower (rget "bath" roster-entry)))
        (cons (rget "name" roster-entry)
              (and (bad-shower? shower) shower))))
    (filter cdr (map bad-shower-for (table-for 'roster)))))

;;; People with bad shower times.
(define (people-w/bad-shower-times)
  (let* ((t (table-for 'shower-times)))
    (define (bad-shower-time? time)
      (not (member time (map (lambda (t) (rget "time" t)) t))))
    (define (bad-shower-time-for roster-entry)
      (let ((time (rget "bath time" roster-entry)))
        (cons (rget "name" roster-entry)
              (and (bad-shower-time? time) time))))
    (filter cdr (map bad-shower-time-for (table-for 'roster)))))

;;; Shower conflicts
(define (shower-conflicts)
  (let* ((s (table-for 'showers))
         (times      (map cdar (table-for 'shower-times)))
         (rooms      (map cdar s))
         (capacities (map (lambda (s) (string->number (cdadr s))) s)))
    (define (showerers-for time place)
      (define (tp-match? roster-entry)
        (and (string=? time  (rget "bath time" roster-entry))
             (string=? place (rget "bath"      roster-entry))))
      (map (lambda (x) (rget "name" x))
           (filter tp-match? (table-for 'roster))))
    (define (simultaneous-for place)
      (map (lambda (time) (length (showerers-for time place)))
           times))
    (define (conflicts-for place cap)
      (let* ((s (map (lambda (time) (list time
                                     place
                                     (showerers-for time place)))
                     times))
             (s (filter (lambda (x) (> (length (list-ref x 2)) cap))
                        s)))
        s))
    (map car
         (filter (lambda (x) (not (null? x)))
                 (map conflicts-for rooms capacities)))))

(define (lint [name-len 25])
  (let ((p (people-w/bad-jobs)))
    (cond
     ((null? p) (display "Everyone has a job.\n"))
     (else
      (display "People with bad jobs:\n")
      (let loop ((p p))
        (when (not (null? p))
          (display (format "~a  job: ~a~%"
                           (cat (caar p) (- name-len)) (cadar p)))
          (loop (cdr p)))))))

  (let ((p (jobs-wo/people)))
    (display "\n")
    (cond
     ((null? p) (display "All jobs are filled.\n"))
     (else
      (display "Jobs that aren't filled:\n")
      (let loop ((p p))
        (when (not (null? p))
          (display (format "~a~%" (car p)))
          (loop (cdr p)))))))

  (let ((p (people-w/bad-rooms)))
    (display "\n")
    (cond
     ((null? p) (display "Everyone has a room.\n"))
     (else
      (display "People with bad rooms:\n")
      (let loop ((p p))
        (when (not (null? p))
          (display (format "~a  room: ~a~%"
                           (cat (caar p) (- name-len)) (cdar p)))
          (loop (cdr p)))))))
  
  (let ((p (people-w/bad-showers)))
    (display "\n")
    (cond
     ((null? p) (display "Everyone has a shower/bath.\n"))
     (else
      (display "People with bad showers:\n")
      (let loop ((p p))
        (when (not (null? p))
          (display (format "~a  room: ~a~%"
                           (cat (caar p) (- name-len)) (cdar p)))
          (loop (cdr p)))))))
    
  (let ((p (people-w/bad-shower-times)))
    (display "\n")
    (cond
     ((null? p) (display "Everyone has a valid shower time.\n"))
     (else
      (display "People with bad shower times:\n")
      (let loop ((p p))
        (when (not (null? p))
          (display (format "~a  room: ~a~%"
                           (cat (caar p) (- name-len)) (cdar p)))
          (loop (cdr p)))))))

  (let ((p (shower-conflicts)))
    (display "\n")
    (cond
     ((null? p) (display "There are no shower conflicts.\n"))
     (else
      (display "Shower conflicts:\n")
      (let loop ((p p))
        (when (not (null? p))
          (let ((time   (list-ref (car p) 0))
                (place  (list-ref (car p) 1))
                (people (list-ref (car p) 2)))
            (display (format "~a at ~a:  ~a~%"
                             (cat place -15) (cat time 8)
                             (list-ref (car p) 2))))
          (loop (cdr p))))))))
;;; * main

(define (process-command-line)
  (home-dir        (build-path "/home/ejk/rzc/sesshin-greeter"))
  (tmp-dir         (build-path "/tmp"))
  (external-format 'json)
  (command-line
   #:once-each
   [("-i" "--input-dir") d "directory containing source tables"
    (input-dir (build-path d))]
   [("-o" "--output-dir") d "directory to contain pdf output"
    (output-dir (build-path d))]
   [("-f" "--format") f
    ((format "docx, json or tsv input, (defaults to ~a)"
             (external-format)))
    (external-format (string->symbol f))]
   [("-t" "--tmp-dir") d
    ("directory to contain intermediate output"
     (format "(defaults to ~a)" (tmp-dir)))
    (tmp-dir (build-path d))]))

(define (main)
  (process-command-line)
  (set! table-for (compile-source))
  (lint)
  (publish-all))

(module+ main (main))
