#lang racket

;;; * provide and global variables

(provide main)
(define *tmp-dir* "/tmp")
(define *start-date* #f)                ;defined later
(define regularize-name #f)             ;defined later
;;; * utilities
;;; Return the directory containing the sesshin doc files.
(define (dir-path)
  (build-path
   (find-system-path 'home-dir)
   "rzc/sesshin-greeter" *start-date*))

;;; Return the doc file path whose name matches the regular expression
;;; re.
(define (find-doc re)
  (let loop ((lst (directory-list (dir-path) #:build? #t)))
    (cond
     ((null? lst)
      (error (format "failed to find file w/~a" re)))
     ((regexp-match? re (path->string (car lst)))
      (car lst))
     (else (loop (cdr lst))))))

(define (doc-file->new-file doc-file ext)
  (let* ((f doc-file)
         (f (file-name-from-path f))
         (f (path->string f))
         (f (regexp-replace #px"\\.[^.]+$" f ext))
         (f (build-path *tmp-dir* f)))
    f))
(define (doc-file->txt-file doc-file)
  (doc-file->new-file doc-file ".txt"))
(define (doc-file->pdf-file doc-file)
  (doc-file->new-file doc-file ".pdf"))

;;; Find the doc file whose name matches the regular expression `re',
;;; convert it to a text file and then run `process-proc' on that
;;; text. The #:table argument determines whether or not to convert
;;; the text as a table.
(define (read-doc re process-proc #:table [table #f])
  (let* ((doc (find-doc re))
         (txt (doc-file->txt-file doc))
         (pdf (doc-file->pdf-file doc))
         (cmd (string-append
               "lowriter --convert-to " (if table "pdf" "txt")
               " --outdir " *tmp-dir* " \"" (path->string doc) "\""))
         (cmd (string-append
               cmd
               (if (not table)
                   ""
                   (string-append ";tabula " (path->string pdf)
                                  " -f TSV -o " (path->string txt)))))
         (cmd (string-append "(" cmd ") >/dev/null 2>&1")))
    (system cmd)
    (call-with-input-file txt process-proc)))

;;; Trim leading and trailing whitespace.
;;; There is some strange character that I had to include in the
;;; regular expression that prints as whitespace but that is
;;; considered a word constituent---at least as far as emacs is
;;; concerned.
(define (unpad str)
  (let* ((str (regexp-replace #px"^[\\s﻿]*" str ""))
         (str (regexp-replace #px"[\\s﻿]*$" str "")))
    str))

;;; Transform "Smith, John" to ("Smith" . "John").
(define (name-w/comma->pair name)
  (let ((p (map unpad (regexp-split "," name))))
    (cons (car p) (cadr p))))

;;; Transform "John Smith" to ("Smith" . "John")
(define (name->pair name)
  (let* ((name-elts (remove* '("") (regexp-split #px"\\s+" name)))
         (elts-rev  (reverse name-elts))
         (last      (car elts-rev))
         (rest      (reverse (cdr elts-rev)))
         (rest      (map (lambda (x) (string-append x " ")) rest))
         (rest      (apply string-append rest))
         (rest      (regexp-replace #px"\\s*$" rest "")))
    (cons last rest)))

(define (last-name name)  (car name))
(define (first-name name) (cdr name))
(define (build-name last first) (cons last first))

(define (name=? n1 n2)
  (and (string=? (last-name n1)  (last-name n2))
       (string=? (first-name n1) (first-name n2))))

(define (name<? n1 n2)
  (or (string<? (last-name n1) (last-name n2))
      (and (string=? (last-name n1)  (last-name n2))
           (string<? (first-name n1) (first-name n2)))))

(define (name>? n1 n2)
  (not (or (name=? n1 n2) (name<? n1 n2))))

;;; * roster
(define (process-roster in)
  (define (blank? line) (regexp-match? #px"^\\s*$" line))
  (define (part-time? line) (regexp-match? #px"Time:\\s*$" line))
  (define (clean name)   ;clean name
    (let* ((_ name)
           (_ (regexp-replace #px"\\s*\\(.*" _ ""))
           (_ (if (regexp-match #px"," _)
                  _
                  (regexp-replace #px" " _ ", "))))
      _))
  (let loop ((line (read-line in 'any))
             (ignoring #t)
             (_ '()))
    (if (eof-object? line)
        _
        (let ((next-line (read-line in 'any)))
          (cond
           ((and (not ignoring) (not (blank? line))) ;line is name
            (loop (read-line in 'any) #f (cons (clean line) _)))
           ((not ignoring)              ;blank so enter header
            (loop next-line #t _))
           (else                        ;ignoring
            (loop next-line
                  (and (not (blank? line))
                       (or (not (part-time? line))
                           (blank? next-line)))
                  _)))))))

;;; Return a regularizer function that takes an argument `name' of the
;;; form
;;;   (last . first)
;;; where `first' can be:
;;;   - a single string consisting of a first name (eg, "Allan")
;;;   - a single string consisting of multiple space-separated names
;;;     (eg, "Anna Belle")
;;;   - a single string consisting of initial(s) with or without
;;;     periods (eg, "AB" or "A.B." or "A.")
;;;   - missing altogether.
;;; and returns the corresponding name in the roster---or #f if it
;;; can't find one.
;;;
;;; Names can refer to teachers. For example if there is an entry in
;;; the roster of the form "Goldmann, Robert-sensei" the regularizer
;;; function will return that entry for "Goldmann-sensei" and
;;; "Robert-sensei".
(define (make-name-regularizer roster)
  (define (abbrev name n)
    (build-name
     (last-name name)
     (apply string-append
            (map (lambda (x) (substring x 0 (min n (string-length x))))
                 (regexp-split #px"\\s+" (first-name name))))))

  ;; Find a match for an ordinary participant.
  (define (add-abbrevs name)
    (let loop ((n 1) (r '()))
      (if (> n (string-length (first-name name)))
          r
          (loop (+ n 1) (cons (cons (abbrev name n) name) r)))))
  (define (abbrev-for name) (car name))
  (define (full-name name)  (cdr name))
  (define (abbrev=? name1 name2)
    (name=? name1 (abbrev-for name2)))
  (define (match-last roster-name candidate-name)
    (let* ((c-last (last-name candidate-name)) ;perhaps abbreviated
           (r-last (last-name roster-name))
           (r-last (substring r-last 0 (min (string-length r-last)
                                            (string-length c-last)))))
      (string=? c-last r-last)))
  (define (find-ordinary-match name)
    (let ((possibles (filter (lambda (x) (match-last x name)) roster)))
      (define (exact-match)
        (let ((r (member name possibles name=?)))
          (and r (car r))))
      (define (abbrev-match)
        (let ((possibles (apply append (map add-abbrevs possibles)))
              (name (build-name
                     (last-name name)
                     (regexp-replace*
                      #px"\\." (first-name name) ""))))
          (let ((r (member name possibles abbrev=?)))
            (and r (full-name (car r))))))
      (cond
       ((null? possibles) #f)
       ((= (length possibles) 1) (car possibles))
       ;; we have two or more hits
       ((exact-match))
       ((abbrev-match))
       (else #f))))

  ;; Find a match for a teacher.
  (define reg (pregexp "(?i:-(sensei|roshi))"))
  (define (teacher-type? name)
    (or (regexp-match? reg (last-name  name))
        (and (not (null? (first-name name)))
             (regexp-match? reg (first-name name)))))
  (define (clean-name name)
    (build-name
     (regexp-replace reg (last-name  name) "")
     (if (null? (first-name name))
         '()
         (regexp-replace reg (first-name name) ""))))
  ;; Arguments name and roster-name are already known to be teachers.
  (define (teacher-match? name roster-name)
    (let ((cleaned-name (clean-name name))
          (cleaned-roster-name (clean-name roster-name)))
      (or (and (not (null? (first-name cleaned-name)))
               (name=? cleaned-name cleaned-roster-name))
          (string=? (last-name cleaned-name)
                    (last-name cleaned-roster-name))
          (string=? (last-name cleaned-name)
                    (first-name cleaned-roster-name)))))
  (define (get-full-name teacher-name)
    (define (t-match? roster-teacher-name)
      (teacher-match? teacher-name roster-teacher-name))
    (let* ((full-names (filter teacher-type? roster))
           (full-names (filter t-match? full-names)))
      (and (not (null? full-names))
           (car full-names))))
  (define (find-teacher-match name)
    (and (teacher-type? name) (get-full-name name)))
  (lambda (name)
    (or (find-ordinary-match name)
        (find-teacher-match  name))))

(define (test-regularizer)
  (define regularize-name
    (make-name-regularizer '(("Leiserson" . "Anna Belle")
                             ("Leiserson" . "Allan")
                             ("Goldmann" . "Robert-sensei")
                             ("Kjolhede" . "Bodhin-roshi"))))
  (define (should-match n1 n2)
    (let ((r (regularize-name n1)))
      (cond
       ((not r)
        (display (format " *** could not regularize ~a~%" n1)))
       ((name=? r n2)
        (display (format "properly matched ~a with ~a~%" n1 n2)))
       (else 
        (display
         (format " *** failed to match ~a with ~a\n" n1 n2))))))
  (define (should-not-match n1 n2)
    (let ((r (regularize-name n1)))
      (cond
       ((not r)
        (display (format " *** could not regularize ~a~%" n1)))
       ((not (name=? r n2))
        (display
         (format "properly failed to match ~a with ~a~%" n1 n2)))
       (else
        (display
         (format " *** improperly matched ~a with ~a\n" n1 n2))))))
  (let ((abl   '("Leiserson" . "Anna Belle"))
        (al    '("Leiserson" . "Allan"))
        (abli  '("Leiserson" . "AB"))
        (abli2 '("Leiserson" . "A.B."))
        (ali   '("Leiserson" . "A."))
        (ali2  '("Leiserson" . "Al"))
        (rsg   '("Goldmann" . "Robert-sensei"))
        (rs    '("Robert-sensei"))
        (gs    '("Goldmann-sensei")))
    (should-match     abl   abl)
    (should-match     abli  abl)
    (should-match     abli2 abl)
    (should-match     ali   al)
    (should-match     ali2  al)
    (should-match     rsg   rsg)
    (should-match     rs    rsg)
    (should-match     gs    rsg)
    (should-not-match ali   abl)
    (should-not-match ali2  abl)
    (should-not-match abli  al)))

;;; * rooms
(define (process-rooms in)
  (define (participant? l) (regexp-match? #px"•.*\\S" l))
  (define (room-occupant l)
    (let* ((r (cadr (regexp-match #px"•(.*)" l)))
           (r (regexp-replace #px"\\(.*" r ""))
           (r (unpad r))
           (r (regularize-name (name->pair r)))
           (r (if r r '("Mysterioso" . "L'Angelo"))))
      r))
  (define (room-number l) (unpad l))
  (let loop ((l (read-line in 'any))
             (r (list))
             (current-room ""))
    (cond
     ((eof-object? l) r)
     ((participant? l)
      (loop (read-line in 'any)
            (cons (cons (room-occupant l) current-room) r)
            current-room))
     (else
      (loop (read-line in 'any) r (room-number l))))))

;;; * jobs
(define (collect-jobs lst)
  (let loop ((names (remove-duplicates (map car lst) name=?))
             (r (list)))
    (cond
     ((null? names) r)
     (else
      (let* ((current-name (car names))
             (jobs lst)
             (jobs (filter
                    (lambda (x) (name=? (car x) current-name))
                    jobs))
             (jobs (map cdr jobs)))
        (loop (cdr names)
              (cons (cons current-name jobs) r)))))))

(define (process-work in)
  (define (job-line? l) (regexp-match? #px"\t" l))
  ;; water table section
  ;; Assume that the water table stanza always comes at the very end
  ;; of the list of work assignments.
  (define (wt-line? l) (regexp-match? #px"[Ww]ater\\s+[Tt]able" l))
  (define (extract-job l default)
    (let* ((r (regexp-match #px"^([^\t]*)\t" l))
           (r (list-ref r 1))
           (r (unpad r))
           (r (if (string=? r "") default r)))
      r))
  (define (extract-name l)
    (let* ((r (regexp-match #px"\t([^(]*)\\(?.*$" l))
           (r (list-ref r 1))
           (r (unpad r))
           (r (if (string=? r "") "L'Angello Mysterioso" r))
           (r (regularize-name (name->pair r)))
           (r (if r r '("Mysterioso" . "L'Angelo"))))
      r))
  (let loop ((l (read-line in 'any))
             (r (list))
             (previous-job "")
             (water-table #f))
    (cond
     ((eof-object? l) (collect-jobs r))
     ((not (job-line? l))
      (loop (read-line in 'any) r previous-job (wt-line? l)))
     (else
      (let ((job (string-append
                  (if water-table "Water Table, " "")
                  (extract-job l previous-job)))
            (name (extract-name l)))
        (loop (read-line in 'any)
              (cons (cons name job) r)
              job
              water-table))))))

;;; * zendo duties
(define (process-zendo-duties in)
  (define (next-line)
    (let ((l (read-line in 'any)))
      (if (eof-object? l) l (regexp-replace #px"\\s*$" l ""))))
  (define (deleteable-line? l)
    (or (regexp-match? #px"^[\\s\uFEFF]*[Zz]endo\\s*[Dd]utie" l)
        (regexp-match? #px"^\\s*$" l)))
  (define (prefix-line? l) (not (regexp-match? #px"\t" l)))
  (define (booklet-line? l)
    (let ((r (regexp-match #px"\\(\\d+-\\d+\\)" l)))
      (and r (car r))))
  (define (multi-job-line? l)
    (and (not (booklet-line? l))
         (regexp-match
          #px"(^\\s*[^\t]+\t[^\t]+)(\t[^\t]+\t[^\t]+)" l)))
  (define (extract-job l default)
    (let ((b (booklet-line? l))
          (m (regexp-match #px"^\\s*([^\t]+)\t" l)))
      (cond
       (b (string-append "Booklets " b))
       (m (unpad (list-ref m 1)))
       (else default))))
  (define (extract-name l)
    (let* ((m (regexp-match #px"([^\t]+)$" l))
           (m (if m
                  (regularize-name
                   (name->pair
                    (unpad (list-ref m 1))))
                  m)))
      m))
  (define (update-prefix prefix line)
    (if (and (not (booklet-line? line))
             (regexp-match? #px"[Cc]hant.*ooklet" prefix))
        "" prefix))
  (define (job-title prefix job)
    (if (string=? prefix "") job (format "~a ~a" prefix job)))
  (let loop ((l (next-line))
             (r (list))
             (pre "")
             (previous-job ""))
    (cond
     ((eof-object? l) (collect-jobs r))
     ((deleteable-line? l)
      (loop (next-line) r pre previous-job))
     ((prefix-line? l)
      (loop (next-line) r (unpad l) previous-job))
     (else
      (let* ((pre (update-prefix pre l))
             (mj (multi-job-line? l))
             (l (if mj (list-ref mj 1) l))
             (current-job (extract-job l previous-job))
             (current-name (extract-name l)))
        (loop (if mj (list-ref mj 2) (next-line))
              (if (not current-name)
                  r
                  (cons (cons current-name
                              (job-title pre current-job))
                        r))
              pre
              current-job))))))

;;; * showers

;;; Convert the time of day expressed as a string to time of day in
;;; number of minutes. For example
;;;   "5:00 am" ==> 300
(define (minutes-of-day str)
  (let ((a (regexp-match #px"([\\d]+):([\\d]+).*(.m)" str)))
    (if (not a)
        0
        (let* ((h (string->number (list-ref a 1)))
               (h (if (= h 12) 0 h))
               (m (string->number (list-ref a 2)))
               (p (regexp-match? #px"p" (list-ref a 3))))
          (+ m (* 60 (+ h (if p 12 0))))))))

(define (minutes->tod minutes)
  (let* ((h (quotient minutes 60))
         (m (modulo minutes 60))
         (p (if (>= h 12) "pm" "am"))
         (h (if (>= h 13) (- h 12) h)))
    (format "~a:~a ~a" h (substring (format "~a" (+ 100 m)) 1) p)))

(define (scan-shower-table in)
  (define (clean str)
    (regexp-replace #px"\"" (regexp-replace #px"\r.*" str "") ""))
  (let loop ((l (read-line in 'linefeed))
             (r (list))
             (m 1000))
    (if (eof-object? l)
        (reverse r)
        (let* ((entries (regexp-split #px"\t" l))
               (entries (map clean entries))
               (entries (cons (minutes-of-day (car entries))
                              (cdr entries)))
               (mod (car entries)))
          (when (and (not (zero? mod)) (< mod m))
            (set! r (cons '(0 "Women Soak" "Women Soak"
                              "Men Soak" "Men Soak") r)))
          (loop (read-line in 'linefeed)
                (cons entries r)
                mod)))))

(define (process-showers in)
  (define (header-line? entries) (= (car entries) 0))
  (define (create-s-time person minutes shower)
    (cons person (cons (minutes->tod minutes) shower)))
  (define (good-name sname)
    (let* ((r (regularize-name (if (regexp-match? #px"," sname)
                                   (name-w/comma->pair sname)
                                   (list sname))))
           (r (if r r '("Mysterioso" . "L'Angelo"))))
      r))
  (define (finalize showers)
    (let loop ((s (apply append showers))
               (r (list)))
      (cond
       ((null? s) r)
       ((string=? (caar s) "") (loop (cdr s) r))
       (else (loop (cdr s)
                   (cons (cons (good-name (caar s))
                               (cdar s))
                         r))))))
  (let loop ((s (scan-shower-table in))
             (h (list))
             (r (list)))
    (cond
     ((null? s) (finalize r))
     ((header-line? (car s))
      (loop (cdr s) (cdar s) r))
     (else
      (loop (cdr s)
            h
            (cons (map (lambda (person shower)
                         (create-s-time person (caar s) shower))
                       (cdar s) h)
                  r))))))

(define (show-showers shower-assignments)
  (let loop ((sa shower-assignments))
    (when (not (null? sa))
      (display (car sa))
      (display "\n")
      (loop (cdr sa)))))
(define (shower-time  s) (car s))
(define (shower-place s) (cdr s))
;;; * summarize
(define (build-s-record name room shower jobs duties)
  (list name room shower jobs duties))
(define (s-name r)   (list-ref r 0))
(define (s-room r)   (list-ref r 1))
(define (s-shower r) (list-ref r 2))
(define (s-jobs r)   (list-ref r 3))
(define (s-duties r) (list-ref r 4))

(define (collect-fields
         roster room-assignments shower-assignments job-assignments
         zendo-duties)
  (define (find-entry name lst)
    (let* ((elt (cdar lst))
           (dummy (cond
                   ((string? elt) "")
                   ((list? elt) '())
                   ((pair? elt) '("" . ""))))
           (r (member name lst (lambda (x y) (name=? x (car y)))))
           (r (if r (cdar r) dummy)))
      r))
  (let loop ((roster roster)
             (r (list)))
    (cond
     ((null? roster) r)
     (else
      (let* ((person (car roster))
             (roster (cdr roster))
             (record (build-s-record
                      person
                      (find-entry person room-assignments)
                      (find-entry person shower-assignments)
                      (find-entry person job-assignments)
                      (find-entry person zendo-duties))))
        (loop roster (cons record r)))))))

;;; * publish
;;; ** table
(define (work r)
  (let* ((w (append (s-jobs r) (s-duties r))))
    (if (null? w) '("") w)))

(define (create-org-table s filename)

  (define (p out)
    (define (jobs-string r)
      (substring
       (apply string-append
              (map (lambda (x) (format "|||||~a|\n" x)) (work r)))
       5))
    (define (display-record r)
      (display
       (format
        "|~a, ~a|~a|~a|~a|~a"
        (last-name (s-name r)) (first-name (s-name r))
        (s-room r)
        (shower-place (s-shower r))
        (shower-time (s-shower r))
        (jobs-string r))
       out))
    (display "|-\n|name|room|shower|time|jobs/duties|\n|-\n" out)
    (let loop ((s (sort s name<? #:key s-name)))
      (when (not (null? s))
        (display-record (car s))
        (loop (cdr s))))
    (display "|-\n" out))
  
  (call-with-output-file filename p #:exists 'replace))

(define (sesshin-id)
  (match-let ([(list year month day)
               (regexp-split #px"-" *start-date*)])
    (format
     "~a ~a Sesshin"
     year
     (list-ref '("junk" "January" "February" "March" "April" "May"
                 "June" "July" "August" "September" "October"
                 "November" "December")
               (string->number month)))))

;;; I am on the lookout for a more general solution to this. Maybe
;;; there exists a latex package/environment that can handle big
;;; European alphabets.
(define (escape str [tr '((#px"&"         . "\\\\&")
                          (#px"\\s*–\\s*" . "---")
                          (#px"é"         . "\\\\'e")
                          (#px"ü"         . "\\\\\"u")
                          (#px"’"         . "'"))])
  (cond
   ((null? tr) str)
   (else
    (escape (regexp-replace* (caar tr) str (cdar tr)) (cdr tr)))))

(define (create-latex-table s filename)
  (define (p out)
    (define (jobs-string r)
      (let* ((j (map (lambda (x) (format " \\\\~%\\> ~a" (escape x)))
                     (work r)))
             (j (apply string-append j))
             (j (substring j 7))
             (j (format
                 (string-append
                  "~%\\begin{minipage}[t]{0in}\\begin{tabbing}~%"
                  "\\= ~a~%"
                  "\\end{tabbing}\\end{minipage} \\\\~%")
                 j)))
        j))
    (define (display-record r)
      (display
       (format
        "~a, ~a & ~a & ~a & ~a & ~a"
        (escape (last-name (s-name r)))
        (escape (first-name (s-name r)))
        (s-room r)
        (shower-place (s-shower r))
        (shower-time (s-shower r))
        (jobs-string r))
       out))
    (display (format "
      \\documentclass[10pt]{article}
      \\usepackage{charter, longtable}
      \\usepackage[table]{xcolor}
      \\hoffset -1.0in
      \\setlength{\\textwidth}{6.5in}
      \\setlength{\\textheight}{9in}
      \\definecolor{Odd}{gray}{0.7}
      \\definecolor{Even}{gray}{1.0}
      \\begin{document}

      \\pagestyle{myheadings}
      \\markboth{~a}{~a}

      \\rowcolors{2}{Odd}{Even}
      \\begin{longtable}{lllrl}
       & room & shower & time & jobs/duties \\\\ \\hline
       \\endhead
       \\multicolumn{5}{r}{\\small\\slshape continued on next page} \\
       \\endfoot
       \\endlastfoot
      " (sesshin-id) (sesshin-id)) out)
    (let loop ((s (sort s name<? #:key s-name)))
      (when (not (null? s))
        (display-record (car s))
        (loop (cdr s))))
    (display "\\end{longtable}\n\\end{document}\n" out))

  (call-with-output-file filename p #:exists 'replace))

;;; ** slips
(define pages-per-cut  3)
(define n-per-page     5)
(define n-per-cut      (* n-per-page pages-per-cut))
(define (generate-dummies d [r '()])
  (if (zero? d)
      r
      (generate-dummies
       (- d 1)
       ;;                     name room shower jobs zendo
       (cons (build-s-record '("" . "") "" '("" . "") '() '()) r))))
(define (neaten-slips s)
  (let ((n (length s)))
    (append s (generate-dummies
               (- (* n-per-page (ceiling (/ n n-per-page))) n)))))
(define (sort-slips s)
  (define (index->order k n-total)
    (define cuts-total     (ceiling (/ n-total n-per-cut)))
    (define n-in-final-cut (- n-total (* n-per-cut (- cuts-total 1))))
    (let* ((pages-per-cut
            (if (= cuts-total (+ 1 (quotient k n-per-cut)))
                (+ 1 (quotient (- n-in-final-cut 1) n-per-page))
                pages-per-cut))
           (s (* n-per-cut (quotient k n-per-cut)))
           (k (- k s)))
      (let-values ([(xn xp) (quotient/remainder k pages-per-cut)])
        (+ xn (* xp n-per-page) s))))
  (define (add-order s [n-total (length s)])
    (let loop ((k 0) (s s) (r '()))
      (if (null? s)
          r
          (loop (+ k 1) (cdr s)
                (cons
                 (cons (index->order k n-total) (car s))
                 r)))))
  (let* ((s (sort s name<? #:key s-name))
         (s (neaten-slips s))
         (s (add-order s))
         (s (sort s < #:key car))
         (s (map cdr s)))
    s))

(define (create-latex-slips s tex-file)
  ;; jobs->2cols takes a list of jobs---as strings---and returns a
  ;; list of rows for the jobs table. Each row has two entries.
  (define (jobs->2cols jobs)
    (let* ((n  (length jobs))
           (n1 (if (<= n 6)
                   (min n 3)
                   (ceiling (/ n 2))))
           (jobs (map escape jobs))
           (col1 (take jobs n1))
           (col2 (drop jobs n1))
           (col2 (append col2 (build-list (- (* 2 n1) n)
                                          (lambda (n) (string))))))
      (map list col1 col2)))
  (define (latex out)
    ;; Argument p is the name, room and so on for a participant.
    (define (info->tex p)
      (let ((shower-time (car (s-shower p)))
            (shower      (cdr (s-shower p)))
            (jobs        (append (s-jobs p) (s-duties p))))
        (display (format "
          \\slip\\begin{minipage}[t]{\\swidth}
          ~a, ~a

          \\begin{tabular*}{\\swidth}[t]{lllll}
          room: ~a && shower: ~a && shower time: ~a \\\\
          \\end{tabular*}

          \\begin{tabular*}{\\swidth}[t]{lll}
          \\\\
          jobs: \\\\~%"
          (escape (last-name (s-name p)))
          (escape (first-name (s-name p)))
          (s-room p) shower shower-time) out)
        (for-each
         (lambda (r) (display (format "~a && ~a \\\\~%"
                                 (list-ref r 0)
                                 (list-ref r 1)) out))
         (jobs->2cols jobs))
        (display "
          \\end{tabular*}\\end{minipage}
          \\noindent\\makebox[\\linewidth]{\\rule{15in}{.02in}}"
                 out)))
    (display "
      \\documentclass[12pt]{article}
      \\setlength{\\textheight}{11in}
      \\voffset -.5in
      \\hoffset -1.0in
      \\newcommand{\\swidth}{7.5in}
      \\newcommand{\\slip}{
        \\begin{minipage}[t]{0in}\\vspace{2in}\\end{minipage}
        \\vspace{-.23in}}
      \\begin{document}
      \\noindent" out)
    (for-each info->tex (sort-slips s))
    (display "\\end{document}\n" out))

  (call-with-output-file tex-file latex #:exists 'replace))

;;; * debug
(set! *start-date* "2019-11-02")
(define roster (map name-w/comma->pair
                    (read-doc #px"[Rr]oster" process-roster)))
(set! regularize-name (make-name-regularizer roster))
(define x (read-doc #px"[Ss]hower" process-showers #:table #t))

;;; * main

(define (main start-date)
  (define (pdflatex tex-file)
    (system (format "pdflatex ~a;pdflatex ~a" tex-file tex-file)))
  (set! *start-date* start-date)
  (define roster (map name-w/comma->pair
                      (read-doc #px"[Rr]oster" process-roster)))
  (set! regularize-name (make-name-regularizer roster))
  (let* ((room-assignments (read-doc #px"[Rr]oom" process-rooms))
         (job-assignments (read-doc #px"[Ww]ork" process-work))
         (zendo-duties (read-doc #px"[Zz]endo\\s*[Dd]uties"
                                 process-zendo-duties))
         (shower-assignments (read-doc #px"[Ss]hower"
                                       process-showers #:table #t))
         (this-sesshin
          (collect-fields roster room-assignments shower-assignments
                          job-assignments zendo-duties))
         (tex-tab (format "~a/~a-sessh.tex" *tmp-dir* *start-date*))
         (tex-slp (format "~a/~a-slips.tex" *tmp-dir* *start-date*)))
    (create-latex-table this-sesshin tex-tab)
    (create-latex-slips this-sesshin tex-slp)
    (pdflatex tex-tab)
    (pdflatex tex-slp)))
