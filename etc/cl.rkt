#lang racket

(require srfi/54 racket/draw)

(define names0
  '("Smmmith, Joe" "Snodgrass, Fabian" "Zuckerstein, Vladimir"
    "Cleopatra Pepperday" "Fred Flintstone"))

(define names names0)
(set! names (append names names0))
(set! names (append names names0))
(set! names (append names names0))
(set! names (append names names0))
(set! names (append names names0))
(set! names (append names names0))
(set! names (append names names0))
(set! names (append names names0))
(set! names (append names names0))
(set! names (append names names0))
(set! names (append names names0))

(define cl-columns
  '("Dues" "Sess" "Mon Mtg" "Seat Chg" "Kit" "Out door" "HK" "Zend"
    "Tea Cer" "Din Rm" "Rm Fan" "Part Time"))

(define (doit [font-size 10] [font-face "Courier"])
  (define font (make-font #:size font-size
                          #:size-in-pixels? #t
                          #:face font-face))
  (define gray-brush  (new brush% [color "gray"]))
  (define clear-pen   (new pen%   [style 'transparent]))
  (define dc
    (new pdf-dc%
         [interactive #f]
         [use-paper-bbox #f]
         [width  610]
         [height 756]
         [output "/tmp/cl.pdf"]
         [as-eps #f]))
  (define y0      10)
  (define x0       0)
  (define x-name  15)
  (define x-end  605)
  (define (line-height)
    (let-values ([(w h d a) (send dc get-text-extent "Aby")])
      h))
  (define (x-start n [pad 0]) (- (+ 210 (* 33 n)) pad))
  (define (print-header)
    (define (print-header-line columns y [n 0])
      (when (not (null? columns))
        (send dc draw-text (car columns) (x-start n) y)
        (print-header-line (cdr columns) y (+ n 1))))
    (define (split-entry entry)
      (let ((l (regexp-split #px" " entry)))
        (if (= (length l) 2) l (list (car l) ""))))
    (let ((lol (map split-entry cl-columns)))
      (print-header-line (map car  lol) y0)
      (print-header-line (map cadr lol) (+ y0 (* .8 (line-height))))))
  (define (print-rules)
    (define y1 (+ y0 (* (line-height) (+ (length names) 2))))
    (let ((x (- x-name 2)))
      (send dc draw-line x y0 x y1))
    (let loop ((c cl-columns)
               (n 0))
      (when (not (null? c))
        (send dc draw-line (x-start n 2) y0 (x-start n 2) y1)
        (loop (cdr c) (+ n 1)))))
  ;; The line number is n. Even lines are gray.
  (define (print-entry n name y)
    (let ((old-pen   (send dc get-pen))
          (old-brush (send dc get-brush)))
      (send dc set-pen   clear-pen)
      (send dc set-brush (if (zero? (modulo n 2))
                             gray-brush
                             old-brush))
      (send dc draw-rectangle x0 y (- x-end x0) (line-height))
      (send dc set-pen   old-pen)
      (send dc set-brush old-brush)
      (send dc draw-text name x-name y)))
  (define (print-entries lon [n 0])
    (when (not (null? lon))
      (print-entry n (car lon) (+ y0 (* (+ 2 n) (line-height))))
      (print-entries (cdr lon) (+ n 1))))
  (send* dc
    (start-doc "useless string")
    (set-font font)
    (start-page))
  (print-header)
  (print-entries names)
  (print-rules)
  (send* dc
    (end-page)
    (end-doc)))
