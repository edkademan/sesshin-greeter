#lang racket

(require srfi/54 racket/draw)

(define names
  '("Smith, Joe" "Snodgrass, Fabian" "Zuckerstein, Vladimir"
    "Cleopatra Pepperday" "Fred Flintstone"))

(define (doit [font-size 12] [font-face "Courier"])
  (define font (make-font #:size font-size
                          #:size-in-pixels? #t
                          #:face font-face))
  (define gray-brush  (new brush% [color "gray"]))
  (define clear-brush (new brush% [style 'transparent]))
  (define black-pen   (new pen%   [color "black"]))
  (define clear-pen   (new pen%   [style 'transparent]))
  (define dc
    (new pdf-dc%
         [interactive #f]
         [use-paper-bbox #f]
         [width 592]
         [height 756]
         [output "/tmp/cl.pdf"]
         [as-eps #f]))
  (define x0       0)
  (define x-name  20)
  (define x-end  610)
  (define (line-height)
    (let-values ([(w h d a) (send dc get-text-extent "Aby")])
      h))
  ;; The line number is n. Odd lines are gray.
  (define (print-entry n name y)
    (let* ((old-pen   (send dc get-pen))
           (old-brush (send dc get-brush)))
      (send dc set-pen   clear-pen)
      (send dc set-brush (if (= (modulo n 2) 1)
                             gray-brush
                             clear-brush))
      (send dc draw-rectangle x0 y (- x-end x0) (line-height))
      (send dc set-pen   old-pen)
      (send dc set-brush old-brush)
      (send dc draw-text name x-name y)))
  (define (print-entries n lon)
    (when (not (null? lon))
      (print-entry n (car lon) (+ 30 (* n (line-height))))
      (print-entries (+ n 1) (cdr lon))))
  (send* dc
    (start-doc "useless string")
    (set-font font)
    (start-page))
  (print-entries 0 names)
  (send* dc
    (end-page)
    (end-doc)))
