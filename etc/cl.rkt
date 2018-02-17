#lang racket

(require srfi/54 racket/draw)

(define names
  '("Smith, Joe" "Snodgrass, Fabian" "Zuckerstein, Vladimir"))

(define (doit [font-size 16] [font-face "Courier"])
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
  (define x-end  580)
  (define (gray-line x y w h)
    (let ((b (send dc get-brush))
          (p (send dc get-pen)))
      (send dc set-brush gray-brush)
      (send dc set-pen   clear-pen)
      (send dc draw-rectangle x y w h)
      (send dc set-brush b)
      (send dc set-pen p)))
  (define (name-entry name y)           ;top left
    (define-values (w h d a) (send dc get-text-extent name))    
    (gray-line x0 y (- x-end x0) h)
    (send dc draw-text name x-name y))
  (send* dc
    (start-doc "useless string")
    (set-font font)
    (start-page))
  (name-entry "Foobary" 50)
  (send* dc
    (end-page)
    (end-doc)))
