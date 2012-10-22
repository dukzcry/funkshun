; tested with CHICKEN Version 4.8.0

;(use srfi-1) ; filter, iota
(use srfi-27) ; random-real

(define fold-left fold)
(define ndisplay (
 lambda pack (display pack) (newline)
))