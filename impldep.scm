; tested with CHICKEN Version 4.7.0.6

;(use srfi-1) ; filter, iota
(use srfi-27) ; random-real

(define fold-left fold)
(define ndisplay (
	lambda curry (display curry) (newline)
))