(define random-integer random)
(define fold-left fold)
(define ndisplay (
	lambda curry (display curry) (newline)
))

;(use srfi-1) ; filter, iota
(use srfi-27) ; random-real