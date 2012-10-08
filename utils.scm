(define (who-starts?) 
	(if (zero? ((lambda (num) (random-integer num)) 2)
		) 'computer 
		' player
	)
)


(define (get-rank a) (car a))
(define (get-suit a) (cdr a))
(define (any-cards-of? rank lst)
	(cond ((null? lst) #f) 
		((eq? rank (car (get-rank lst))) #t)
		(else (any-cards-of? rank (cdr lst)))
	)
)

(define ranks (append (iota 9 2) '(J Q K A)))
(define (make-deck)
	(define (make-card rank suit) (cons rank suit))
	(define (pile suit) (map (lambda (x) (make-card x suit)) ranks))
	(define (append-piles)
		(define (append-pile lst suits)
			(if (null? suits)
				lst
				(append lst (pile (car suits)) (append-pile lst (cdr suits)))
		))
		(append-pile '() '(♠ ♣ ♥ ♦))
	)
	(shuffle append-piles)
)
(define (pop) (
	if (null? deck)
		'()
	(let ((top (car deck)))
		(set! deck (cdr deck))
		top)
))