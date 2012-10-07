(define (who-starts?) 
	(if (zero? ((lambda (num) (random-integer num)) 2)
		) 'computer 
		' player
	)
)


(define (get-suit a) (car a))
(define (get-value a) (cdr a))


(define values (append (iota 10 2) '(J Q K A)))

(define (make-stack)
	(define (make-card suit rank) (cons suit rank))
	(define (pile suit) (map (lambda (x) (make-card suit x)) values))
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

(define (pop)
	(if (null? stack)
		'()
	(let ((res (car stack)))
		(set! stack (cdr stack))
		res)
)