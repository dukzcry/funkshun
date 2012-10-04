(define (who-starts?) 
	(if (zero? ((lambda (num) (random num)) 2)
		) 'computer 
		' player
	)
)

(define (make-card val suit) (cons val suit))
(define (value? a) (car a))
(define (suit? a) (cdr a))