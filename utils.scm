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
(define (send rank pile) (
	filter (lambda (x) (eq? rank (get-rank x))) pile
))
; (recv (pop) deck) or (recv books pile)
(define (recv books pile) (append pile books))
(define (draw pile)
	(define (sort-filter p less) (sort (filter (lambda (x) (p (get-rank x))) pile) less))
	(append (sort-filter number? (lambda (x y) (< (get-rank x) (get-rank y))))
		(sort-filter symbol? (lambda (x y) (string<? (symbol->string (get-rank x)) 
			(symbol->string (get-rank y))))))
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
	(define (shuffle lst) (
		map cdr
		(sort (map (lambda (x) (cons (random-real 1.0) x)) lst)
			(lambda (x y) (< (car x) (car y)))
	)))
	(shuffle (append-piles))
)
(define (pop) (
	if (null? deck)
		'()
	(let ((top (car deck)))
		(set! deck (cdr deck))
		`(,top)
)))