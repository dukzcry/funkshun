(define deck '())
(define (set-deck!) (set! deck (make-deck)))

(define (game)
	(set-deck!)
	(define computer-pile '())
	(define user-pile '())
	(define computer-score '())
	(define user-score '())
	(let ((piles (init-piles computer-pile user-pile pop pop)))
		(set! computer-pile (car piles))
		(set! user-pile (cdr piles))
	)
	(draw user-pile)
	(let ((who (who-starts?)))
		(display `(,who "starts first")) (newline)
	)
	(print-winner computer-score user-score)
(game))