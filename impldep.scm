(define (shuffle lst) (
	map cdr
    (sort (lambda (x y) (< (car x) (car y)))
    	(map (lambda (x) (cons (random 1.0) x)) lst))
))