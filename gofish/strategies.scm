#| 1. take into account ranks the player asked for +
 (1.1.) note number of asks for each +
(2.) ask ranks for which you have most cards at first +
3. don't ask for same card few times, if player didn't take from deck
 (4.) periodically ask for cards the player is aware you have
 (4.1.) s/periodically/count probability/
(5.) pick random strategy at start, change if loosing |#

; transparent, always returns rank
(define (first-max pile)
 ; (r . c) -> (c . r) for assoc
 (define get-acc-rank cdr)
 (define get-count car)
 (define (fold-left-custom proc acc list) (
  let ((rank (get-acc-rank (car acc))))
   (if (eq? (get-count (car acc)) 'max)
    rank
   (if (null? list)
      (let ((res (assoc 2 acc ;(reverse acc)
        )))
       (if res (get-acc-rank res)
         rank
      ))
      (fold-left-custom proc (proc (car list) acc) (cdr list)))
 )))
 (fold-left-custom (lambda (pair acc) (
  let ((rank (get-acc-rank (car acc)))
       (count (get-count (car acc))))
   (if (:rank-eq? rank pair)
    (if (eq? count 2)
     (cons (cons 'max rank) '())
    (cons
     (cons (+ count 1) rank)
     (cdr acc)))
    (cons
     (cons 1 (get-rank pair)) acc))
  ))
 '((())) (sort-pile pile))
)