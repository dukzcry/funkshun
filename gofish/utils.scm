(define (who-starts?) 
 (if (zero? ((lambda (num) (random-integer num)) 2)
  ) 'computer
    'player
 )
)
(define print-winner (
 lambda pack (ndisplay (cond ((apply > pack) 'computer) 
  ((apply < pack) 'player)
  ;(apply = pack) 'both)
  ) "won")
))


(define (get-rank a) (car a))
(define (get-suit a) (cdr a))
(define (:rank-eq? rank lst) (eq? rank (get-rank lst)))
(define (any-cards-of? rank pile)
 (cond ((null? pile) #f) 
  ((:rank-eq? rank (car pile)) #t)
  (else (any-cards-of? rank (cdr pile)))
 )
)
(define send assoc-pile)
; asked rank in first, other in rest
(define (assoc-pile rank pile)
 (define rule `(eq? ',rank (get-rank x)))
 (define (filter-pile func) (eval `(filter (lambda (x) (,@func)) ',pile)))
 (cons (filter-pile rule) (filter-pile `(not ,rule)))
)
(define (sort-pile pile)
 (define (sort-filter p less) (sort (filter (lambda (x) (p (get-rank x))) pile) less))
  (append (sort-filter number? (lambda (x y) (< (get-rank x) (get-rank y))))
  (sort-filter symbol? (lambda (x y) (string<? (symbol->string (get-rank x)) 
   (symbol->string (get-rank y))))))
)
(define (book-ready? rank pile) (
 if (eq? (fold-left (lambda (x count) (if (:rank-eq? rank x) (+ count 1) count)) 0 pile)
     4) #t #f
))

(define +:ranks+ (append (iota 9 2) '(J Q K A)))
(define (make-deck)
 (define (make-card rank suit) (cons rank suit))
 (define (pile suit) (map (lambda (x) (make-card x suit)) +:ranks+))
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
; mutator
(define (pop) (
 if (null? *deck*)
  '()
 (let ((top (car *deck*)))
  (set! *deck* (cdr *deck*))
  `(,top)
)))

; (recv (pop) pile) or (recv cards pile)
(define (recv lst pile) (append pile lst))
(define (init-piles computer-func user-func)
 (define (init-pile n f s)
  (if (> n 7)
   (cons f s)
   (init-pile (+ n 1) (computer-func f) (user-func s))
 ))
 (init-pile 1 '() '())
)


(define (:rank->string rank) (
 if (number? rank)
  (number->string rank)
  (symbol->string rank)
))
(define (:string->rank string) (
 let ((num (string->number string)))
  (if num
  num
  (string->symbol string))
))
(define (tree-insert tree key)
  (avl-tree-replace tree (:rank->string key) 
   (lambda (x) (if (null? x) 1
    (+ x 1)
   ))
))
(define (tree-remove tree key)
 avl-tree-remove-key tree (:rank->string key))
(define (tree-find tree key) (
 :string->rank (avl-tree-lookup tree (:rank->string key))
))
(define (tree-max-val tree) (
  :string->rank (avl-tree-max-value tree)
))