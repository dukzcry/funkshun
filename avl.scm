; implementation-independent AVL tree
; copy-paste from http://swizard.info/articles/functional-data-structures.html#link12

; srfi instead of non-standard (define-record)
(use srfi-9) ; record types

(define-record-type avl-tree (make-avl-tree root less? equ?) avl-tree?
 (root avl-root)
 (less? avl-less)
 (equ? avl-equ)
)
(define-record-type avl-node (make-avl-node key value l-child r-child l-depth r-depth) 
 avl-node?
 (key avl-key)
 (value avl-value)
 (l-child avl-l-child) (r-child avl-r-child)
 (l-depth avl-l-depth) (r-depth avl-r-depth)
)
(define (with-avl-tree tree func) (
 func (avl-root tree) (avl-less tree) (avl-equ tree)
))
(define (with-avl-node node func) (
 func (avl-key node) (avl-value node) (avl-l-child node) (avl-r-child node) 
 (avl-l-depth node) (avl-r-depth node)
))

(define (make-empty-avl-tree less? equ?)
 (make-avl-tree '() less? equ?))
(define (:make-left node child depth)
 (with-avl-node node
                 (lambda (k v lc rc ld rd) (make-avl-node k v child rc depth rd))))
(define (:make-right node child depth)
  (with-avl-node node
                 (lambda (k v lc rc ld rd) (make-avl-node k v lc child ld depth))))
(define (calc-depth node)
  (with-avl-node node (lambda (k v lc rc ld rd) (+ (max ld rd) 1))))
(define (:avl-subtree-insert-node make-proc child node . args)
  (let ((new-child (apply :avl-tree-insert-node child args)))
    ; add new child, compose subtree
    (make-proc node new-child (calc-depth new-child))))

(define :rotate-right-set (list :make-right
                                avl-l-child
                                :make-left
                                avl-r-child
                                avl-r-depth))
(define :rotate-left-set  (list :make-left
                                avl-r-child
                                :make-right
                                avl-l-child
                                avl-l-depth))
(define (:avl-tree-rotate node make-a get-child-a make-b get-child-b get-depth-b)
  (let* ((child-a (get-child-a node))
         (child-b (get-child-b child-a))
         (depth-b (get-depth-b child-a)))
    ; new-child = foredad -> child ~ dad dir
    ; new tree = dad -> new-child, modify dad depth
    (let ((new-node (make-b node child-b depth-b)))
      (make-a child-a new-node (calc-depth new-node)))))
(define (avl-tree-check-rotate node)
  (with-avl-node node
                 (lambda (k v lc rc ld rd) (cond ((and (> ld rd)
                             (> (- ld rd) 1))
                        (apply :avl-tree-rotate node :rotate-right-set))
                       ((and (> rd ld)
                             (> (- rd ld) 1))
                        (apply :avl-tree-rotate node :rotate-left-set))
                       (else node)))))

(define (:avl-tree-insert-node node ckey cvalue less?)
 (if (not (avl-node? node))
  (make-avl-node ckey cvalue '() '() 0 0)
   (avl-tree-check-rotate
    (let ((args (list node ckey cvalue less?)))
        (with-avl-node node
                       (lambda (k v lc rc ld rd) (if (less? ckey k)
                           (apply :avl-subtree-insert-node
                                  :make-left lc args)
                           (apply :avl-subtree-insert-node
                                  :make-right rc args))))))))
(define (avl-tree-insert tree key value)
  (with-avl-tree tree
                 (lambda (r l e) (make-avl-tree
                  (:avl-tree-insert-node r key value l)
                  l
                  e))))
(define (avl-tree-lookup tree ckey) (
 let ((equ? (avl-equ tree))
      (less? (avl-less tree)))
 (define (lookup node) (
   if (not (avl-node? node))
    #f
    (with-avl-node node (lambda (k v lc rc ld rd) (
     cond ((equ? ckey k) v)
          ((less? ckey k) (lookup lc))
          (else (lookup rc)))))
 ))
 (lookup (avl-root tree))
))