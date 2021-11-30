#lang racket


(require test-engine/racket-tests)




(define-struct graph (name nodes) #:transparent #:mutable)


(define-struct node (name edges) #:transparent #:mutable)





(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


(define-syntax my-eval
  (syntax-rules ()
    [(my-eval arg)
     (eval arg ns)]))






(define x 3)
(define y 5)
(set! y 5)









(define z (list y))

(set! y 8)










(define z2 (list (quote y)))
(set! y 11)








(test)


(define n0 0)
(set! n0 (make-node (quote n0) empty))
(define n00 0)
(set! n00 (make-node (quote n00) (cons (quote n0) empty)))
(define n01 0)
(set! n01 (make-node (quote n01) empty))
(define n02 0)
(set! n02 (make-node (quote n02) empty))
(define nb1 (make-node (quote nb1) (cons (quote nb2) empty)))
(define nb2 (make-node (quote nb2) (cons (quote nb1) empty)))

(define ne0 (make-node (quote ne0) (cons (quote n00) (cons (quote n01) empty))))




(define g0 (make-graph (quote g0) empty))
(define g1 (make-graph (quote g1) (cons (quote n0) empty)))



(define-syntax create
  (syntax-rules ()
    [(create x e)
     (begin (define x e)
            (set! x e))]))


(check-expect (add-unique (quote n0) g0) (cons (quote n0) empty))
(check-expect (add-unique (quote n0) g1) (cons (quote n0) empty))

(define (add-unique n g)
  (if (false? (member n (graph-nodes g)))
      (cons n (graph-nodes g))
      (graph-nodes g)))

(check-expect (add-unique-edges (quote n00) n0) (cons (quote n00) empty))
(check-expect (add-unique-edges (quote n0) n00) (cons (quote n0) empty))

(define (add-unique-edges n2 n1)
  (if (false? (member n2 (node-edges n1)))
      (cons n2 (node-edges n1))
      (node-edges n1)))


(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph g)
     (begin
       (define g (make-graph (quote g) empty))
       (set! g (make-graph (quote g) empty)))]
    [(new vertex n in g)
     (begin
       (set! g (make-graph (graph-name g)
                           (add-unique (quote n) g))))]
    ))

(define-syntax edge
  (syntax-rules ()
    [(edge n1 n2)
     (set! n1 (make-node (node-name n1) (add-unique-edges (quote n2) n1)))]
    ))


(define-syntax edges
  (syntax-rules (<- <-> ->)
    [(edges n1 <- n2)
     (edge n1 n2)]
    [(edges n1 <-> n2)
     (begin (edge n1 n2)
            (edge n2 n1))]
    [(edges n1 -> n2)
     (edge n2 n1)]
    [(edges n1 op n2 op2 n3 ...)
     (begin (edges n1 op n2)
            (edges n2 op2 n3 ...))]
    ))

(define-syntax does
  (syntax-rules (have an edge from ? a bidirectional path)
    [(does n1 have an edge from n2 ?)
     (if (member (quote n1) (node-edges n2))
         true
         false)]
    [(does n1 have a bidirectional edge from n2 ?)
     (and (does n1 have an edge from n2 ?)
          (does n2 have an edge from n1 ?))]
    [(does n1 have a path from n2 ?) 
     (path-helper (quote n1) n2)]
    ))



(check-expect (path-helper (quote n00) n0) false)
(check-expect (path-helper (quote n0) n00) true)
(check-expect (path-helper (quote n0) ne0) true)




(define (path-helper n1 n2)
  (local [
          (define (path-helper-inner n)
            (cond
              [(empty? (node-edges (my-eval n))) false]
              [else (if (not (false? (member n1 (node-edges (my-eval n)))))
                        true
                        (ormap (λ (n3) (path-helper-inner n3)) (node-edges (my-eval n)))
                        )]
              ))]
    (path-helper-inner n2)))
