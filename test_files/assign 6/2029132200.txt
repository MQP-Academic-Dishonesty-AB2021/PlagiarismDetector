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










(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph g)
     (define g (make-graph (quote g) empty))]
    [(new vertex n in g)
     (begin (define n (make-node (quote n) empty))
            (add-unique (quote n) g)
            )]))



(define (add-unique n g)
  (set-graph-nodes! g
                    (if (member n (graph-nodes g))
                        (graph-nodes g)
                        (cons n
                              (graph-nodes g)))))




(define-syntax edge
  (syntax-rules ()
    [(edge n1 n2)
     (set-node-edges! n1
                      (if (member (quote n2) (node-edges n1))
                          (node-edges n1)
                          (cons (quote n2)
                                (node-edges n1))))]))






(define-syntax edges
  (syntax-rules (<- -> <->)
    [(edges n1 <- n2)
     (edge n2 n1)]
    [(edges n1 -> n2)
     (edge n1 n2)]
    [(edges n1 <-> n2)
     (begin (edge n1 n2)
            (edge n2 n1))]
    [(edges n1 dir n2 dir2 ...)
     (begin (edges n1 dir n2)
            (edges n2 dir2 ...))]))









(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(edges n0 -> n1 -> n2 <-> n3)


(check-expect (does n0 have an edge from n0 ?) false)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n1 have an edge from n0 ?) true)


(check-expect (does n1 have a bidirectional edge from n0 ?) false)
(check-expect (does n3 have a bidirectional edge from n0 ?) false)
(check-expect (does n3 have a bidirectional edge from n2 ?) true)
(check-expect (does n2 have a bidirectional edge from n3 ?) true)


 (check-expect (does n0 have a path from n3 ?) false)
(check-expect (does n2 have a path from n0 ?) true)
(check-expect (does n1 have a path from n0 ?) true)

(define-syntax does
  (syntax-rules (have a an edge bidirectional path from ?)
    [(does n2 have an edge from n1 ?)
     (not (false? (member (quote n2) (node-edges n1))))]
    [(does n2 have a bidirectional edge from n1 ?)
     (not (false? (and (member (quote n2) (node-edges n1))
                       (member (quote n1) (node-edges n2)))))]
    [(does n2 have a path from n1 ?)
     (nodePath? n1 n2)]))

 







(define (nodePath? n nn)
  
  
  (local [(define (fn-for-node n todo visited) 
            (cond [(member (node-name nn) (node-edges (my-eval n))) true]
                  [(member (node-name n) visited)
                   (fn-for-lon todo visited)]
                  [else
                   (fn-for-lon (append (node-edges (my-eval n)) todo)
                               (cons (node-name n) visited))])) 
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (my-eval (car todo))
                                (cdr todo)
                                visited)]))]
    (fn-for-node n empty empty)))


(define x 3)
(define y 5)
(set! y 5)









(define z (list y))

(set! y 8)










(define z2 (list (quote y)))
(set! y 11)








(test)