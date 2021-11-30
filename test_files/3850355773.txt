

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















(define-syntax create
  (syntax-rules (graph vertex in)
    [(create graph e1)
     (begin
       (define e1 (make-graph (quote e1) empty))
       (set-graph-name! e1 (quote e1)))]
    [(create vertex e2)
     (begin
       (define e2 (make-node (quote e2) empty))
       (set-node-name! e2 (quote e2)))]))


(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph e1)
     (create graph e1)]
    [(new vertex e2 in e1)
     (begin
       (create vertex e2)
       (define add-node (cons (node-name e2) (graph-nodes e1)))
       (set-graph-nodes! e1 add-node))]))




(define-syntax edge
  (syntax-rules ()
    [(edge e1 e2)
     (begin
       (define add-edge (cons (node-name e2) (node-edges e1)))
       (set-node-edges! e1 add-edge))]))


(define-syntax edges 
  (syntax-rules (-> <- <->) 
    [(edges e1 -> e2)
     (edge e1 e2)]
    [(edges e1 <- e2)
     (edge e2 e1)]
    [(edges e1 <-> e2)
     (begin (edge e2 e1)
            (edge e1 e2))]
    [(edges e1 op1 e2 op2 ...)
     (begin
       (edges e1 op1 e2)
       (edges e2 op2 ...))]))




(define-syntax does
  (syntax-rules (have an a edge bidirectional path ?)
    [(does e1 have an edge from e2 ?)
     (not (false? (member (node-name e1) (node-edges e2))))]
    [(does e1 have a bidirectional edge from e2 ?)
     (not (false? (and (member (node-name e1) (node-edges e2))
                       (member (node-name e2) (node-edges e1)))))]
    [(does e1 have a path from e2 ?)
     (path? e1 e2)]))

(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)
(edges n0 <- n1 <-> n2 -> n9)

(check-expect (does n0 have an edge from n1 ?) true)
(check-expect (does n9 have an edge from n2 ?) true)
(check-expect (does n1 have an edge from n0 ?) false)
(check-expect (does n2 have a bidirectional edge from n9 ?) false)
(check-expect (does n1 have a bidirectional edge from n2 ?) true)





(check-expect (does n0 have a path from n2 ?) true)
(check-expect (does n2 have a path from n1 ?) true)
(check-expect (does n9 have a path from n0 ?) false)
(check-expect (does n9 have a path from n2 ?) true)

(define (path? n1 n2)
  (local [(define (fn-for-node n todo visited)
            (if (member (node-name (my-eval n)) visited)
                (fn-for-lon todo visited)
                (fn-for-lon (append (node-edges (my-eval n)) todo)
                        (cons (node-name (my-eval n)) visited))))
          (define (fn-for-lon todo visited)
            (cond
              [(not (false? (member (node-name n1) visited))) true]
              [(empty? todo) false]
              [else
               (fn-for-node (first todo)
                            (rest todo)
                            visited)]))]
    (fn-for-node n2 empty empty)))



      

(test)