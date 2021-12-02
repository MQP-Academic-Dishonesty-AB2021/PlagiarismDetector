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
    [(new graph gname)
     (begin
       (define gname (make-graph (quote gname) empty))
       (create gname gname))]
    [(new vertex vertex1 in graph1)
     (begin
       (define vertex1 (make-node (quote vertex1) empty))
       (create vertex1 vertex1)
       (set-graph-nodes! graph1 (add-unique vertex1 (graph-nodes graph1))))]))


(define-syntax create
  (syntax-rules ()
    [(create oldvar newvar)
     (set! oldvar newvar)]))




(check-expect (add-unique (make-node (quote n1) empty)
                          empty) 
              (cons (node-name (make-node (quote n1) empty)) empty))
(check-expect (add-unique
               (make-node (quote n1) empty)
               (cons (node-name (make-node (quote n1) empty)) empty))
              (cons (node-name (make-node (quote n1) empty)) empty))
(define (add-unique node allnodes)
  (if (member (node-name node) allnodes)
      allnodes
      (cons (node-name node) allnodes)))

(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)

(define-syntax edge
  (syntax-rules (-> <-> <-)
    [(edge n1 -> n2)
     (set-node-edges! n1 (add-unique n2 (node-edges n1)))]
    [(edge n1 <- n2)
     (set-node-edges! n2 (add-unique n1 (node-edges n2)))]
    [(edge n1 <-> n2)
     (begin
       (edge n1 -> n2)
       (edge n1 <- n2)
       )]
    ))

(edge n0 -> n1)


(define-syntax edges
  (syntax-rules ()
    [(edges n1 op n2)
     (edge n1 op n2)]
    [(edges n1 op n2 op2 n3 ...)
     (begin (edges n1 op n2)
            (edges n2 op2 n3 ...))]))

(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)


(define-syntax does
  (syntax-rules (have a an bidirectional edge path from ?)
    [(does n1 have an edge from n2 ?)
     (not (false? (member (quote n1) (node-edges n2))))]
    [(does n1 have a bidirectional edge from n2 ?)
     (not (false? (and (member (quote n1) (node-edges n2))
                       (member (quote n2) (node-edges n1)))))]
    [(does n1 have a path from n2 ?)
     (path n2 n1)]))

(does n1 have an edge from n0 ?)
(does n0 have an edge from n1 ?)
(does n2 have a bidirectional edge from n1 ?)



(define (path node1 node2)
  (local
    
    
    [(define (fn-for-node node todo visited)
       (cond [(member (my-eval node) visited)
              (fn-for-lon todo visited)]
             [(equal? (my-eval node)
                      (my-eval node2)) true]
             [else (fn-for-lon (append todo (node-edges (my-eval node)))
                               (cons (my-eval node) visited))]))
     
     
     (define (fn-for-lon todo visited)
       (if (empty? todo)
           false
           (fn-for-node (first todo) (rest todo) visited)))
     ]
    (fn-for-node node1 empty empty)))

(does n2 have a path from n0 ?)


(test)