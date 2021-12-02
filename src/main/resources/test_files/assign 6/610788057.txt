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









(define-syntax create
  (syntax-rules ()
    [(create name val)
     (begin (define name 0)
            (set! name val))]))










(check-expect (add-unique '() '(n0)) '(n0))

(check-expect (add-unique '(n0) '(n1)) '(n0 n1))

(check-expect (add-unique '(n0 n1) '(n3)) '(n0 n1 n3))

(check-expect (add-unique '(n0 n1) '(n1)) '(n0 n1))

(define (add-unique l1 l2)
  (if (member (first l2) l1)
      l1
      (append l1 l2)))




(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph (quote name) '()))]
    [(new vertex name in graph-name)
     (begin (create name (make-node (quote name) '()))
            (set-graph-nodes! graph-name
                              (add-unique (graph-nodes graph-name)
                                          (list (quote name)))))]))






(define-syntax edge
  (syntax-rules ()
    [(edge node1 node2)
     (set-node-edges! node1
                      (add-unique (node-edges node1)
                                  (list (quote node2))))]))



(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges node1 -> node2)
     (edge node1 node2)]
    [(edges node1 <- node2)
     (edge node2 node1)]
    [(edges node1 <-> node2)
     (begin (edge node1 node2) (edge node2 node1))]
    [(edges node1 op1 node2 op2 ...)
     (begin (edges node1 op1 node2) (edges node2 op2 ...))]))





(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)

(new graph g1)
(new vertex n3 in g1)
(new vertex n4 in g1)
(new vertex n5 in g1)
(new vertex n6 in g1)
(edges n3 <-> n4 -> n5 <- n3 <- n6)



(check-expect (does n1 have an edge from n0 ?) #t)
(check-expect (does n0 have an edge from n1 ?) #f)
(check-expect (does n2 have a bidirectional edge from n1 ?) #t)
(check-expect (does n4 have an edge from n5 ?) #f)
(check-expect (does n3 have a bidirectional edge from n4 ?) #t)
(check-expect (does n4 have a bidirectional edge from n3 ?) #t)
(check-expect (does n3 have a bidirectional edge from n6 ?) #f)
(check-expect (does n2 have a path from n0 ?) #t)
(check-expect (does n5 have a path from n4 ?) #t)
(check-expect (does n4 have a path from n5 ?) #f)
(check-expect (does n5 have a path from n6 ?) #t)

(define-syntax does
  (syntax-rules (have an a bidirectional edge path from ?)
    [(does node1 have an edge from node2 ?)
     (if (member (quote node1) (node-edges node2)) true false)]
    [(does node1 have a bidirectional edge from node2 ?)
     (and (does node1 have an edge from node2 ?)
          (does node2 have an edge from node1 ?))]
    [(does node1 have a path from node2 ?)
     (path-exists? (quote node2) (quote node1))]))





(check-expect (path-exists? 'n4 'n5) #t)
(check-expect (path-exists? 'n5 'n4) #f)
(check-expect (path-exists? 'n6 'n5) #t)

(define (path-exists? node1 node2)
  (local [(define (fn-for-node? node todo visited)
            (cond [(member node2 (node-edges (my-eval node))) true]
                  [(member node visited) (fn-for-lon? todo visited)]
                  [else (fn-for-lon? (append todo (node-edges (my-eval node)))
                                     (append visited (list node)))]))
          (define (fn-for-lon? todo visited)
            (cond [(empty? todo) false]
                  [else (fn-for-node? (first todo)
                                      (rest todo)
                                      visited)]))]
    (fn-for-node? node1 empty empty)))












 



(test)