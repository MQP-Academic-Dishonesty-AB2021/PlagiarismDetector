




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
     (begin (define name val)
            (set! name val))]))







(define (add-unique node LON)
  (if (member node LON)
      LON
      (cons node LON)))





(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name
             (make-graph (quote name) empty))]

    [(new vertex node-name in graph-name)
     (begin
       (create node-name (make-node (quote node-name) empty))
       (set-graph-nodes! graph-name
                         (add-unique (quote node-name)
                                     (graph-nodes (my-eval graph-name)))))]
    ))





(new graph G0)
(new vertex N0 in G0)
(new vertex N1 in G0)
(new vertex N2 in G0)
(new vertex N9 in G0)


(check-expect (add-unique 'N0 '())
              (list 'N0))
(check-expect (add-unique 'N1 (list 'N1 'N2)) (list 'N1 'N2))


(check-expect (begin (edge N0 N1) (edge N0 N9) (node-edges (my-eval N0))) '(N9 N1))

(define-syntax edge
  (syntax-rules ()
    [(edge node1 node2)
     (set-node-edges! node1 (add-unique (quote node2)
                                        (node-edges node1)))]))





(define-syntax edges
  (syntax-rules (-> <- <->)    
    [(edges node1 -> node2)
     (edge node1 node2)]
    
    [(edges node1 <-> node2)
     (begin (edge node1 node2)
            (edge node2 node1))]
    
    [(edges node1 <- node2)
     (edge node2 node1)]

    [(edges node1 func1 node2 func2 ...)
     (begin (edges node1 func1 node2)
            (edges node2 func2 ...))]      
    ))

(edges N0 -> N1 <-> N2 -> N0 -> N9 <- N2)       
(check-expect (node-edges (my-eval N1)) '(N2))  
(check-expect (node-edges (my-eval N9)) '())
(check-expect (node-edges (my-eval N2)) '(N9 N0 N1))





(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)





(define-syntax does
  (syntax-rules (have an edge from ? a bidirectional path)

    [(does node1 have an edge from node2 ?)
     (has-edge? node2 node1)]
    
    [(does node1 have a bidirectional edge from node2 ?)
     (and (has-edge? node1 node2) (has-edge? node2 node1))]
    
    
    [(does node1 have a path from node2 ?)
     (path-exists? (quote node2) (quote node1))]
    ))

(check-expect (does n1 have a bidirectional edge from n2 ?) true)
(check-expect (does n0 have a bidirectional edge from n2 ?) false)

(check-expect (does n0 have an edge from n2 ?) true)
(check-expect (does n2 have an edge from n0 ?) false)
(check-expect (does n1 have an edge from n9 ?) false)


(check-expect (does n1 have a path from n2 ?) true) 
(check-expect (does n2 have a path from n0 ?) true)
(check-expect (does n1 have a path from n1 ?) true)
(check-expect (does n1 have a path from n9 ?) false)





(check-expect (has-edge? n1 n2) true)
(check-expect (has-edge? n0 n9) true)
(check-expect (has-edge? n9 n0) false)
(check-expect (has-edge? n1 n9) false)

(define (has-edge? node1 node2)
  (if (member (node-name (my-eval node2)) (node-edges (my-eval node1)))
      true
      false))







(check-expect (path-exists? (quote n1) (quote n1)) true)
(check-expect (path-exists? (quote n1) (quote n2)) true)
(check-expect (path-exists? (quote n1) (quote n9)) true)
(check-expect (path-exists? (quote n9) (quote n0)) false)

(define (path-exists? startNode targetNode)
  (local[
         (define (path-node node todo visited)
           (cond [(equal? node targetNode) true]
                 [(member node visited)
                  (path-LON todo visited)]
                 [else (path-LON (append (node-edges (my-eval node)) todo)
                                 (cons (node-name (my-eval node)) visited))]
                  
                 ))
       
         (define (path-LON todo visited)
           (cond [(empty? todo) false]
                 [else (path-node (first todo)
                                  (rest todo)
                                  visited)]))
         
         ]
    (path-node startNode empty empty)))





(test)