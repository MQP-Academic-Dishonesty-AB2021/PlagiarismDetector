
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






(define-syntax create
  (syntax-rules ()
    [(create name val)
     (begin (define name val) (set! name val))]))






(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name) (create name (make-graph (quote name) empty))]
    [(new vertex name in the-graph)
     (begin (create name (make-node (quote name) empty))
            (set-graph-nodes! the-graph (add-unique (quote name) (graph-nodes the-graph))))]))








(define anode (make-node "a node" '()))
(define thenode (make-node "the node" '()))
(define lenode (make-node "le node" '()))
(define lanode (make-node "la node" '()))
(define elnode (make-node "el node" '()))

(check-expect (add-unique anode '()) (list anode)) 
(check-expect (add-unique anode (list thenode lenode lanode)) 
              (list anode thenode lenode lanode)) 
(check-expect (add-unique lenode (list anode thenode lanode elnode))
              (list lenode anode thenode lanode elnode))
(check-expect (add-unique elnode (list elnode lanode thenode)) 
              (list elnode lanode thenode))
(check-expect (add-unique elnode (list elnode)) (list elnode)) 


(define (add-unique node lon)
  (if (member node lon)
      lon
      (cons node lon)))








(define-syntax edge
  (syntax-rules ()
    [(edge n1 n2)
     (set-node-edges! n1 (add-unique (quote n2) (node-edges n1)))]))









(define-syntax edges
  (syntax-rules (-> <-> <-)
    [(edges n1 dir n2 dir2 more ...)
     (begin (edges n1 dir n2) (edges n2 dir2 more ...))]
    [(edges n1 -> n2) (edge n1 n2)]
    [(edges n1 <- n2) (edge n2 n1)]
    [(edges n1 <-> n2) (begin (edge n1 n2) (edge n2 n1))]))














(new graph sample-graph)
(new vertex node1 in sample-graph)
(new vertex node2 in sample-graph)
(new vertex node3 in sample-graph)
(new vertex node4 in sample-graph)
(new vertex node5 in sample-graph)
(new vertex node6 in sample-graph)

(edge node1 node2)
(edges node2 <-> node5 <- node4 -> node3)


(check-expect (does node2 have an edge from node1 ?) #t) 
(check-expect (does node1 have an edge from node2 ?) #f) 
(check-expect (does node6 have an edge from node3 ?) #f) 
(check-expect (does node5 have an edge from node4 ?) #t) 
(check-expect (does node2 have an edge from node5 ?) #t) 
(check-expect (does node5 have an edge from node2 ?) #t) 


(check-expect (does node2 have a bidirectional edge from node5 ?) #t) 
(check-expect (does node5 have a bidirectional edge from node2 ?) #t) 
(check-expect (does node2 have a bidirectional edge from node1 ?) #f) 
(check-expect (does node5 have a bidirectional edge from node4 ?) #f) 


(check-expect (does node6 have a path from node1 ?) #f) 
(check-expect (does node3 have a path from node2 ?) #f) 
(check-expect (does node5 have a path from node1 ?) #t) 
(check-expect (does node2 have a path from node1 ?) #t) 


(define-syntax does
  (syntax-rules (have an a bidirectional edge path from ?)
    [(does n1 have an edge from n0 ?)
     (if (member (quote n1) (node-edges n0))
         #t
         #f)]
    [(does n1 have a bidirectional edge from n0 ?)
     (if (and (member (quote n1) (node-edges n0))
              (member (quote n0) (node-edges n1)))
         #t
         #f)]

    [(does n1 have a path from n0 ?)
     (reachable? n0 n1)]))










(new graph graph-example2)
(new vertex v1 in graph-example2)
(new vertex v2 in graph-example2)
(new vertex v3 in graph-example2)
(new vertex v4 in graph-example2)
(new vertex v5 in graph-example2)
(new vertex v6 in graph-example2)

(edges v1 -> v2 <-> v3 <- v5 <- v1)
(edge v3 v6)

(check-expect (reachable? v4 v1) #f) 
(check-expect (reachable? v5 v4) #f) 
(check-expect (reachable? v2 v5) #f) 
(check-expect (reachable? v1 v2) #t) 
(check-expect (reachable? v5 v6) #t) 
(check-expect (reachable? v1 v6) #t) 


(define (reachable? start-node end-node)
  (local [(define (fn-for-node current-node todo visited)
            (if (eq? current-node end-node)
                #t
                (if (member current-node visited)
                    (fn-for-lon todo visited)
                    (fn-for-lon (append (node-edges current-node) todo)
                                (cons current-node visited)))))
          
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) #f]
                  [else (fn-for-node (my-eval (first todo))
                                     (rest todo)
                                     visited)]))]
          
    (fn-for-node start-node empty empty)))




(define-struct path-node (path node))






















(new graph graph-example3)
(new vertex n1 in graph-example3)
(new vertex n2 in graph-example3)
(new vertex n3 in graph-example3)
(new vertex n4 in graph-example3)
(new vertex n5 in graph-example3)
(new vertex n6 in graph-example3)
(new vertex n7 in graph-example3)
(new vertex n8 in graph-example3)

(edges n1 <-> n2 -> n3 -> n6)
(edges n3 <- n4 <- n5 <-> n8)
(edges n3 -> n6 -> n7)
(edge n1 n7)

(check-expect (path n3 n5) "") 
(check-expect (path n1 n7) "n1 -> n7") 
(check-expect (path n4 n6) "n4 -> n3 -> n6") 
(check-expect (path n1 n2) "n1 -> n2") 
(check-expect (path n8 n6) "n8 -> n5 -> n4 -> n3 -> n6") 
(check-expect (path n2 n6) "n2 -> n3 -> n6") 
(check-expect (path n2 n7) "n2 -> n1 -> n7") 


(define (path start-node end-node)
  (local [(define (fn-for-node current-node todo visited)
            (if (eq? (path-node-node current-node) end-node)
                (path-node-path current-node)
                (if (or (not (reachable? (path-node-node current-node) end-node))
                        (member (path-node-node current-node) visited))
                    (fn-for-lon todo (add-unique (path-node-node current-node) visited))
                    (fn-for-lon
                     (append todo
                             (map (λ (node)
                                    (make-path-node
                                     (string-append (path-node-path current-node)
                                                    " -> "
                                                    (symbol->string (node-name (my-eval node))))
                                     (my-eval node)))
                                  (node-edges (path-node-node current-node))))
                     (cons (path-node-node current-node) visited)))))
          
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) ""]
                  [else (fn-for-node (first todo) (rest todo) visited)]))]
          
    (fn-for-node (make-path-node (symbol->string (node-name start-node)) start-node) empty empty)))

(test) 
