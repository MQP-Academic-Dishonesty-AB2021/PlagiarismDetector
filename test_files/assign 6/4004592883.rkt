


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
    [(create var val)
     (begin (define var val)
            (set! var val)
            var)]))







(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name) (create name (make-graph (quote name) empty))]
    [(new vertex vname in g0)
     (begin (create vname (make-node (quote vname) empty))
            (set! g0
                  (make-graph (graph-name g0)
                              (cons (quote vname)
                                    (graph-nodes g0))))
            graph)]))




(define-syntax edge
  (syntax-rules ()
    [(edge node1 node2)
     (begin (create newnode1 (make-node (node-name (my-eval node1))
                                        (cons (quote node2)
                                              (node-edges (my-eval node1)))))
            (set! node1 newnode1)
            void)]))




(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges node1 -> node2 arrow ...)
     (begin (edge node1 node2)
            (edges node2 arrow ...)
            void)
     ]
    [(edges node1 <- node2 arrow ...)
     (begin (edge node2 node1)
            (edges node2 arrow ...)
            void)]
    [(edges node1 <-> node2 arrow ...)
     (begin (edge node1 node2)
            (edge node2 node1)
            (edges node2 arrow ...)
            void)]
    [(edges node) node]))





(create n1 (make-node 'n1 empty))
(create n2 (make-node 'n1 empty))
(create n3 (make-node 'n3 empty))
(create n4 (make-node 'n4 empty))
(edges n1 <-> n2 -> n4)

(check-expect (does n1 have an edge from n2 ?) true)

(check-expect (does n1 have an edge from n3 ?) false)

(check-expect (does n1 have a bidirectional edge from n2 ?) true)

(check-expect (does n1 have a bidirectional edge from n3 ?) false)


(check-expect (does n1 have a path from n1 ?) true)

(check-expect (does n2 have a path from n1 ?) true)

(check-expect (does n3 have a path from n1 ?) false)

(check-expect (does n4 have a path from n1 ?) true)


(define-syntax does
  (syntax-rules (have a an bidirectional edge path from ?)
    [(does node1 have an edge from node2 ?)
     (not (false? (member (quote node1) (node-edges node2))))]
    [(does node1 have a bidirectional edge from node2 ?)
     (and
      (not (false? (member (quote node1) (node-edges node2))))
      (not (false? (member (quote node2) (node-edges node1)))))]
    [(does node1 have a path from node2 ?)
     (not (false? (find-path node2 node1)))]))






(check-expect (find-path n1 n1) true)

(check-expect (find-path n1 n2) true)

(check-expect (find-path n1 n3) false)

(check-expect (find-path n1 n4) true)

(check-expect (find-path n4 n1) false)



(define (find-path n0 n1)
  (local [(define (fn-for-node node todo visited)
            (cond
              [(equal? (my-eval node) (my-eval n1)) true]
              [(member node visited)
               (fn-for-lon (rest (node-edges (my-eval node))) visited)]
              [else 
               (fn-for-lon (append (node-edges (my-eval node)) todo)
                           (cons (node-name (my-eval node)) visited))]))
          (define (fn-for-lon todo visited)
            (cond
              [(empty? todo) false]
              [else (fn-for-node (first todo) (rest todo) visited)]))]
    (fn-for-node n0 empty empty)))






(check-expect (add-unique (make-node "A" empty) (make-graph "AG" empty))
              (make-graph "AG" (list (make-node "A" empty))))


(check-expect (add-unique (make-node "B" (list (make-node "A" empty)))
                          (make-graph "BG" (list (make-node "A" empty))))
              (make-graph "BG" (list (make-node "B"
                                                (list (make-node "A" empty)))
                                     (make-node "A" empty))))


(check-expect (add-unique (make-node "A" empty)
                          (make-graph "AG" (list (make-node "A" empty))))
              (make-graph "AG" (list (make-node "A" empty))))

              

(define (add-unique node graph)
  (if (member node (graph-nodes graph))
      graph
      (make-graph (graph-name graph) (cons node (graph-nodes graph)))))



(test)