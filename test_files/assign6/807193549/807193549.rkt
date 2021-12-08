


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
  (syntax-rules ()
    [(create name value)
     (begin
       (define name value)
       (set! name value))]))

(create CREATE-TEST-VAR-0 0)
(create CREATE-TEST-VAR-1 0)
(set! CREATE-TEST-VAR-1 1)


(check-expect CREATE-TEST-VAR-0 0)


(check-expect CREATE-TEST-VAR-1 1)













(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph (quote name) empty))]
    [(new vertex name in g)
     (begin (create name (make-node (quote name) empty))
            (set-graph-nodes! g (cons (quote name) (graph-nodes g))))]))

(new graph TEST-GRAPH-1)
(new vertex TEST-NODE-1 in TEST-GRAPH-1)


(check-expect TEST-NODE-1 (make-node 'TEST-NODE-1 empty))


(check-expect TEST-GRAPH-1 (make-graph 'TEST-GRAPH-1 '(TEST-NODE-1)))







(define-syntax edge
  (syntax-rules ()
    [(edge from to)
     (set-node-edges! from (cons (quote to) (node-edges from)))]))

(new graph EDGE-TEST-GRAPH)
(new vertex EDGE-TEST-NODE-0 in EDGE-TEST-GRAPH)
(new vertex EDGE-TEST-NODE-1 in EDGE-TEST-GRAPH)
(edge EDGE-TEST-NODE-0 EDGE-TEST-NODE-1)


(check-expect EDGE-TEST-NODE-1 (make-node 'EDGE-TEST-NODE-1 empty))


(check-expect EDGE-TEST-NODE-0
              (make-node 'EDGE-TEST-NODE-0 '(EDGE-TEST-NODE-1)))





























(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges n0 -> n1)
     (edge n0 n1)]
    [(edges n0 <- n1)
     (edge n1 n0)]
    [(edges n0 <-> n1)
     (begin 
       (edge n0 n1)
       (edge n1 n0))]
    [(edges n0 arrow1 n1 arrow2 ...)
     (begin (edges n0 arrow1 n1)
            (edges n1 arrow2 ...))]))

(new graph EDGES-TEST-GRAPH)
(new vertex EDGES-TEST-NODE-0 in EDGES-TEST-GRAPH)
(new vertex EDGES-TEST-NODE-1 in EDGES-TEST-GRAPH)
(new vertex EDGES-TEST-NODE-2 in EDGES-TEST-GRAPH)
(new vertex EDGES-TEST-NODE-3 in EDGES-TEST-GRAPH)
(edges EDGES-TEST-NODE-0 -> EDGES-TEST-NODE-1
       <- EDGES-TEST-NODE-2 <-> EDGES-TEST-NODE-3)


(check-expect EDGES-TEST-NODE-1
              (make-node 'EDGES-TEST-NODE-1 empty))


(check-expect EDGES-TEST-NODE-0
              (make-node 'EDGES-TEST-NODE-0 '(EDGES-TEST-NODE-1)))


(check-expect EDGES-TEST-NODE-2
              (make-node 'EDGES-TEST-NODE-2
                         '(EDGES-TEST-NODE-3 EDGES-TEST-NODE-1)))


(check-expect EDGES-TEST-NODE-3
              (make-node 'EDGES-TEST-NODE-3 '(EDGES-TEST-NODE-2)))

































(define-syntax does
  (syntax-rules (have an edge from ? a bidirectional path)
    [(does n0 have an edge from n1 ?)
     (not (false? (member (node-name n0) (node-edges n1))))]
    [(does n0 have a bidirectional edge from n1 ?)
     (and (does n0 have an edge from n1 ?)
          (does n1 have an edge from n0 ?))]
    [(does n0 have a path from n1 ?)
     (local
       [
        (define (reachable-nodes node visited)
          (if (member (node-name node) visited) 
              empty
              (cons (node-name node)
                    (reachable-nodes--lon (node-edges node)
                                          (cons (node-name node) visited)))))
        
        (define (reachable-nodes--lon lon visited)
          (cond
            [(empty? lon) empty]
            [else
             (append
              (reachable-nodes (my-eval (first lon)) visited)
              (reachable-nodes--lon (rest lon) visited))]))]
       (not (false? (member (node-name n0) (reachable-nodes n1 empty)))))]))

(new graph DOES-TEST-GRAPH)
(new vertex DOES-TEST-NODE-0 in DOES-TEST-GRAPH)
(new vertex DOES-TEST-NODE-1 in DOES-TEST-GRAPH)
(new vertex DOES-TEST-NODE-2 in DOES-TEST-GRAPH)
(new vertex DOES-TEST-NODE-3 in DOES-TEST-GRAPH)
(edges
 DOES-TEST-NODE-0 -> DOES-TEST-NODE-1 <- DOES-TEST-NODE-2 <-> DOES-TEST-NODE-3)


(check-expect (does DOES-TEST-NODE-0 have an edge from DOES-TEST-NODE-0 ?)
              false)


(check-expect (does DOES-TEST-NODE-0 have an edge from DOES-TEST-NODE-1 ?)
              false)


(check-expect (does DOES-TEST-NODE-1 have an edge from DOES-TEST-NODE-2 ?)
              true)


(check-expect
 (does DOES-TEST-NODE-2 have a bidirectional edge from DOES-TEST-NODE-3 ?) true)


(check-expect
 (does DOES-TEST-NODE-1 have a bidirectional edge from DOES-TEST-NODE-2 ?)
 false)

(new graph PATH-TEST-GRAPH)
(new vertex PATH-TEST-NODE-0 in PATH-TEST-GRAPH)
(new vertex PATH-TEST-NODE-1 in PATH-TEST-GRAPH)
(new vertex PATH-TEST-NODE-2 in PATH-TEST-GRAPH)
(new vertex PATH-TEST-NODE-3 in PATH-TEST-GRAPH)
(new vertex PATH-TEST-NODE-4 in PATH-TEST-GRAPH)
(new vertex PATH-TEST-NODE-5 in PATH-TEST-GRAPH)
(edges PATH-TEST-NODE-0 -> PATH-TEST-NODE-1 <-> PATH-TEST-NODE-2 <->
       PATH-TEST-NODE-3 <-> PATH-TEST-NODE-4 <-> PATH-TEST-NODE-5)


(check-expect
 (does PATH-TEST-NODE-0 have a path from PATH-TEST-NODE-0 ?)
 true)


(check-expect
 (does PATH-TEST-NODE-0 have a path from PATH-TEST-NODE-1 ?)
 false)


(check-expect
 (does PATH-TEST-NODE-5 have a path from PATH-TEST-NODE-0 ?)
 true)


(check-expect
 (does PATH-TEST-NODE-1 have a path from PATH-TEST-NODE-5 ?)
 true)


(check-expect
 (does PATH-TEST-NODE-0 have a path from PATH-TEST-NODE-5 ?)
 false)


(test)