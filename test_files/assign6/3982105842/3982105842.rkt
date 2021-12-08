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
    [(create var val)
     (begin (define var 0)
            (set! var val))]))


(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph (quote name) empty))]
    [(new vertex name)
     (create name (make-node (quote name) empty))]
    [(new vertex v in g)
     (begin (create v (make-node (quote name) empty))
            (add-unique (quote v) (graph-nodes g)))]))









(check-expect (add-unique 1 (list 2 3)) (list 1 2 3))
(check-expect (add-unique 1 (list 1 3)) (list 1 3))

(define (add-unique x lox)
  (if (member x lox)
      lox
      (cons x lox)))



(define-syntax edge
  (syntax-rules ()
    [(edge v0 v1)
     (set-node-edges! v0
     (add-unique (quote v1) (node-edges v0)))]))


(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges v0 -> v1)
     (edge v0 v1)]
    [(edges v0 <- v1)
     (edge v1 v0)]
    [(edges v0 <-> v1)
     (begin (edge v0 v1)
            (edge v1 v0))]
    [(edges v0 op v1 rest ...)
     (begin (edges v0 op v1)
            (edges v1 rest ...))]))


(define-syntax does
  (syntax-rules (have an a bidirectional edge  path from ?)
    [(does v1 have an edge from v0 ?)
     (not (false? (member (quote v1) (node-edges v0))))]
    [(does v1 have a bidirectional edge from v0 ?)
     (and (does v1 have an edge from v0 ? )
          (does v0 have an edge from v1 ? ))]
    [(does end have a path from start ?)
     (path? start end)]))





(define node1 (make-node 'node1 empty))
(define node2 (make-node 'node2 empty))
(define node3 (make-node 'node3 empty))
(define node4 (make-node 'node4 empty))

(define graph1 (make-graph 'graph1 (list 'node1 'node2 'node3 'node4)))

(edges node1 -> node2 -> node3 -> node4 <- node1)

(check-expect (path? node1 node2) true)
(check-expect (path? node1 node3) true)
(check-expect (path? node4 node2) false)



(define (path? start end)
  
  
  (local [(define (fn-for-node n todo visited)
            
            
            
            (cond [(equal? (node-name n) (node-name end)) true]
                  [(member (node-name n) visited) 
                   (fn-for-lon todo visited)]
                  [else
                   (fn-for-lon (append (node-edges n) todo)
                               (cons (node-name n) visited))]))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (my-eval (first todo))
                                (rest todo)
                                visited)]))]
    (fn-for-node start empty empty)))






(test)