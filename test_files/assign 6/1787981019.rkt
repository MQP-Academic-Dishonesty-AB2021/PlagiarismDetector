#lang Racket




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
     (begin (define name val)
            (set! name val))]))




(create g1 (make-graph 'g1 empty))
(create n3 (make-node 'n3 empty))
(create g2 (make-graph 'g2 (list 'n3)))

(define (add-unique node lon)
  (not (member node lon)))
      

(check-expect (add-unique 'n3 (graph-nodes g1)) true)    
(check-expect (add-unique 'n3 (graph-nodes g2)) false)   








(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph (quote name) empty))]
    [(new vertex vert in name)
     (begin
       (create vert (make-node (quote vert) empty))
       (if (add-unique (quote vert) (graph-nodes name))
           (set-graph-nodes! name (cons (quote vert) (graph-nodes name)))
           (graph-nodes name))
       )]))


(new graph g0)
(new vertex n1 in g0)
(new vertex n0 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)







(define-syntax edge
  (syntax-rules ()
    [(edge e1 e2)
     (begin
       (if (add-unique (quote e2) (node-edges e1))  
           (set-node-edges! e1 (cons (quote e2) (node-edges e1)))
           (void)))]))
       






(define-syntax edges
  (syntax-rules (-> <-> <-)    
    [(edges e1 -> e2)
     (begin
       (if (add-unique (quote e2) (node-edges e1))  
           (set-node-edges! e1 (cons (quote e2) (node-edges e1)))
           (void)))]
    [(edges e1 <- e2)
     (begin
       (if (add-unique (quote e1) (node-edges e2))
           (set-node-edges! e2 (cons (quote e1) (node-edges e2)))
           (void)))
     ]
    [(edges e1 <-> e2)
     (begin
       (edges e1 -> e2)
       (edges e2 -> e1))]
    [(edges e1 op e2 op2 ...)
     (begin
       (edges e1 op e2)
       (edges e2 op2 ...))]
    [(edges e2)
     "done"]        
    ))
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)








(check-expect (pathing n1 n0) true)
(check-expect (pathing n2 n0) true)
(check-expect (pathing n1 n9) false)

(define (pathing OOI origin)
  (local
    [(define (fn-for-node node todo visited)
       (cond
         [(equal? OOI node) true]
         [(member (node-name node) visited) (fn-for-lon (append todo (node-edges node)) visited)]
         [else (fn-for-lon (append todo (node-edges node)) (cons (node-name node) visited))]))
     (define (fn-for-lon todo visited)
       (if (empty? todo)
           false  
           (fn-for-node (my-eval (first todo)) (rest todo) visited)))]
    (fn-for-node origin empty empty)))





(define-syntax does
  (syntax-rules (bidirectioanl edge ? path)
    [(does e1 have an edge from e2 ?)
     (not (add-unique (quote e1) (node-edges e2)))]
    [(does e1 have a bidirectional edge from e2 ?)
     (and (does e1 have an edge from e2 ?)
          (does e2 have an edge from e1 ?))]
    [(does e1 have a path from e2 ?)
     (pathing e1 e2)]))

(check-expect (does n2 have a path from n0 ?) true)
(check-expect (does n1 have a path from n9 ?) false)
(check-expect (does n0 have a path from n0 ?) true)
(check-expect (does n9 have a path from n0 ?) true)

(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n2 have a bidirectional edge from n1 ?) true)
(check-expect (does n0 have a bidirectional edge from n1 ?) false)
(test)


