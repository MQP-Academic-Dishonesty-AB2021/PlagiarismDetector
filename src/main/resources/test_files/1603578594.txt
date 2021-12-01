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














(define g0-init (make-graph 'g0 '()))
(define g0-final (make-graph 'g0 '(n42)))

(define g1-init (make-graph 'g1 '(n42)))
(define g1-final (make-graph 'g1 '(n42)))

(define g2-init (make-graph 'g2 '(n6 n7)))
(define g2-final (make-graph 'g2 '(n6 n7 n42)))



(define n0-init (make-node 'n0 '()))
(define n0-final (make-node 'n0 '(n42)))

(define n1-init (make-node 'n1 '(n42)))
(define n1-final (make-node 'n1 '(n42)))

(define n2-init (make-node 'n2 '(n6 n7)))
(define n2-final (make-node 'n2 '(n42 n6 n7)))



(define nd0 (make-node 'nd0 '(nd1)))
(define nd1 (make-node 'nd1 '(nd2 nd3)))
(define nd2 (make-node 'nd2 '()))
(define nd3 (make-node 'nd3 '(nd1 nd2)))












(check-expect (begin
                (add-unique 'n42 g0-init set-graph-nodes! graph-nodes)
                g0-init)
              g0-final)


(check-expect (begin
                (add-unique 'n42 g1-init set-graph-nodes! graph-nodes)
                g1-init)
              g1-final)


(check-expect (begin
                (add-unique 'n42 g2-init set-graph-nodes! graph-nodes)
                g2-final)
              g2-final)


(check-expect (begin
                (add-unique 'n42 n0-init set-node-edges! node-edges)
                n0-init)
              n0-final)


(check-expect (begin
                (add-unique 'n42 n1-init set-node-edges! node-edges)
                n1-init)
              n1-final)


(check-expect (begin
                (add-unique 'n42 n2-init set-node-edges! node-edges)
                n2-init)
              n2-final)
  
(define (add-unique n elem setter field)
  (local [(define (add-unique lon)
            (cond
              [(empty? lon) (setter elem (cons n (field elem)))]
              
              [(equal? n (first lon)) 
               (void)]
              [else
               (add-unique (rest lon))]))]
    (add-unique (field elem))))







(check-expect (path-exists? 'nd0 'nd1) true)


(check-expect (path-exists? 'nd2 'nd0) false)


(check-expect (path-exists? 'nd0 'nd2) true)

(define (path-exists? n0 n1)
  (local [(define (fn-for--node n todo visited)
            (cond 
              [(member n visited)
               (fn-for--lon todo visited)]
              [(not (false? (member n1 (node-edges (my-eval n)))))
               true]
              [else
               (fn-for--lon 
                (append (node-edges (my-eval n)) todo) 
                (cons n visited))]))
          (define (fn-for--lon todo visited)
            (cond
              [(empty? todo) false]
              [else
               (fn-for--node (first todo) (rest todo) visited)]))]
    (fn-for--node n0 empty empty)))







(define-syntax create
  (syntax-rules ()
    [(create el exprs)
     (begin
       (define el exprs)
       (set! el exprs))]))



(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph g0)
     (create g0 (make-graph 'g0 '()))]
    [(new vertex n0 in g0)
     (begin
       (create n0 (make-node 'n0 '()))
       (add-unique 'n0 g0 set-graph-nodes! graph-nodes))])) 




(define-syntax edge
  (syntax-rules ()
    [(edge n0 n1)
     (add-unique 'n1 n0 set-node-edges! node-edges)]))



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
    [(edges n0 op1 n1 op2 ...)
     (begin
       (edges n0 op1 n1)
       (edges n1 op2 ...))]))



(define-syntax does
  (syntax-rules (have an a bidirectional edge path from ?)
    [(does n1 have an edge from n0 ?)
     (not (false? (member 'n1 (node-edges n0))))]
    [(does n1 have a birdirectional edge from n0 ?)
     (not (false? (and (member 'n1 (node-edges n0))
                       (member 'n0 (node-edges n1)))))]
    [(does n1 have a path from n0 ?)
     (path-exists? 'n0 'n1)]))






(new graph g)
(new vertex n0 in g)
(new vertex n1 in g)
(new vertex n2 in g)
(new vertex n9 in g)


(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)


(check-expect (begin
               (edge n0 n1)
               n0) (make-node 'n0 '(n9 n1)))


(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n1 have an edge from n2 ?) true)
(check-expect (does n0 have an edge from n2 ?) true)
(check-expect (does n2 have an edge from n0 ?) false)


(check-expect (does n2 have a bidirectional edge from n1 ?) true)
(check-expect (does n1 have a bidirectional edge from n0 ?) false)


(check-expect (does n2 have a path from n0 ?) true)
(check-expect (does n2 have a path from n9 ?) false)
(check-expect (does n9 have a path from n2 ?) true)
(check-expect (does n9 have a path from n1 ?) true)

(test)