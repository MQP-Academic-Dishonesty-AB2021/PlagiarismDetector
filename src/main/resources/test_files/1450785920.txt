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







(define TESTG (make-graph 'g '()))
(define TESTN (make-node 'n1 '()))
(define TESTN2 (make-node 'n2 '()))
(define TESTN3 (make-node 'n3 '()))
(define TESTN4 (make-node 'n4 '()))






(define-syntax create
  (syntax-rules ()
    [(create var val)
     (begin
       (define var val)
       (set! var val))]))





(check-expect
 (begin
   (add-unique TESTN TESTG)
   TESTG)
 (make-graph 'g '(n1)))

(check-expect
 (begin
   (add-unique TESTN2 TESTG)
   TESTG)
 (make-graph 'g '(n2 n1)))

(check-expect 
 (begin
   (add-unique TESTN TESTG)
   TESTG)
 (make-graph 'g '(n2 n1)))


(define (add-unique node graph)
  (if
   (member (node-name node) (graph-nodes graph))
   
   graph
   (set-graph-nodes! graph (cons (node-name node) (graph-nodes graph)))))
        







(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph g0)
     (create g0 (make-graph 'g0 '()))]
    [(new vertex new-node in g)
     (begin
       (create new-node (make-node 'new-node '()))
       (add-unique new-node g))]))








(check-expect
 (begin
   (add-uniquev2 TESTN3 TESTN4)
   TESTN4)
 (make-node 'n4 '(n3)))

(check-expect
 (begin
   (add-uniquev2 TESTN2 TESTN4)
   TESTN4)
 (make-node 'n4 '(n2 n3)))

(check-expect 
 (begin
   (add-uniquev2 TESTN TESTN2)
   TESTN4)
 (make-node 'n4 '(n2 n3)))

(define (add-uniquev2 edge node)
  (if
   (member (node-name edge) (node-edges node))
   
   node
   (set-node-edges! node (cons (node-name edge) (node-edges node)))))




(define-syntax edge
  (syntax-rules ()
    [(edge n0 e0)
     (add-uniquev2 e0 n0)]))




(define-syntax edges
  (syntax-rules (-> <-> <-)
    [(edges n0 -> n1)
     (add-uniquev2 n1 n0)]
    [(edges n0 <- n1)
     (add-uniquev2 n0 n1)]
    [(edges n0 <-> n1)
     (begin
       (add-uniquev2 n1 n0)
       (add-uniquev2 n0 n1))]
    [(edges n0 arg0 n1 arg2 ...)
     (begin
       (edges n0 arg0 n1)
       (edges n1 arg2 ...))]))



(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(new vertex n4 in g0)
(new vertex n9 in g0)







(edges n0 -> n1 <-> n2 -> n0 -> n4 <- n2)





(check-expect (does n0 have an edge from n0 ?) false) 
(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n2 have a bidirectional edge from n1 ?) true)
(check-expect (does n2 have a bidirectional edge from n4 ?) false) 
(check-expect (does n9 have a path from n0 ?) false) 
(check-expect (does n2 have a path from n0 ?) true)


(define-syntax does
  (syntax-rules (have a an bidirectional edge path from ?)
    [(does n0 have an edge from n1 ?)
     (if
      (member 'n0 (node-edges n1))
      true
      false)]
    [(does n0 have a bidirectional edge from n1 ?)
     (and
      (if
       (member 'n0 (node-edges n1))
       true
       false)
      (if
       (member 'n1 (node-edges n0))
       true
       false))]
    
    [(does n0 have a path from n1 ?)
     (path? n1 n0)]))










(check-expect (path? n0 n0) true) 
(check-expect (path? n0 n1) true)
(check-expect (path? n0 n2) true)
(check-expect (path? n4 n1) false)
(check-expect (path? n2 n0) true)
(check-expect (path? n0 n9) false)

(define (path? n0 n1)
  (local [(define (inner-path n0 todo visited) 
            (cond
              [(member n0 visited) 
               (fn-for-loe todo visited)]
              [(equal? (node-name (my-eval n0)) (node-name (my-eval n1)))
               true] 
              [else
               (fn-for-loe (append (node-edges (my-eval n0)) todo) 
                           (cons n0 visited))]))
          (define (fn-for-loe todo visited)
            (cond [(empty? todo) false] 
                  [else
                   (inner-path (first todo) 
                               (rest todo)
                               visited)]))]
    (inner-path n0 empty empty)))








(check-expect (path n1 n4) "n1 -> n2 -> n4")

(define (path node1 node2)
  (local [(define (inner-path node1 todo visited current lop) 
            (cond
              [(member node1 visited) 
               (fn-for-loe todo visited (string-append (symbol->string (node-name (my-eval node1))) " ->") lop)]
              
              [(equal? (node-name (my-eval node1)) (node-name (my-eval node2)))
               (fn-for-loe (append (node-edges (my-eval node1)) todo) 
                           (cons node1 visited)
                           ""
                           (cons current lop))]

              [(empty? (node-edges (my-eval node1)))
               (fn-for-loe todo visited " ->" lop)]
              
              [else
               (fn-for-loe (append (node-edges (my-eval node1)) todo) 
                           (cons node1 visited)
                           (string-append current
                            " ->")
                           lop
                           )]))
          
          (define (fn-for-loe todo visited current lop)
            (cond
              [(empty? todo)
               (first (sort lop string>?))
               ]
                  
              [else
               (inner-path (first todo) 
                           (rest todo)
                           visited
                           (string-append current " " (symbol->string (first todo)))
                           lop)]))]
    
    (inner-path node1 empty empty (symbol->string (node-name (my-eval node1))) empty)))




    


(define x 3)
(define y 5)
(set! y 5)









(define z (list y))

(set! y 8)










(define z2 (list (quote y)))
(set! y 11)










(test)