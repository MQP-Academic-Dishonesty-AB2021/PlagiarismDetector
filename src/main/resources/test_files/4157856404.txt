
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
     
     (begin (define name void) 
            (set! name value))]))





(check-expect (add-unique testGraph (quote node1)) (graph-nodes testGraph)) 
(check-expect (add-unique testGraph (quote node2)) (graph-nodes testGraph)) 







(define (add-unique graph node)
  
  (if (member node (graph-nodes graph))
      (graph-nodes graph)
      (cons node (graph-nodes graph))))





(define-syntax new
  (syntax-rules (graph vertex in)
    
    [(new graph g)
     (create g (make-graph (quote g) empty))]
    
    [(new vertex n in g)
     (begin
       (create n (make-node (quote n) empty))
       (set-graph-nodes! g (add-unique g (quote n))))]))


(new graph testGraph)
(new vertex node1 in testGraph)
(new vertex node2 in testGraph)






(define-syntax edge 
  (syntax-rules ()
    [(edge start end)
     
     (if (member (quote end) (node-edges start))
         (set-node-edges! start (node-edges start))
         
         (set-node-edges! start (cons (quote end) (node-edges start))))]))










(define-syntax edges
  (syntax-rules (<-> <- ->)
    [(edges n1 <- n2) (edge n2 n1)]
    [(edges n1 -> n2) (edge n1 n2)]
    [(edges n1 <-> n2) (begin (edge n1 n2) (edge n2 n1))]
    
    [(edges n1 direction1 n2 direction2 ...)
     (begin (edges n1 direction1 n2)
            (edges n2 direction2 ...))]))











(define-syntax does
  (syntax-rules (have a an path edge bidirectional from ?)
    
    [(does n1 have an edge from n2 ?)
     (not (false? (member (quote n1) (node-edges n2))))]
    
    [(does n1 have a bidirectional edge from n2 ?)
     (not (false?
           (and (member (quote n2) (node-edges n1))
                (member (quote n1) (node-edges n2)))))]
    
    [(does n1 have a path from n2 ?) (path? n2 n1)])) 


(new vertex node3 in testGraph)
(new vertex node4 in testGraph) 
(edges node1 -> node2 <-> node3 -> node4 -> node1)


(check-expect (does node1 have an edge from node1 ?) false) 
(check-expect (does node1 have an edge from node4 ?) true)  
(check-expect (does node3 have an edge from node4 ?) false) 
(check-expect (does node4 have an edge from node3 ?) true)  
(check-expect (does node3 have an edge from node1 ?) false) 

(check-expect (does node2 have a bidirectional edge from node2 ?) false) 
(check-expect (does node1 have a bidirectional edge from node4 ?) false) 
(check-expect (does node2 have a bidirectional edge from node1 ?) false) 
(check-expect (does node2 have a bidirectional edge from node3 ?) true)  
(check-expect (does node3 have a bidirectional edge from node2 ?) true)  






(new vertex node5 in testGraph) 

(new vertex node7 in testGraph) 
(edge node7 node1)











(check-expect (path? node1 node2) true) 
(check-expect (path? node1 node4) true) 
(check-expect (path? node4 node1) true) 
(check-expect (path? node5 node1) false) 

(define (path? n1 n2) 
  (local [(define (fn-for-node n todo visited)
                  
            (cond [(equal? (my-eval n) (my-eval n2)) true]
                  
                  [(member n visited) (fn-for-lon todo visited)]
                  
                  [else
                   (fn-for-lon (append (node-edges (my-eval n)) todo)
                               (cons n visited))]))
          
          (define (fn-for-lon todo visited)
                  
            (cond [(empty? todo) false]
                  
                  [else
                   (fn-for-node (first todo) (rest todo) visited)]))]
    
    
    
    (fn-for-node n1 empty empty)))


(check-expect (does node5 have a path from node1 ?) false) 
(check-expect (does node2 have a path from node1 ?) true)  
(check-expect (does node4 have a path from node1 ?) true)  
(check-expect (does node7 have a path from node1 ?) false) 
(check-expect (does node1 have a path from node7 ?) true)  






(new graph lostWoods) 
                      




(new vertex f1 in lostWoods)
(new vertex f2 in lostWoods)
(new vertex f3 in lostWoods)
(new vertex f4 in lostWoods)
(new vertex f5 in lostWoods)
(new vertex f6 in lostWoods)
(new vertex f7 in lostWoods)
(new vertex f8 in lostWoods)

(edge f5 f1)
(edges f1 -> f2 -> f4)
(edges f2 <-> f3 <-> f6 <- f8)
(edge f1 f6)
(edge f3 f1)



(check-expect (does f5 have an edge from f1 ?) false) 
(check-expect (does f1 have an edge from f5 ?) true)  

(check-expect (does f8 have a path from f6 ?) false) 
(check-expect (does f3 have a path from f6 ?) true)  
(check-expect (does f4 have a path from f5 ?) true)  
(check-expect (does f1 have a path from f1 ?) true)  

(check-expect (does f6 have a bidirectional edge from f2 ?) false) 
(check-expect (does f6 have a bidirectional edge from f3 ?) true)  

(check-expect (does f7 have a path from f1 ?) false)
(check-expect (does f7 have a path from f2 ?) false)
(check-expect (does f7 have a path from f3 ?) false)
(check-expect (does f7 have a path from f4 ?) false)
(check-expect (does f7 have a path from f5 ?) false)
(check-expect (does f7 have a path from f6 ?) false)
(check-expect (does f7 have a path from f8 ?) false)





(test)