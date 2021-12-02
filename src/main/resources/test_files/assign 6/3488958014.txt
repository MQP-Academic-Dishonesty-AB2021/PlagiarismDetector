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
    [(create var num)
     (begin
       (define var num)
       (set! var num))]))








(check-expect (add-unique n0 g0 graph-nodes) (list (node 'n0 '(n9 n1)) 'n9 'n2 'n1 'n0)) 
(check-expect (add-unique ne g0 graph-nodes) (cons ne (graph-nodes g0)))

(define (add-unique elm1 elm2 fn)
  (cond [(not (false? (member elm1 (fn elm2))))
         (fn elm2)]
        [else
         (cons elm1 (fn elm2))]))



(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph (quote name) (quote ())))]
    [(new vertex n in g)
     (begin
       (create n (make-node (quote n) (quote ())))
       (set-graph-nodes! g (add-unique (node-name n) g graph-nodes)))]))


(create r 12) 
(new graph g0)  
(new vertex n0 in g0) 
(new vertex n1 in g0) 
(define ne (make-node 'ne empty)) 






(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges node1 node2)
     (set-node-edges! node1 (add-unique (node-name node2) node1 node-edges))]
    
    [(edges node1 -> node2)
     (set-node-edges! node1 (add-unique (node-name node2) node1 node-edges))]
    [(edges node1 <- node2)
     (set-node-edges! node2 (add-unique (node-name node1) node2 node-edges))]
    [(edges node1 <-> node2)
     (begin
       (edges node1 node2)
       (edges node2 node1))]
    
    [(edges node1 op1 node2 op2 node3 ...)
     (begin
       (edges node1 op1 node2)
       (edges node2 op2 node3 ...))]))

    
(new vertex n2 in g0)
(new vertex n9 in g0)
(edges n0 n1)
(edges n0 n1) 



(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)








(define-syntax does
  (syntax-rules (have a an bidirectional edge path from ?)
    [(does node1 have an edge from node2 ?)
     (if (member (quote node1) (node-edges node2))
         true
         false)]
    [(does node1 have a bidirectional edge from node2 ?)
     (if (and (member (quote node1) (node-edges node2)) (member (quote node2) (node-edges node1)))
         true
         false)]
    [(does node1 have a path from node2 ?)
     (path? node1 node2)]))

(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n2 have a bidirectional edge from n1 ?) true) 
          










(check-expect (path? n0 n1) true)                     
(check-expect (path? n9 n1) true)                     
(check-expect (path? n1 n9) false)                    
(check-expect (path? n0 n0) true)                     
(check-expect (path? n9 n9) false)                    
(check-expect (does n0 have a path from n1 ?) true)   
(check-expect (does n9 have a path from n1 ?) true)   
(check-expect (does n0 have a path from n9 ?) false)  
(check-expect (does n0 have a path from n0 ?) true)   
(check-expect (does n9 have a path from n9 ?) false)  

(define (path? node1 node2)
  (local [
          (define (path0 n1 n2 todo) 
            (cond [(empty? todo) false]
                  [(equal? n1 n2) true]
                  [else
                   (path0 n1
                          (my-eval (first todo))
                          (append (node-edges (my-eval (first todo))) (rest todo)))]))]
   
    (path0 node1 node2 (node-edges node2))))

(test)








  
  
