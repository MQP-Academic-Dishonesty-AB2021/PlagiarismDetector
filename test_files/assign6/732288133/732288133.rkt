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
















(define (add-unique nd graph)
  (if (member nd (graph-nodes graph))
      graph
      (begin (set-graph-nodes! graph (cons nd (graph-nodes graph)))
             graph)))

(define gtest (make-graph 'gtest empty))
(define gtest2 (make-graph 'gtest2 (list 'nt1 'nt3)))
(define nt1 (make-node 'nt1 empty))
(define nt2 (make-node 'nt2 empty))
(define nt3 (make-node 'nt3 empty))
(define nt4 (make-node 'nt4 (list 'nt1 'nt2)))

(check-expect (add-unique 'nt1 gtest)    
              (make-graph 'gtest
                          (list 'nt1)))
(check-expect (add-unique 'nt1 gtest)    
              (make-graph 'gtest
                          (list 'nt1)))
(check-expect (add-unique 'nt2 gtest2)   
              (make-graph 'gtest2
                          (list 'nt2 'nt1 'nt3)))
(check-expect (add-unique 'nt3 gtest2)   
              (make-graph 'gtest2
                          (list 'nt2 'nt1 'nt3)))
(check-expect (add-unique 'nt4 gtest)    
              (make-graph 'gtest
                          (list 'nt4 'nt1)))



(define-syntax create
  (syntax-rules ()
    [(create var val)
     (begin (define var val)
            (set! var val))]))


(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph 'name empty))]
    
    [(new vertex nd in name)
     (begin
       (create
        nd
        (make-node 'nd empty))
       (add-unique 'nd name))]))


(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)






(define-syntax edge
  (syntax-rules ()
    [(edge node1 node2)
     (if (member 'node2 (node-edges node1))
         void
         (set-node-edges! node1 (cons 'node2 (node-edges node1))))]))





(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges node1 -> node2)
     (edge node1 node2)]
    [(edges node1 <- node2)
     (edge node2 node1)]
    [(edges node1 <-> node2)
     (begin (edge node1 node2) (edge node2 node1))]
    [(edges node1 op1 node2 op2 ...)
     (begin (edges node1 op1 node2)
            (edges node2 op2 ...))]))









(define-syntax does
  (syntax-rules (have an edge from ? a bidirectional path)
    [(does node1 have an edge from node2 ?)
     (not (false? (member 'node1 (node-edges node2))))]
    [(does node1 have a bidirectional edge from node2 ?)
     (and (does node1 have an edge from node2 ?)
          (does node2 have an edge from node1 ?))]
    
    [(does node1 have a path from node2 ?)
     (path? node2 node1)]))


(edges n0 -> n1 <-> n2 -> n0)
 

(check-expect (does n1 have an edge from n0 ?) true)
 

(check-expect (does n2 have an edge from n0 ?) false)



(check-expect (does n2 have an edge from n1 ?) true)




(define (path? node1 node2)
  
  
                               
  (local [(define (fn-for-node n todo visited)
            (cond
              [(member n (list node2)) true] 
              [(member (node-name n) visited)
               (fn-for-lon todo visited)]
              [else (fn-for-lon (append (node-edges n) todo)
                                (cons (node-name n) visited))])) 
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (my-eval (first todo))
                                (rest todo)
                                visited)]))]
    (fn-for-node node1 empty empty)))

(new graph gtest3)
(new vertex ntp1 in gtest3)
(new vertex ntp2 in gtest3)
(new vertex ntp3 in gtest3)
(new vertex ntp4 in gtest3)
(new vertex ntp5 in gtest3)
(new vertex ntp6 in gtest3)

(edges ntp5 -> ntp1 -> ntp3 -> ntp5 <-> ntp2)
(edges ntp4 -> ntp1)

(check-expect (path? ntp1 ntp2) true) 
(check-expect (path? ntp1 ntp4) false) 
(check-expect (path? ntp4 ntp1) true) 
(check-expect (path? ntp6 ntp5) false) 
(check-expect (path? ntp5 ntp1) true) 



(test) 

