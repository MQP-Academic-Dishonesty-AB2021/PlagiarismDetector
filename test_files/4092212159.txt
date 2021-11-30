

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
    [(create x a)
     (begin (define x a) (set! x a))]))



(define-syntax new
  (syntax-rules (graph vertex in)
    
    [(new graph g)
     (create g
             (make-graph
              (quote g)
              empty))]
    
    [(new vertex v in g)
     (begin
       (create v (make-node (quote v) empty))
       (add-unique v g))]))





(define (add-unique v g)
  (cond
    [(not (false? (member (node-name v) (graph-nodes g)))) (void)]
    [else
     (set-graph-nodes! g (cons (node-name v) (graph-nodes g)))]))


(new graph x1)
(new graph x2)
(new graph x3)
(define m1 (make-node 'm1 '()))
(define m2 (make-node 'm2 '()))
(define m3 (make-node 'm3 '()))



(add-unique m2 x2)                  
(check-expect x2 (graph 'x2 '(m2))) 

(add-unique m1 x1)
(check-expect (add-unique m1 x1) (void)) 


(add-unique m2 x1)
(check-expect x1 (graph 'x1 '(m2 m1)))

(add-unique m2 x3)
(add-unique m3 x3)
(check-expect x3 (graph 'x3 '(m3 m2)))








(define-syntax edge
  (syntax-rules ()
    [(edge p c)
     (add-node p (quote c))]))






(define-syntax edges
  (syntax-rules (<- -> <->)
    
    [(edges p -> c)
     (add-node p c)]
    
    [(edges c <- p)
     (add-node p  c)]
    
    [(edges bd1 <-> bd2)
     (begin
       (add-node bd1 bd2)
       (add-node bd2 bd1))]
    
    [(edges a op1 b op2 ...)
     (begin
       (edges a op1 b)
       (edges b op2 ...))]))





(define (add-node n1 n2)
  (if (not (member (node-name n2) (node-edges n1)))
      (set-node-edges! n1 (cons (node-name n2) (node-edges n1)))
      (void)))




(define l1 (make-node 'l1 '()))
(define l2 (make-node 'l2 '()))
(define l3 (make-node 'l3 '()))
(define l4 (make-node 'l4 '()))
(define l5 (make-node 'l5 '()))
(define l6 (make-node 'l6 '()))




(add-node l1 l2)
(check-expect l1 (node 'l1 '(l2)))  

(add-node l1 l2)
(check-expect l1 (node 'l1 '(l2)))  


(add-node l4 l5)
(add-node l4 l6)
(check-expect l4 (node 'l4 '(l6 l5)))

(add-node l6 l1)
(add-node l6 l2)
(add-node l6 l3)
(check-expect l6 (node 'l6 '(l3 l2 l1)))











(define-syntax does
  (syntax-rules (have an edge from ?    
                      a bidirectional   
                      path)             
    
    [(does p have an edge from c ?)                             
     (cond
       [(false? (member (quote p) (node-edges c))) false]
       [else true])]
    
    [(does bd1 have a bidirectional edge from bd2 ?)            
     (and
      (cond
        [(false? (member (quote bd1) (node-edges bd2))) false]
        [else true])
      (cond
        [(false? (member (quote bd2) (node-edges bd1))) false]
        [else true]))]
 
    
    
    
    [(does x have a path from y ?)                              
     (path? x y)]))




(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)


(check-expect (does n0 have an edge from n0 ?) false) 
(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n2 have an edge from n0 ?) false)



(check-expect
 (does n2 have a bidirectional edge from n1 ?) true)  
(check-expect
 (does n0 have a bidirectional edge from n9 ?) false)
(check-expect
 (does n0 have a bidirectional edge from n0 ?) false) 




(check-expect (path? n0 n9) false) 
(check-expect (path? n1 n1) true) 


(check-expect (path? n0 n2) true)
(check-expect (path? n9 n2) true)

(define (path? x y)
  (local [
          (define (fn-for-node node visited)
            (cond
              [(not (false? (member (node-name node) visited))) false]
              [(equal? node x) true]
              [else (fn-for-lon (node-edges node)
                                (cons (node-name node) visited))]))
          
          (define (fn-for-lon lon visited)
            (cond
              [(empty? lon) false]
              [else
               (or
                (fn-for-node (my-eval (first lon)) visited)
                (fn-for-lon (rest lon) visited))]))]
    
    (fn-for-node y empty)))

























(test)