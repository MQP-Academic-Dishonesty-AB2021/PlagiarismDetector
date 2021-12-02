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













(define-syntax new
  (syntax-rules(graph vertex in)
    [(new graph name0)
     (create name0 (make-graph (quote name0) (quote ())))]
   
    [(new vertex node0 in graph0)
     (begin (create node0 (make-node (quote node0) (quote())))
            (set-graph-nodes! graph0 (add-unique (graph-nodes graph0) node0))
            graph0)
     ] ))


(define-syntax create
  (syntax-rules ()
    [ (create v value)

      (begin (define v value) 
             (set! v value))
      ]))



(define (add-unique alist anode)
  (if (member (node-name anode) alist)
      alist
      (cons (node-name anode) alist))
  )

(check-expect (add-unique (graph-nodes g0) n0) (graph-nodes g0)) 
(check-expect (add-unique (graph-nodes g0)
                          (make-node (quote n1) (quote ())))
              (quote (n1 n0))) 

(check-expect (add-unique (node-edges  (make-node (quote n1) (quote ()))) n2)  (quote (n2))) 
(check-expect (add-unique (node-edges  (make-node (quote n1) (quote (n2)))) n2)  (quote (n2))) 

(new graph g0)
(new vertex n0 in g0)





(define-syntax edge
  (syntax-rules ()
    [(edge n1 n2)
     (begin 
       (set-node-edges! n1 (add-unique (node-edges n1) n2)) void)]))



(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges n1) n1]
    [(edge n1 -> n2 operator ...)
     (begin
       (edge n1 n2)
       (edges n2 operator ...)
       void)]
    [(edge n1 <-> n2 operator ...)
     (begin
       (edge n2 n1)
       (edge n1 n2)
       (edges n2 operator ...)
       void)]
                             
    [(edge n1 <- n2 operator ...)
     (begin
       (edge n2 n1)
       (edges n2 operator ...) void)]))

(create n1 (make-node (quote n1) (quote ())))
(create n2 (make-node (quote n2) (quote ())))
(create n3 (make-node (quote n3) (quote ())))


 


(define-syntax does
  (syntax-rules (have an a bidirectional edge from ?)
    [(does n1 have an edge from n0 ?)
    (not (false? (member (node-name n1) (node-edges n0))))]
     [(does n1 have a bidirectional edge from n0 ?)
     (and  (not (false? (member (node-name n1) (node-edges n0))))
            (not (false? (member (node-name n0) (node-edges n1)))))]
     [(does n1 have a path from n0 ?)
      (path? n1 n0)]
    ))


(check-expect (does n1 have an edge from n0 ?) false) 
(check-expect (does n1 have a bidirectional edge from n0 ?) false)  

(create n4 (make-node (quote n4) (quote ())))
(create n5 (make-node (quote n5) (quote (n4 n5))))

(create n6 (make-node (quote n6) (quote (n7))))
(create n7 (make-node (quote n7) (quote (n6))))

(check-expect (does n4 have an edge from n4 ?) false) 
(check-expect (does n5 have an edge from n5 ?) true) 
(check-expect (does n4 have an edge from n5 ?) true)  
(check-expect (does n5 have an edge from n4 ?) false)  
(check-expect (does n6 have a bidirectional edge from n7 ?) true)  


 




(define (path? enode fnode)
  
  
  (local
    [(define (fn-for-node node todo visited)
       (cond
         [(equal? (my-eval node) (my-eval enode)) true]
         [(member (node-name (my-eval node)) visited)
          (fn-for-edges todo visited)]
         [else
          (fn-for-edges (append (node-edges (my-eval node)) todo)
                        (cons (node-name (my-eval node)) visited))]))

        (define (fn-for-edges todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-node fnode empty empty)))
     
(create n9 (make-node (quote n9) (quote (n8))))
(create n8 (make-node (quote n8) (quote (n9 n8 n7 n5))))

(check-expect (does n4 have a path from n9 ?) true) 
(check-expect (does n9 have a path from n4 ?) false) 

(check-expect (does n9 have a path from n9 ?) true) 
(check-expect (does n5 have a path from n9 ?) true) 
(check-expect (does n8 have a path from n9 ?) true) 
(check-expect (does n0 have a path from n9 ?) false) 
(check-expect (does n8 have a path from n8 ?) true) 



(test)
