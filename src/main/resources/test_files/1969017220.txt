


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
    [(create var num)
     (begin
       (define var 0)
       (set! var num)
       var
     )]))


(define-syntax new
  (syntax-rules (graph)
    [(new graph G)
     (define G (make-graph (quote G) (list )))]))


(define-syntax newv
  (syntax-rules (vertex in)
    [(newv vertex node in graph)
     (begin
       (define node (make-node (quote node) (list)))
       (set! graph (make-graph (graph-name graph) (add-unique (node-name node) (graph-nodes graph)))))]))


(check-expect (add-unique n1 (list n1 n2 n9)) (list n1 n2 n9))
(check-expect (add-unique n1 (list n2 n9)) (list n1 n2 n9))



(define (add-unique node Lon)
  (if (false? (member node Lon))
      (cons node Lon)
      Lon))






(define-syntax edge
  (syntax-rules ()
    [(edge one two)
     (set! one (make-node (node-name one) (add-unique (node-name two) (node-edges one))))]))



(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges one -> two)
     (set! one (make-node (node-name one) (add-unique (node-name two) (node-edges one))))]
    [(edges one <- two)
     (set! two (make-node (node-name two) (add-unique (node-name one) (node-edges two))))]
    [(edges one <-> two)
     (begin
       (set! one (make-node (node-name one) (add-unique (node-name two) (node-edges one))))
       (set! two (make-node (node-name two) (add-unique (node-name one) (node-edges two)))))]
    [(edges one op two op2 ...)
     (begin
       (edges one op two)
       (edges two op2 ...))]))
       
       



(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n2 ?) true)
(check-expect (does n0 have an edge from n0 ?) false)
(check-expect (does n0 have a bidirectional edge from n2 ?) false)
(check-expect (does n1 have a bidirectional edge from n2 ?) true)
(check-expect (does n0 have a path from n2 ?) true)



(define-syntax does
  (syntax-rules (an bidirectional path)
    [(does one have an edge from two ?)
     (if (false? (member (quote one) (node-edges two)))
     false
     true)]
    [(does one have a bidirectional edge from two ?)
     (if (or (false? (member (quote one) (node-edges two))) (false? (member (quote two) (node-edges one))))
         false
     true)]
    [(does one have a path from two ?)
     (pathfunc one two)]))



(new graph g0)
(newv vertex n0 in g0)
(newv vertex n1 in g0)
(newv vertex n2 in g0)
(newv vertex n9 in g0)
(edge n0 n1)
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)


(check-expect (pathfunc n2 n9) false)
(check-expect (pathfunc n0 n0) true)
(check-expect (pathfunc n0 n1) true)
(check-expect (pathfunc n9 n2) true)
(check-expect (pathfunc n1 n0) true)




(define (pathfunc nodeone nodetwo)
  (local [(define (pathfuncUnit nodetwo todo visited)
            (cond [(equal? (node-name nodeone) (node-name nodetwo)) true]
                  [(member (node-name nodetwo) visited) (pathfuncList todo visited)]
                  [else (pathfuncList (append (node-edges nodetwo) todo)
                                      (cons (node-name nodetwo) visited))]))
          (define (pathfuncList todo visited)
            (cond [(empty? todo) false]
                  [else
                   (pathfuncUnit (my-eval (first todo)) (rest todo) visited)])
                   )]
    (pathfuncUnit nodetwo empty empty)))
          






(define x 3)
(define y 5)
(set! y 5)









(define z (list y))

(set! y 8)










(define z2 (list (quote y)))
(set! y 11)








(test)