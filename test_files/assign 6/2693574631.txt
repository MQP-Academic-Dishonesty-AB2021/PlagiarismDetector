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








(test)


(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (begin (define name (make-graph (quote name) empty))
            (create name name))]
    [(new vertex vertex-name in graph-name)
     (begin (define vertex-name (make-node (quote vertex-name) empty)) 
            (create vertex-name vertex-name)
            (create graph-name (make-graph (quote graph-name)
                                           (add-unique (node-name vertex-name)
                                                       (graph-nodes graph-name)))))]))

(define-syntax edge
  (syntax-rules ()
    [(edge start-vertex end-vertex)
     (begin (set-node-edges! start-vertex
                             (add-unique (node-name end-vertex) (node-edges start-vertex)))
            end-vertex)]))


(define-syntax edges
  (syntax-rules (<- -> <->)
    [(edges e1 <- e2)
     (edge e2 e1)]
    [(edges e1 -> e2)
     (edge e1 e2)]
    [(edges e1 <-> e2)
     (begin (edge e1 e2) (edge e2 e1) e2)]
    [(edges e1 dir1 e2 dir2 ...)
     (edges (edges e1 dir1 e2) dir2 ...)]))


(define-syntax create
  (syntax-rules () 
    [(create variable val)
     (set! variable val)]))




(define (add-unique val listofVal) 
  (if (member val listofVal)
      listofVal
      (cons val listofVal)))

(check-expect (add-unique (quote n1) empty)
              (cons (quote n1) empty))
(check-expect (add-unique (quote n1) (cons (quote n1) empty))
              (cons (quote n1) empty))
(check-expect (add-unique (quote n2) (cons (quote n1) empty))
              (cons (quote n2) (cons (quote n1) empty)))
(test)

(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(new vertex n9 in g0)
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)



(define-syntax does
  (syntax-rules (have a bidirectional edge an from path ?)
    [(does e1 have an edge from e2 ?)
     (if (member (quote e1) (node-edges e2))
         true
         false)]
    [(does e1 have a bidirectional edge from e2 ?) 
     (and (does e1 have an edge from e2 ?) (does e2 have an edge from e1 ?))]
    [(does e1 have a path from e2 ?)
     (reachable? e1 e2)]))



(define (reachable? e1 e2)
  (local [(define (fn-for-node node worklist visited)
            (cond
              [(equal? (node-name (my-eval node)) (node-name (my-eval e2))) true]
              [(member node visited) (fn-for-edges worklist visited)]
              [else (fn-for-edges (append (node-edges (my-eval node)) worklist)
                                  (append (list node) visited))]))
      
          (define (fn-for-edges worklist visited)
            (cond [(empty? worklist) false]
                  [else (fn-for-node (first worklist) (rest worklist) visited)]))]
    (fn-for-node e1 empty empty)))

(check-expect (reachable? n0 n1) true)
(check-expect (reachable? n9 n2) false)
(check-expect (reachable? n1 n9) true)    
(test)