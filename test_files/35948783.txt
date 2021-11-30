




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





(define-syntax create
  (syntax-rules () [(create name val)
                    (begin (define name val)
                           (set! name val))]))


(define-syntax new
  (syntax-rules (graph vertex in)
    
    [(new graph name)
     (create name (make-graph (quote name) '()))]
    
    [(new vertex node in actualGraph)
     (begin (create node (make-node (quote node) '()))
            (add-unique (node-name node) actualGraph))]))



(new graph gt0)

(check-expect (begin (add-unique 'n0 gt0)
                     (graph-nodes gt0))
              '(n0)) 

(check-expect (begin (add-unique 'n0 gt0)
                     (graph-nodes gt0))
              '(n0)) 

(define (add-unique node graph)
  (if (false? (member node (graph-nodes graph)))
      (set-graph-nodes! graph (cons node (graph-nodes graph)))
      (void)))





(define-syntax edge
  (syntax-rules ()
    [(edge node0 node1)
     (if (false? (member (node-name node1) (node-edges node0)))
         (set-node-edges! node0 (cons (quote node1) (node-edges node0)))
         (void))]))

(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)


(define-syntax edges
  (syntax-rules (<- -> <->)
    [(edges node0 -> node1)
     (edge node0 node1)]
    [(edges node0 <- node1)
     (edge node1 node0)]
    [(edges node0 <-> node1)
     (begin (edge node0 node1)
            (edge node1 node0))]
    [(edges node0 fn node1 fn2 ...)
     (begin (edges node0 fn node1)
            (edges node1 fn2 ...))]))





(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)


(define-syntax does
  (syntax-rules (have an edge from ? a bidirectional path)
    [(does node0 have an edge from node1 ?)
     (not (false? (member (node-name node0) (node-edges node1))))]
    [(does node0 have a bidirectional edge from node1 ?)
     (and (does node0 have an edge from node1 ?)
          (does node1 have an edge from node0 ?))]
    
    [(does node0 have a path from node1 ?)
     (find-path (my-eval node1) (my-eval node0))]))

(check-expect (does n1 have an edge from n0 ?) true) 
(check-expect (does n0 have an edge from n1 ?) false) 
(check-expect (does n2 have an edge from n9 ?) false) 
(check-expect (does n2 have a bidirectional edge from n1 ?) true) 

(check-expect (does n2 have an edge from n0 ?) false) 
(check-expect (does n2 have a path from n0 ?) true) 
(check-expect (does n2 have a path from n9 ?) false) 
(check-expect (does n9 have a path from n1 ?) true) 









(check-expect (find-path n0 n2) true)
(check-expect (find-path n2 n9) true)
(check-expect (find-path n9 n1) false)

(define (find-path fromNode findNode)
  (local [
          
          (define (fn-for-node n visited todo)
            (if (member n visited)
                (fn-for-lon visited todo)
                (if (equal? n findNode)
                    true
                    (fn-for-lon (append (node-edges n) todo) (cons n visited)))))
          
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else (fn-for-node (my-eval(first todo)) visited (rest todo))]))
          ]
    (fn-for-node fromNode empty empty)))











(check-expect (path n0 n2) "n0 -> n1 -> n2")
(check-expect (path n2 n9) "n2 -> n9")
(check-expect (path n9 n0) "")

(define (path fromNode findNode)
  (local [
          
          (define (fn-for-node n visited todo results)
            (if (member n visited)
                (fn-for-lon visited todo results)
                (if (equal? n (my-eval findNode))
                    (inversePathToString results)
                    (fn-for-lon (append todo (node-edges n)) (cons n visited) results))))
          
          (define (fn-for-lon todo visited results)
            (cond [(empty? todo) ""]
                  [else
                   (fn-for-node
                    (my-eval (first todo))
                    visited
                    (rest todo)
                    (cons (first todo) results))]))
          ]
    (fn-for-node (my-eval fromNode) empty empty (list (node-name (my-eval fromNode))))))







(check-expect (inversePathToString '(n2 n1 n9 n0)) "n0 -> n1 -> n2")
(check-expect (inversePathToString '(n9 n2 n1)) "n1 -> n2 -> n9")
(check-expect (inversePathToString '()) "")

(define (inversePathToString path)
  (cond [(empty? path) ""]
        [(= (length path) 1) (symbol->string (node-name (my-eval (first path))))]
        [else
         (if (does (my-eval(first path)) have an edge from (my-eval(second path)) ?)
             (string-append
              (inversePathToString (rest path))
              " -> " (symbol->string (node-name (my-eval (first path)))))
      
             (inversePathToString (cons (first path) (rest (rest path)))))]))

(test)