

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












(check-expect (add-unique empty) empty)
(check-expect (add-unique (list 'n0 'n0 'n1)) (list 'n0 'n1))
(check-expect (add-unique (list 'n0)) (list 'n0))

(define (add-unique los)
  (local [(define (add-uniquenode los added)
            (cond [(empty? los) empty]
                  [(member (first los)  added) (add-uniquenode (rest los) added)]
                  [else (cons (first los)
                             (add-uniquenode (rest los) (cons (first los) added)))]))]
    (add-uniquenode los empty)))



(define-syntax create
  (syntax-rules ()
    [(create name value)
     (begin (define name value)
            (set! name value))]))




(define-syntax new 
  (syntax-rules (graph vertex in)
    [(new graph name nodes)
    (create name (make-graph (quote name) (add-unique nodes)))]
     [(new vertex node in graph1)
     (begin (new graph graph1 (cons (quote node) (graph-nodes graph1)))
           (create node (make-node (quote node) empty)))]))





(define-syntax edge
  (syntax-rules ()
    [(edge node1 node2)
     (create node1 (make-node (quote node1) (add-unique (cons (quote node2)(node-edges node1)))))]))



(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges node1 -> node2)
     (edge node1 node2)]
    [(edges node1 <- node2)
     (edge node2 node1)]
    [(edges node1 <-> node2)
     (begin (edge node1 node2)
     (edge node2 node1))]
    [(edges node1 op node2 op2 ...)
     (begin (edges node1 op node2)
            (edges node2 op2 ...))]))




(begin (define g0 (make-graph 'g0 '(n0 n1 n2)))
       (define n0 (make-node 'n0 '(n1)))
       (define n1 (make-node 'n1 '(n0)))
       (define n2 (make-node 'n2 empty))
       (define n3 (make-node 'n3 '(n1)))
       (define n4 (make-node 'n4 '(n5 n1)))
       (define n5 (make-node 'n5 '(n4))))




(check-expect (does n0 have an edge from n1 ?) true)
(check-expect  (does n1 have an edge from n2 ?) false)
(check-expect  (does n0 have a bidirectional edge from n1 ?) true)
(check-expect (does n0 have a path from n1 ?) true)
(check-expect  (does n1 have a path from n2 ?) false)
(check-expect  (does n0 have a path from n3 ?) true)
(check-expect (does n0 have a path from n4 ?) true)


(define-syntax does
  (syntax-rules (have a an bidirectional edge from ? path)
    [(does node2 have an edge from node1 ?)
     (not (false? (member (quote node2) (node-edges node1))))]
    [(does node2 have a bidirectional edge from node1 ?)
     (and (not (false? (member (quote node2) (node-edges node1))))
          (not (false? (member (quote node1) (node-edges node2)))))]
    [(does node2 have a path from node1 ?)
     (path node1 node2)]))







(check-expect (path n2 n0) false)
(check-expect (path n1 n0) true)
(check-expect (path n3 n0) true)
(check-expect (path n4 n0) true)

(define (path primary final)
  (local [(define (pathnode node todo visited)
            (cond[(equal? node final) true]
                 [(member node visited) (pathlon todo visited)]
                 [else (pathlon (append (node-edges node) todo) (cons node visited))]))
          (define (pathlon todo visited)
            (cond [(empty? todo) false]
                  [else (pathnode (my-eval (first todo)) (rest todo) visited)]))]
    (pathnode primary empty empty)))



(test)