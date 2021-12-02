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
    [(create identifier value)
     (begin (define identifier value)
            (set! identifier value))]))



(define-syntax new 
  (syntax-rules (graph vertex in node)
    [(new graph name)
     (create name (make-graph (quote name) '()))]
    [(new graph name nodes)
     (create name (make-graph (quote name) nodes))]
    [(new node name)
     (create name (make-node (quote name) '()))]
    [(new node name edges)
     (create name (make-node (quote name) edges))]
    [(new vertex a-node in a-graph)
     (add-unique a-node a-graph)]))






(check-expect(begin(add-unique n0 g2)g2) (make-graph 'g2 '(n1 n0)))

(check-expect(begin(add-unique n4 g1)g1) (make-graph 'g1 '(n4)))

(check-expect(begin(add-unique n5 g2)g2)
             (make-graph 'g2 '(n5 n1 n0)))

(define (add-unique a-node a-graph)
  (if (member (node-name a-node) (graph-nodes a-graph))
      (void)
      (set-graph-nodes! a-graph
                        (cons (node-name a-node)
                              (graph-nodes a-graph)))))



(define-syntax edge
  (syntax-rules ()
    [(edge start-node end-node)
     (if (member (quote end-node) (node-edges start-node))
         (void)
         (set-node-edges! start-node
                          (cons (quote end-node) (node-edges start-node))))]))


(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges start-node -> end-node)
     (edge start-node end-node)]
    [(edges start-node <- end-node)
     (edge end-node start-node)]
    [(edges start-node <-> end-node)
     (begin (edge start-node end-node)
            (edge end-node start-node))]
    [(edges start-node arrow-a end-node arrow-b ...)
     (begin (edges start-node arrow-a end-node)
            (edges end-node arrow-b ...))]))


(check-expect (does n4 have an edge from n0 ?) #f)

(check-expect (does n0 have an edge from n4 ?) #t)

(check-expect (does n0 have a bidirectional edge from n1 ?) #t)

(check-expect (does n1 have a bidirectional edge from n0 ?) #t)

(check-expect (does n4 have a bidirectional edge from n0 ?) #f)

(check-expect (does n2 have a bidirectional edge from n4 ?) #f)

(check-expect (does n1 have a path from n3 ?) #f)

(check-expect (does n2 have a path from n0 ?) #f)

(check-expect (does n0 have a path from n4 ?) #t)

(define-syntax does
  (syntax-rules (does have a an bidirectional edge path from ?)
    [(does end-node have an edge from start-node ?)
     (list? (member (node-name end-node) (node-edges start-node)))]
    [(does end-node have a bidirectional edge from start-node ?)
     (and (does end-node have an edge from start-node ?)
          (does start-node have an edge from end-node ?))]
    [(does end-node have a path from start-node ?)
     (has-path? start-node end-node)]))


(check-expect (has-path? n3 n1) #f)

(check-expect (has-path? n0 n2) #f)

(check-expect (has-path? n4 n1) #t)

(check-expect (has-path? n4 n0) #t)

(check-expect (has-path? n4 n4) #t)

(check-expect (has-path? n2 n2) #f) 




(define (has-path? start-node end-node) 
  (local [(define visited empty)
          (define (has-path?--helper this-node)
            (ormap (lambda (this-node)
                     (cond
                       [(member this-node visited) #f]
                       [(equal? (my-eval this-node) (my-eval end-node)) #t]
                       [else (begin (set! visited (cons this-node visited))
                                    (has-path?--helper this-node))]))
                   (node-edges (my-eval this-node))))]
    (has-path?--helper start-node)))




(check-expect (path n0 n4) "n0 -> n1 -> n4") 
(check-expect (path n4 n0) "n4 -> n0")
(check-expect (path n2 n4) "n2 -> n0 -> n1 -> n4") 
(check-expect (path n3 n4) "") 

(define (path start-node end-node)
  (local
    [(define path-worklist empty)
     (define (add-to-path this-path nodes)
       (map cons nodes (make-list (length nodes) this-path)))
     (define (path--helper paths)
       (local [(define path-to-end
                 (ormap (lambda (this-path)
                          (cond
                            [(equal? (car this-path)
                                     (node-name end-node)) this-path]
                            [else (begin (set! path-worklist
                                        (append
                                         (add-to-path this-path (node-edges
                                          (my-eval
                                           (first this-path)))) path-worklist))
                                  #f)
                                  ]))
                        paths))]
         (if (not path-to-end)
             (path--helper path-worklist)
             path-to-end)))]
    (if (has-path? start-node end-node) 
        (string-join (map symbol->string (reverse
          (path--helper (add-to-path
                          (list (node-name start-node))
                          (node-edges (my-eval start-node)))))) " -> ")
        "")))


(new graph g0)
(new graph g1)
(new graph g2)
(new node n0)
(new node n1 '(n0))
(new node n2)
(new node n3)
(new node n4)
(new node n5)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(new vertex n0 in g2)
(new vertex n1 in g2)








(edges n2 -> n0 <-> n1 -> n4 -> n0 -> n3)



(test)