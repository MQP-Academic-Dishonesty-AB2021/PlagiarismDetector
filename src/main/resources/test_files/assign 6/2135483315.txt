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






(define (add-unique el lst) (if (false? (member el lst)) (cons el lst) lst))
(check-expect (add-unique 6 (list 9)) (list 6 9))
(check-expect (add-unique 6 (list 6 9)) (list 6 9))

(define-syntax-rule 
  (create name value)
  (begin (define name value)
         (set! name value)))



(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph 'name empty))]
    [(new vertex name in gname)
     (begin (create name (make-node 'name empty))
            (set-graph-nodes! gname (add-unique 'name (graph-nodes gname))))]))

(define-syntax-rule (edge a b) (set-node-edges! a (add-unique 'b (node-edges a))))


(define-syntax edges
  (syntax-rules (<- <-> ->)
    [(edges a <- b) (edge b a)]
    [(edges a <-> b) (begin (edge a b) (edge b a))]
    [(edges a -> b) (edge a b)]
    [(egdes a func b rst ...)
     (begin (edges a func b) (edges b rst ...))]))



(define-syntax does
  (syntax-rules (have an a bidirectional edge path from ?)
    [(does node1 edge node2)
     (not (false? (member 'node1 (node-edges node2))))]
    [(does node1 bidirectional node2)
     (and (does node1 edge node2) (does node2 edge node1))]
    [(does node1 path node2) (is-path 'node1 'node2)]
    [(does node1 have an edge from node2 ?)
     (does node1 edge node2)]
    [(does node1 have a bidirectional edge from node2 ?)
     (does node1 bidirectional node2)]
    [(does node1 have a path from node2 ?)
     (does node1 path node2)]))

(define-syntax-rule (path node1 node2)
  (get-path 'node1 'node2))

(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(edges n0 -> n1 <-> n2 -> n0)

(check-expect (does n1 have an edge from n0 ?) #t)
(check-expect (does n2 have an edge from n0 ?) #f)

(check-expect (does n1 have an edge from n0 ?) #t)
(check-expect (does n0 have an edge from n1 ?) #f)
(check-expect (does n2 have a bidirectional edge from n1 ?) #t)
(check-expect (does n0 have a bidirectional edge from n1 ?) #f)

(check-expect (does n2 have a path from n0 ?) #t)
(check-expect (does n0 have a path from n3 ?) #f)


(new graph g1)
(new vertex N0 in g1)
(new vertex N1 in g1)
(new vertex N2 in g1)
(new vertex N9 in g1)
(edge N0 N1)
(edge N0 N1)

(edges N0 -> N1 <-> N2 -> N0 -> N9 <- N2)


(define (is-path start end)
  (local [(define (fn-node node todo done)
            (cond
              [(member node done) (fn-list todo done)]
              [(equal? node end) #t]
              [else (fn-list (append (node-edges (my-eval node)) todo) (cons node done))]))
          (define (fn-list todo done)
            (if (empty? todo) #f
                (fn-node (first todo) (rest todo) done)))]
    (fn-node start empty empty)))

(check-expect (is-path 'n0 'n1) #t)
(check-expect (is-path 'n0 'n3) #f)


(define (get-path start end)
  (local [(define (node-path node psf)
            (cond [(equal? node end)
                   (string-append psf (symbol->string node))]
                  [(not (is-path node end)) ""]
                  [else
                   (list-path (node-edges (my-eval node))
                             (string-append psf (symbol->string node) " -> "))]))
          (define (list-path lon psf)
            (cond [(empty? lon) ""]
                  [(non-empty-string? (list-path (rest lon) psf))
                   (list-path (rest lon) psf)]
                   [else (if (not(false? (node-path (first lon) psf)))
                     (node-path (first lon) psf)
                     #f)]))]
    (node-path start "")))

(check-expect (get-path 'n0 'n2) "n0 -> n1 -> n2")
(check-expect (get-path 'n0 'n3) "")
(test)
