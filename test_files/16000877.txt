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
    [(create name value)
     (begin
       (define name value)
       (set! name value))]))




(check-expect (add-unique (list (make-node "2" '()) (make-node "1" '())) (make-node "1" '()))
              (list (make-node "2" '()) (make-node "1" '())))
(check-expect (add-unique (list (make-node "2" '()) (make-node "3" '())) (make-node "1" '()))
              (list (make-node "1" '()) (make-node "2" '()) (make-node "3" '())))
(check-expect (add-unique '()  (make-node "1" '())) (list  (make-node "1" '())))


(define (add-unique lon node)
  (cond [(member node lon) lon]
        [else (cons node lon)]))



(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create
      name
      (make-graph (quote name) (quote ())))]
    [(new vertex n in g)
     (begin
       (create
        n
        (make-node (quote n) (quote ())))
       (set-graph-nodes! g (add-unique (graph-nodes g) (quote n))))]))






(define-syntax edge
  (syntax-rules ()
    [(edge n0 n1)
     (set-node-edges! n0 (add-unique (node-edges n0) (quote n1)))]))



(define-syntax edges
  (syntax-rules (<-> <- ->)
    [(edges n1 <-> n2)
     (begin
       (edge n1 n2)
       (edge n2 n1))]
    [(edges n1 <- n2)
     (edge n2 n1)]
    [(edges n1 -> n2)
     (edge n1 n2)]
    [(edges n1 op1 n2 op2 ...)
     (begin
       (edges n1 op1 n2)
       (edges n2 op2 ...))]))




(new graph g0)
(new vertex v0 in g0)
(new vertex v1 in g0)
(new vertex v2 in g0)
(new vertex v3 in g0)
(new vertex v4 in g0)
(new vertex v5 in g0)
(edges v0 -> v1 -> v2 <-> v3 -> v4 -> v5 <-> v1)





(check-expect (does v2 have an edge from v1 ?) #true)
(check-expect (does v4 have an edge from v5 ?) #false)
(check-expect (does v5 have an edge from v1 ?) #true)


(check-expect (does v5 have a bidirectional edge from v1 ?) #true)
(check-expect (does v3 have a bidirectional edge from v4 ?) #false)


(check-expect (does v2 have a path from v0 ?) #true)
(check-expect (does v0 have a path from v2 ?) #false)
(check-expect (does v2 have a path from v1 ?) #true)
(check-expect (does v5 have a path from v2 ?) #true)
(check-expect (does v0 have a path from v1 ?) #false)

(define-syntax does
  (syntax-rules (have an a edge path bidirectional from ?)
    [(does n2 have an edge from n1 ?)
     (not (false? (member (quote n2) (node-edges n1))))]
    [(does n2 have a bidirectional edge from n1 ?)
     (and
      (not (false? (member (quote n2) (node-edges n1))))
      (not (false? (member (quote n1) (node-edges n2)))))]
    [(does n2 have a path from n1 ?)
     (not (false? (lookup-node (quote n1) (quote n2))))]))




(check-expect (lookup-node (quote v0) (quote v2)) (quote v2))
(check-expect (lookup-node (quote v5) (quote v3)) (quote v3))
(check-expect (lookup-node (quote v1) (quote v0)) #false)
(check-expect (lookup-node (quote v2) (quote v0)) #false)





(define (lookup-node start-node target) 
  
  
  (local [(define (fn-for-node n todo visited) 
            (cond [(equal? (my-eval n) (my-eval target)) n]
                  [(member n visited)
                   (fn-for-lon todo visited)]
                  [else
                   (fn-for-lon (append (node-edges (my-eval n)) todo)
                               (cons n visited))]))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-node start-node empty empty))) 

(test)


