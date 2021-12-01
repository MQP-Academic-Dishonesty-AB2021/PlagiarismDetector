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







(define n1 (make-node (quote n1) empty))
(define n2 (make-node (quote n2) (list (quote n1))))
(define n3 (make-node (quote n3) (list (quote n2))))

(define g1 (make-graph (quote g1) empty))
(define g2 (make-graph (quote g2) (list (quote n1))))
(define g3 (make-graph (quote g3) empty))






(define (add-unique n g)
  (if (not (member n (graph-nodes g)))
      (begin
        (set-graph-nodes! g (cons n (graph-nodes g)))
        g)
      g))






















(define-syntax edge
  (syntax-rules ()
    [(edge n1 n2)
     (if (not (member (quote n2) (node-edges n1)))
         (set-node-edges! n1 (cons (quote n2) (node-edges n1)))
         (void))]))





(define-syntax edges
  (syntax-rules (<- <-> ->)
    [(edges e1 <- e2)
     (edge e2 e1)]
    [(edges e1 -> e2)
     (edge e1 e2)]
    [(edges e1 <-> e2)
     (begin (edge e1 e2)
            (edge e2 e1))]
    [(edges e1 op e2 op2 e3 ...)
     (begin (edges e1 op e2)
            (edges e2 op2 e3 ...))]))







(define-syntax create
  (syntax-rules ()
    [(create x y)
     (begin
       (define x y)
       (set! x y))]))








(define-syntax new
  (syntax-rules (node graph vertex in)
    
    
    [(new node n)
     (begin
       (create n (make-node (quote n) empty))
       n)]
    
    [(new graph g)
     (begin
       (create g (make-graph (quote g) empty))
       g)]
    
    
    
    
    [(new vertex n in g)
     (begin
       (new node n)
    
    
       (add-unique (quote n) g)
       g)]))








(define-syntax does
  (syntax-rules ()
    [(does e1 have an edge from e0 ?)
     (not (false? (member (quote e1) (node-edges e0))))]

    [(does e1 have a bidirectional edge from e0 ?)
     (and
      (member e1 (node-edges e0))
      (member e0 (node-edges e1)))]

    [(does e1 have a path from e0 ?)
     (is-path? e0 e1)]))
      


      
(check-expect (does n1 have an edge from n2 ?) #t)
(check-expect (does n2 have an edge from n1 ?) #f)
(check-expect (does n3 have a bidirectional edge from n2 ?) #f)



(define (all-reachable n)
  (local [(define (all-reachable--node n todo visited)
            (cond [(false? (member n visited))
                   (all-reachable--lon (append (node-edges n) todo) (cons n visited))]
                  [else
                   (all-reachable--lon todo visited)]))
          (define (all-reachable--lon todo visited)
            (cond [(empty? todo) visited]
                  [else
                   (all-reachable--node (my-eval (first todo)) (rest todo) visited)]))]
  (all-reachable--lon (node-edges n) empty)))

(check-expect (all-reachable n3) (list n1 n2))
(check-expect (all-reachable n1) empty)



(define (is-path? n1 n2)
  (not (false? (member n2 (all-reachable n1)))))

(check-expect (is-path? n1 n1) #f)
(check-expect (is-path? n1 n2) #f)
(check-expect (is-path? n2 n1) #t)
(check-expect (is-path? n3 n1) #t)
         


(define x 3)
(define y 5)
(set! y 5)








      

(define z (list y))

(set! y 8)










(define z2 (list (quote y)))
(set! y 11)








(test)