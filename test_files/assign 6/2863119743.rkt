



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
    [(create name x)
     (begin (define name x)
            (set! name x))]))




(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph g)
     (create g (make-graph (quote g) '()))]
    [(new vertex n in g)
     (begin (create n (make-node (quote n) '()))
            
            (set-graph-nodes! g (add-unique (graph-nodes g) (quote n))))]))






(check-expect (add-unique (graph-nodes g1) (quote n0)) '(n0))
(check-expect (add-unique (graph-nodes g0) (quote n0)) '(n9 n2 n1 n0))
(check-expect (add-unique (graph-nodes g0) (quote n9)) '(n9 n2 n1 n0))
(check-expect (add-unique (node-edges n0) (quote n1)) '(n9 n1))
(check-expect (add-unique (node-edges n10) (quote n11)) '(n11))



(define (add-unique lon n)
  (if (member n lon)
      lon
      (cons n lon)))




(define-syntax edge
  (syntax-rules ()
    [(edge n1 n2)
     
     (set-node-edges! n1 (add-unique (node-edges n1) (quote n2)))]))




(define-syntax edges
  (syntax-rules (-> <-> <-)
    [(edges n1 -> n2)
     (edge n1 n2)]
    [(edges n1 <- n2)
     (edge n2 n1)]
    [(edges n1 <-> n2)
     (begin (edge n1 n2)
            (edge n2 n1))]
    [(edges n1 op1 n2 op2 ...)
     (begin (edges n1 op1 n2)
            (edges n2 op2 ...))]))




(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n2 have a bidirectional edge from n1 ?) true)

(check-expect (does n2 have an edge from n0 ?) false)
(check-expect (does n2 have a path from n0 ?) true)

(define-syntax does
  (syntax-rules (have an edge from ?
                      a bidirectional
                      path)
    [(does n1 have an edge from n2 ?)
     (if (member (quote n1) (node-edges n2))
         true
         false)]
    [(does n1 have a bidirectional edge from n2 ?)
     (if (and (member (quote n1) (node-edges n2))
              (member (quote n2) (node-edges n1)))
         true
         false)]
    [(does n1 have a path from n2 ?)
     (has-path? n1 n2)]))





(check-expect (has-path? n2 n0) true)
(check-expect (has-path? n9 n0) false)
(check-expect (has-path? n1 n9) true)



(define (has-path? n1 n2)
  (local [(define (has-path--node node todo visited) 
            (cond [(equal? (my-eval node) n2) true]
                  [(member node visited)
                   (has-path--lon todo visited)]
                  [else
                   (has-path--lon (append (node-edges (my-eval node)) todo)
                                  (cons node visited))]))
          (define (has-path--lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (has-path--node (first todo) 
                                   (rest todo)
                                   visited)]))]
    (has-path--node n1 empty empty)))




 


(new graph g0)
(new graph g1)
(new graph g2)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)
(new vertex n10 in g2)
(new vertex n11 in g2)

(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)

(edges n10 -> n11)


(test)