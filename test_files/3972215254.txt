
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










(define-syntax create
  (syntax-rules ()
    [(create name value)
     (begin
       (define name (void))
       (set! name value))]))



(define (add-unique name lst)
  (if (member name lst)
      lst
      (cons name lst)))

(define test-list (list 1 2 3 4))
(check-expect (add-unique 2 test-list) (list 1 2 3 4))
(check-expect (add-unique 5 test-list) (list 5 1 2 3 4))
(check-expect (add-unique 2 empty) (list 2))
(check-expect (add-unique false (list 2 true)) (list false 2 true))





(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph (quote name) empty))
     ]
    [(new vertex name in g)
     (begin
       (create name (make-node (quote name) empty))
       (set-graph-nodes! g (add-unique (quote name) (graph-nodes g))))
     ]))

(define-syntax edge
  (syntax-rules ()
    [(edge d0 d1)
     (set-node-edges! d0 (add-unique (quote d1) (node-edges d0)))
     ]))









(define-syntax edges
  (syntax-rules (<- -> <->)
    [(edges n1 <- n2)
     (edge n2 n1)
    ]
    [(edges n1 -> n2)
     (edge n1 n2)
    ]
    [(edges n1 <-> n2)
     (begin
       (edge n1 n2)
       (edge n2 n1)
     )
    ]
    [(edges n1 op1 n2 op2 ...)
     (begin
       (edges n1 op1 n2)
       (edges n2 op2 ...))]
    ))


(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
 

(edges n0 -> n1 <-> n2 -> n0)







(define-syntax does
  (syntax-rules (have a an edge bidirectional from ? path)
    [(does n0 have an edge from n1 ?)
     (if (member (quote n0) (node-edges n1)) true false)]
    [(does n0 have a bidirectional edge from n1 ?)
     (if (and
      (does n0 have an edge from n1 ?)
      (does n1 have an edge from n0 ?))
         true
         false)]
    [(does n0 have a path from n1 ?)
     (reachable? n1 n0)]
    ))

(new graph g1)
(new vertex gn0 in g1)
(new vertex gn1 in g1)
(new vertex gn2 in g1)
(new vertex gn3 in g1)
(edges gn0 <-> gn1)
(edges gn2 -> gn3)

(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n2 have a bidirectional edge from n1 ?) true)
(check-expect (does gn3 have a bidirectional edge from gn2 ?) false)
(check-expect (does gn1 have a bidirectional edge from gn0 ?) true)
(check-expect (does gn2 have an edge from gn3 ?) false)
(check-expect (does gn3 have an edge from gn2 ?) true)

(new graph g2)
(new vertex gt0 in g2)
(new vertex gt1 in g2)
(new vertex gt2 in g2)
(new vertex gt3 in g2)
(new vertex gt4 in g2)
(new vertex gt5 in g2)
(new vertex gt6 in g2)
(edges gt0 -> gt1 -> gt2 -> gt3 -> gt4 <-> gt5 <-> gt6 -> gt1)





(define (reachable? node0 node1)
  
  
  
  (local [(define (fn-for-node n todo visited)
            (cond [(equal? n (node-name node1))
                   true]
                  [(member n visited)
                   (fn-for-lon todo visited)]
                  [else
                   (fn-for-lon (append (node-edges (my-eval n)) todo) (cons n visited))]))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (first todo)
                                (rest todo)
                                visited)]))]
    (fn-for-node node0 empty empty)))


(check-expect (reachable? n0 n2) true)
(check-expect (reachable? n0 n1) true)
(check-expect (reachable? n1 n0) true)

(check-expect (reachable? gn0 gn3) false)
(check-expect (reachable? gn3 gn2) false)
(check-expect (reachable? gn0 gn1) true)



(define (path node0 node1)
  
  
  
  
  (local [
          (define (fn-for-node n path-so-far todo visited all-paths)
            (cond [(equal? n (node-name node1))
                   path-so-far
                   ]
                  [(member n visited)
                   (fn-for-lon todo visited all-paths)]
                  [else
                   (fn-for-lon (append todo (node-edges (my-eval n)))
                               (cons n visited)
                               (append all-paths
                                       (map (λ (val)
                                              (string-append path-so-far " -> " (symbol->string val)))
                                            (node-edges (my-eval n))
                                       )
                               ))]))
          (define (fn-for-lon todo visited all-paths)
            (cond [(empty? todo) ""]
                  [else
                   (fn-for-node (first todo)
                                (first all-paths)
                                (rest todo)
                                visited
                                (rest all-paths))]))]
    (fn-for-node node0 (symbol->string (node-name node0)) empty empty empty)))

(check-expect (path n0 n2) "n0 -> n1 -> n2")
(check-expect (path n2 n1) "n2 -> n1")
(check-expect (path gn0 gn3) "")
(check-expect (path gt1 gt6) "gt1 -> gt2 -> gt3 -> gt4 -> gt5 -> gt6")
(check-expect (path gt6 gt5) "gt6 -> gt5") 
(check-expect (path gt6 gt4) "gt6 -> gt5 -> gt4")



(check-expect (does n2 have a path from n0 ?) true)
(check-expect (does n1 have a path from n0 ?) true)
(check-expect (does n0 have a path from n1 ?) true)
(check-expect (does gn3 have a path from gn0 ?) false)
(check-expect (does gn2 have a path from gn3 ?) false)
(check-expect (does gn1 have a path from gn0 ?) true)

(test)