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






(define g1 (make-graph 'g1 '(v1 v2)))











(check-expect (add-unique g1 'v1) 
              "did not insert duplicate value")


(check-expect (add-unique g1 'v3) 
              (make-graph 'g1 '(v1 v2 v3)))

(define (add-unique graph node)
  (if (member node (graph-nodes graph))
      "did not insert duplicate value"
      (begin (set-graph-nodes! graph
             (append (graph-nodes graph) (list node))) graph)))



(define-syntax create
  (syntax-rules()
    [(create x val)
     (begin (define x val) (set! x val) x)]))








(define-syntax new
  (syntax-rules (graph node in vertex edges)
    [(new graph name)
     (create name (make-graph (quote name) empty))]
    [(new vertex name in graph-name)
     (begin (create name (make-node (quote name) empty))
            (add-unique graph-name (quote name)))]
    ))





(define-syntax edge
  (syntax-rules ()
    [(edge n0 n1)
     (set-node-edges! n0 (if (not (member (node-name n1) (node-edges n0)))
                             (append (node-edges n0) (list (node-name n1)))
                             (node-edges n0)))]
    ))










(define-syntax edges
  (syntax-rules(-> <-> <-)
    [(edges n0 -> n1)
     (edge n0 n1)]
    [(edges n0 <- n1)
    (edge n1 n0)]
    [(edges n0 <-> n1)
     (begin (edge n0 n1)
            (edge n1 n0))]
    [(edges n0 arrow1 n1 arrow2 ...)
     (begin (edges n0 arrow1 n1) (edges n1 arrow2 ...))
     ]))



(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(new vertex n4 in g0)
(new vertex n9 in g0)
(edges n0 <-> n1)
(edge n1 n2)
(edge n2 n3)
(edge n3 n4)

(new graph g3)
(new vertex j0 in g3)
(new vertex j1 in g3)
(new vertex j2 in g3)
(new vertex j3 in g3)
(new vertex j4 in g3)
(edges j0 -> j1 <-> j2 -> j3 -> j0)








(define (path? to from)
  (local [(define (path?-node to from todo visited)
    (cond [(equal? (node-name to) (node-name from)) true]
          [(member (node-name from) visited) (path?-lon todo visited)]
          [else (path?-lon (append (node-edges from) todo) (cons (node-name from) visited))]))

(define (path?-lon todo visited)
(cond [(empty? todo) false]
      [else 
       (path?-node to (my-eval (first todo)) (rest todo) visited)]))]

  (path?-node to from empty empty)))


(check-expect (path? n0 n1) true)

(check-expect (path? n9 n1) false)

(check-expect (path? n4 n0) true)



(check-expect (path? j0 j0) true)

(check-expect (path? j4 j0) false)









(define-syntax does
  (syntax-rules (have an a edge bidirectional from ?)
    [(does n1 have an edge from n0 ?)
     (if (member (node-name n1) (node-edges n0))
         true
         false)]
    [(does n1 have a bidirectional edge from n0 ?)
     (and (does n0 have an edge from n1 ?)
          (does n1 have an edge from n0 ?))]
    [(does n1 have a path from n0 ?)
     (path? n1 n0)]))




(check-expect (does n0 have an edge from n1 ?) true)
(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have a bidirectional edge from n1 ?) true)


(check-expect (does n9 have an edge from n2 ?) false)


(check-expect (does n2 have an edge from n1 ?) true)
(check-expect (does n1 have an edge from n2 ?) false)
(check-expect (does n2 have a bidirectional edge from n1 ?) false)


(check-expect (does n0 have a path from n1 ?) true)

(check-expect (does n9 have a path from n1 ?) false)

(check-expect (does n4 have a path from n0 ?) true)


(check-expect (does j0 have a path from j0 ?) true)

(check-expect (does j4 have a path from j1 ?) false)






(define (path to from)
  (local [(define (path?-node to from todo visited pathlst)
            (cond [(equal? (node-name to) (node-name from))
                   (make-path-string (append pathlst
                                             (list (node-name to))))]
                  [(member (node-name to) (node-edges from))
                   (make-path-string (append pathlst
                                             (list (node-name from))
                                             (list (node-name to))))]
                  [(member (node-name from) visited)
                   (path?-lon todo visited pathlst)]
                  [else
                   (path?-lon (append todo (node-edges from))
                              (cons (node-name from) visited)
                              (append pathlst (list (node-name (my-eval from)))))]))

          (define (path?-lon todo visited pathlst)
            (cond [(empty? todo) (make-path-string empty)]
                  [else 
                   (path?-node to (my-eval (first todo))
                               (rest todo) visited pathlst)]))]
    
    (path?-node to from empty empty empty)))




(define (make-path-string lop)
  (local [(define (make-path-string lop rsf)
            (cond [(empty? lop) rsf]
                  [else
                   (make-path-string
                    (rest lop)
                    (string-append rsf " -> " (symbol->string (first lop))))]))]
    
    (make-path-string (rest lop) (symbol->string (first lop)))))


(new graph g2)
(new vertex x0 in g2)
(new vertex x1 in g2)
(new vertex x2 in g2)
(new vertex x3 in g2)
(new vertex x4 in g2)
(new vertex x5 in g2)
(edges x0 -> x1)
(edges x0 -> x4)
(edges x1 -> x2)
(edges x2 -> x4)
(edges x4 -> x5)
(define lon (list 'x0 'x1 'x2 'x3))




(define (reset-list node lon)
  (local [(define (reset-list node lon rsf)
            (cond [(empty? lon) empty]
                  [(equal? (my-eval (first lon)) node)
                   (append rsf (list (node-name (my-eval (first lon)))))]
                  [else
                   (reset-list node (rest lon)
                    (append rsf (list (node-name (my-eval (first lon))))))]))]
    
    (reset-list node lon empty)))
    

(check-expect (path x4 x0) "x0 -> x4")
(check-expect (path x4 x1) "x1 -> x2 -> x4")

(test)