



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
    [(create name val) (begin (define name -1)
                              (set! name val))]
    ))


(define-syntax new
  (syntax-rules (graph in vertex)
    [(new graph name) (create name (make-graph (quote name) empty))]
    [(new vertex name in gname) (begin 
                                  (create name (make-node (quote name) empty))
                                  (set-graph-nodes!
                                   g0
                                   (add-unique (graph-nodes gname)
                                               (quote name))))]
    ))





(define lon0 '(1 2 3))
(define lon1 '(1 2 3 4))

(define (add-unique lox x)
         (cond [(not (foldr 
                      (lambda (one two) (or (equal? one x) two))
                      false
                      lox))
                
                (append lox (cons x empty))]
               [else lox]))
               

(check-expect (add-unique empty 1)'(1))
(check-expect (add-unique lon0 4)'(1 2 3 4))
(check-expect (add-unique lon1 4)'(1 2 3 4))






(define-syntax edge
  (syntax-rules ()
    [(edge node0 node1)
     (set-node-edges! node0 (add-unique (node-edges node0) (quote node1)))]
    )
  )





(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges n0 -> n1) (edge n0 n1)]
    [(edges n0 <- n1) (edge n1 n0)]
    [(edges n0 <-> n1) (begin (edge n1 n0)
                              (edge n0 n1))]
    [(edges n0 arrow0 n1 arrow1 ...) (begin (edges n0 arrow0 n1)
                                            (edges n1 arrow1 ...))]
    )
  )



(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(new vertex n4 in g0)
(new vertex n5 in g0)
(new vertex n6 in g0)





(edges n0 <-> n1 -> n2 -> n3 <- n4)
(edges n6 -> n2)
(edges n6 -> n3)
(edges n6 -> n4)
(edges n5 -> n5)










  

(define-syntax does
  (syntax-rules (bidirectional path ?)
    [(does n0 blah1 blah2 bidirectional blah3 blah4 n1 ?) 
     (not (false? (and (member (quote n0) (node-edges n1))
                       (member (quote n1) (node-edges n0)))))]
    [(does n0 blah1 blah2 path blah4 n1 ?) (path? (quote n0) (quote n1))]
    [(does n1 blah1 blah2 blah3 blah4 n0 ?)
     (not (false? (member (quote n1) (node-edges n0))))]
    )
  ) 

(check-expect (does n0 have a bidirectional edge from n1 ?) true)

(check-expect (does n1 have a bidirectional edge from n0 ?) true)

(check-expect (does n3 have a bidirectional edge from n4 ?) false)
(check-expect (does n4 have a bidirectional edge from n3 ?) false)

(check-expect (does n0 have an edge from n1 ?) true)

(check-expect (does n0 have an edge from n4 ?) false)

(check-expect (does n3 have an edge from n4 ?) true)

(check-expect (does n4 have an edge from n3 ?) false)

(check-expect (does n3 have an edge from n3 ?) false) 

(check-expect (does n6 have an edge from n2 ?) false)
(check-expect (does n2 have an edge from n6 ?) true)

(check-expect (does n5 have an edge from n5 ?) true)







(define (path? start-node end-node)
  (local [
          (define (path-n? node bool-so-far final-node checked)
            (cond
              [(member node checked) #false]
              [(equal? node final-node) #true]
              [else (path-lon?
                     (node-edges (my-eval node))
                     bool-so-far
                     final-node
                     (cons node checked))]
              )
            )
          
          (define (path-lon? lon bool-so-far final-node checked)
            (cond
              [(empty? lon) bool-so-far]
              [else (path-lon? (rest lon) 
                                (or (path-n?
                                     (first lon)
                                     bool-so-far
                                     final-node
                                     checked) bool-so-far)
                                final-node 
                                checked)]
              )
            )
          ]
    (path-n? start-node false end-node empty))
  )


(check-expect (path? (quote n1) (quote n3)) true)

(check-expect (path? (quote n2) (quote n4)) false)


(check-expect (path? (quote n5) (quote n5)) true)

(check-expect (does n1 have a path from n3 ?) true)
(check-expect (does n2 have a path from n4 ?) false)
(check-expect (does n5 have a path from n5 ?) true)




(test)