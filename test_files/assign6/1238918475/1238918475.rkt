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
    [(create x val)
     (begin
       (define x val)
       (set! x val)
       )
     ]
    )
  )



(define-syntax new
  (syntax-rules (graph in vertex)
    [(new graph x)
     (create x (make-graph (quote x) empty))
     ]
    [(new vertex n in g)
     (begin
       (create n (make-node (quote n) empty))
       (set-graph-nodes! g (add-unique  (quote n) (graph-nodes g)))
       )
       
     ]
    )
  )






(check-expect (add-unique 'a1 '(a1 a2 s2)) '(a1 a2 s2)) 

(check-expect (add-unique 'a1 '(a2 s2)) '(a1 a2 s2)) 

(check-expect (add-unique 'a1 '()) '(a1)) 

(define (add-unique newNode graphNodes)
  (if (member newNode graphNodes)
      
      graphNodes
      
      (cons newNode graphNodes)
      )
  )
  



(new graph g2)
g2
(new vertex ss0 in g2)
g2
(my-eval (quote ss0))




(new graph g0)
g0
(new vertex p0 in g0)
(new vertex t0 in g0)
(new vertex k0 in g0)
(new vertex y0 in g0)
(new vertex r0 in g0)
(new vertex w0 in g0)
g0
(my-eval (quote p0))


(define-syntax edge
  (syntax-rules (-> <-> <- )

    
    [(edge node newEdge)
     (if (member (quote newEdge) (node-edges (my-eval node)))
         
         void
         
         (set-node-edges! (my-eval node) (cons (quote newEdge) (node-edges (my-eval node))))
         )
     ]
    [(edge node -> newEdge)
     
     (if (member (quote newEdge) (node-edges (my-eval node)))
         
         void
         
         (set-node-edges! (my-eval node) (cons (quote newEdge) (node-edges (my-eval node))))
         )
     ]
    [(edge node <-> newEdge)
     (if (or (member (quote node) (node-edges (my-eval newEdge)))
             (member (quote newEdge) (node-edges (my-eval node)))
             )
         
         void
         
         (begin
           (set-node-edges! (my-eval node) (cons (quote newEdge) (node-edges (my-eval node))))
           (set-node-edges! (my-eval newEdge) (cons (quote node) (node-edges (my-eval newEdge))))
           )
         )]
    [(edge node <- newEdge)
     (if  (member (quote node) (node-edges (my-eval newEdge)))
          
          void
          
          (set-node-edges! (my-eval newEdge) (cons (quote node) (node-edges (my-eval newEdge))))
          )]
    [(edge node arrow newEdge arrow2 ...)
     (begin
       (edge node arrow newEdge)
       (edge newEdge arrow2 ...)
       )
     ]
    )
  )









(edge w0 <-> y0)

(check-expect (does w0 have an bidirectional edge from y0 ?) true)

(check-expect (does k0 have an bidirectional edge from y0 ?) false) 

(edge r0 <- k0)
(check-expect (does r0 have an edge from k0 ?) true) 
(check-expect (does w0 have an edge from k0 ?) false) 

(edge t0 -> p0)
(check-expect (does p0 have an edge from t0 ?) true) 
(check-expect (does r0 have an edge from t0 ?) false) 



(check-expect (does a1 have a path from d1 ?) true)

(check-expect (does e1 have a path from b1 ?) true)


(check-expect (does a1 have a path from a1 ?) true)

(check-expect (does z1 have a path from b1 ?) false)

(check-expect (does g1 have a path from b1 ?) false)

(define-syntax does
  (syntax-rules (have an edge from ? bidirectional path a)
    [(does e have an edge from te ?)
     (list?(member (quote e) (node-edges (my-eval te))))
     ]
    [(does e have an bidirectional edge from te ?)
     (list?(and (member (quote e) (node-edges (my-eval te)))
                (member (quote te) (node-edges (my-eval e)))
                ))
     ]
    [(does e have a path from te ?)
     (findPath e te)
     ]
    )
  )







(new graph gra1)

(new vertex a1 in gra1)
(new vertex b1 in gra1)
(new vertex c1 in gra1)
(new vertex d1 in gra1)
(new vertex e1 in gra1)
(new vertex g1 in gra1)
(new vertex z1 in gra1)

(edge a1 -> b1 -> c1 -> d1 -> b1 )
(edge c1 <-> e1)
(edge c1 -> z1)
(edge a1 -> g1)


.


(check-expect (findPath a1 d1) true)
(check-expect (findPath e1 b1) true)
(check-expect (findPath a1 b1) true)

(check-expect (findPath z1 b1) false)
(check-expect (findPath g1 b1) false)




(define (findPath node targetNode)
  
  
  (local [(define (lookAtNode node todo visited)
            (cond
              [(member node visited) (lookAtEdges todo visited)]
              [(equal? node targetNode) true]
              [else
               (lookAtEdges (append (map (λ (x) (my-eval x)) (node-edges node)) todo)
                            (cons  node visited))
               ]
              )
            )
          (define (lookAtEdges todo visited)
            (cond [(empty? todo) false]
                  [else
                   (lookAtNode (first todo)
                               (rest todo)
                               visited)]
                  )
            )
          ]
    (lookAtNode node empty empty)
    )
  )









(check-expect (path a1 a1) "a1")

(check-expect (path a1 g1) "a1->g1")

(check-expect (path a1 z1) "a1->b1->c1->z1")

(check-expect (path e1 c1) "e1->c1")

(check-expect (path e1 b1) "e1->c1->d1->b1")



(define (path node targetNode)
  
  
  (local [(define (lookAtNode node todo visited output)
            (cond
              [(member node visited) (lookAtEdges todo visited output)]
              [(equal? node targetNode) (symbol->string (node-name targetNode))]
              [else
               (lookAtEdges (append todo
                                    (getAllTrues
                                     (getIndexOfTrue
                                      
                                      (map find (map (λ (x) (my-eval x)) (node-edges node)))
                                      0
                                      empty
                                      (cons  node visited)
                                      (node-edges node)
                                      )
                                     (node-edges node)
                                     empty
                                     )
                                    )
                            (cons  node visited)
                            (string-append output (symbol->string (node-name node)) "->")
                            )
               ]
              )
            )
          (define (lookAtEdges todo visited output)
            
            (cond [(empty? todo) ""]
                  [(equal? (first todo) targetNode)
                   (string-append output  (symbol->string (node-name targetNode)))
                   ]
                  [else
                   (lookAtNode (first todo)
                               (rest todo)
                               visited
                               output
                               )]
                  )
              
            )

          (define (getIndexOfTrue listOfBool index indexList visited listOfNode)
            (cond
              [(empty? listOfBool) indexList]
              [(and
                (equal? true (first listOfBool))
                (not (member (list-ref listOfNode index ) visited))
                )
               (getIndexOfTrue (rest listOfBool) (add1 index) (cons index indexList) visited listOfNode)
               ]
              [else (getIndexOfTrue (rest listOfBool) (add1 index) indexList visited listOfNode)]
              )
            )

          (define (getAllTrues listOfIndex listOfNodes newList)
            (cond
              [(empty? listOfIndex) newList]
              [else
               (getAllTrues
                (rest listOfIndex)
                listOfNodes
                (cons (my-eval (list-ref listOfNodes (first listOfIndex))) newList)
                )
               ]
              )
            )

          (define (find node)
            (findPath node targetNode)
            )
          ]
    (lookAtNode node empty empty "")
    )
  )






(test)