


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
       (set! x val))]))












(define-syntax new
  (syntax-rules (graph vertex in)
    
    [(new graph g)
     (create g (make-graph (quote g) empty))]
    
    [(new vertex n in g)
     (begin
       (create n (make-node (quote n) empty))
       (add-unique n g))]))











(define (add-unique nod g)
  (cond [(member (node-name nod) (graph-nodes g))
         "This node already exists in this graph"]
        [else
         (set-graph-nodes! g (cons (node-name nod) (graph-nodes g)))]))



(create node1 (make-node "node1" (list empty)))
(create test-graph (make-graph "test" (list empty)))


(check-expect (begin
                (add-unique node1 test-graph)
                test-graph)
              (make-graph "test" (list (node-name node1) empty)))



(check-expect (begin
                (add-unique node1 test-graph)
                test-graph)  
              (make-graph "test" (list (node-name node1) empty)))








(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)
(new vertex n11 in g0)




(define-syntax edge
  (syntax-rules ()
    [(edges first-n second-n)
     (set-node-edges! first-n (cons (quote second-n)
                                    (node-edges first-n)))]))






(define (duplicate? nod edgs)
  (member nod edgs))





(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges first-n -> second-n)
     (if (not (duplicate? (quote second-n) (node-edges first-n)))
         (set-node-edges! first-n (cons (quote second-n)
                                        (node-edges first-n)))
         "This edge already exists")]
    
    [(edges first-n <- second-n)
     (if (not (duplicate? (quote first-n) (node-edges second-n)))
         (set-node-edges! second-n (cons (quote first-n)
                                         (node-edges second-n)))
         "This edge already exists")]
    
    [(edges first-n <-> second-n) 
     (begin
       (edges first-n -> second-n)
       (edges first-n <- second-n))]

    [(edges first-n edg1 second-n edg2 ...)
     (begin                           
       (edges first-n edg1 second-n)
       (edges second-n edg2 ...))]
    )) 








(check-expect (begin   
                (edges n0 -> n1)
                (does n1 have an edge from n0 ?))
              true)

(check-expect (begin   
                (edges n0 -> n1)
                (does n0 have an edge from n1 ?))
              false)

(check-expect (begin   
                (edges n2 <-> n1)
                (does n2 have a bidirectional edge from n1 ?))
              true)

(check-expect (begin   
                (edges n1 -> n9)
                (does n1 have a bidirectional edge from n9 ?))
              false)


(define-syntax does
  (syntax-rules (have
                 a an
                 bidirectional
                 edge
                 path
                 from
                 ?)
    [(does first-n have an edge from second-n ?)
     (if (member (quote first-n) (node-edges second-n))
         true
         false)]
    
    [(does first-n have a bidirectional edge from second-n ?)
     (if (and (member (quote first-n) (node-edges second-n))
              (member (quote second-n) (node-edges first-n)))
         true
         false)]
    
    [(does first-n have a path from second-n ?)
     (path? second-n first-n )] 
    ))










(check-expect (path? n0 n1) true)  
(check-expect (does n1 have a path from n0 ?) true)


(check-expect (path? n1 n2) true)  
(check-expect (does n2 have a path from n1 ?) true)


(check-expect (path? n1 n0) false) 
(check-expect (does n0 have a path from n1 ?) false)


(check-expect (path? n0 n9) true)  
(check-expect (does n9 have a path from n0 ?) true)



(define (path? n-1 n-2)
  
  
  (local [(define (fn-for-node n todo visited)
            (cond
              [(equal? (my-eval n) (my-eval n-2)) true]
              [(member (my-eval n) visited)
               (fn-for-lon todo visited)]
              [else
               (fn-for-lon (append (node-edges (my-eval n)) todo)
                           (cons n visited))]
              ))
          
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (first todo)
                                (rest todo)
                                visited)]))]
    
    (fn-for-node n-1 empty empty)
    ))
     
(test)






(new graph j)
(new vertex n.1 in j)
(new vertex n.2 in j)
(new vertex n.3 in j)
(new vertex n.4 in j)
(new vertex n.5 in j)
(new vertex n.6 in j)
(edges n.1 -> n.2 -> n.3 -> n.5 -> n.6)
(edges n.2 -> n.5)


(define (path n-1 n-2)
  
  
  
  (local [(define (fn-for-node n todo visited rsf)
            (cond
              [(equal? (my-eval n) (my-eval n-2))
               (string-append rsf " -> " (symbol->string (node-name (my-eval n-2))))]
              [(member (my-eval n) visited)
               (fn-for-lon todo visited)]
              [else
               (fn-for-lon (append (node-edges (my-eval n)) todo)
                           (cons n visited)
                           (if (equal? n n-1)
                               (string-append
                                (symbol->string (node-name (my-eval n)))
                                )
                               (string-append
                                rsf
                                " -> "
                                (symbol->string (node-name (my-eval n)))
                                )))]
              ))
          
          (define (fn-for-lon todo visited rsf)
            (cond [(empty? todo) ""]
                  [else
                    (fn-for-node (first todo) (rest todo)
                                 visited rsf)
                    ]))]

    (fn-for-node n-1 empty empty "")
    ))

