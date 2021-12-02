


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








(define (add-unique e loe)
  (begin
    (define temp-nodes (filter (lambda (x) (not (equal? e x))) loe))
    (if (not (empty? temp-nodes))
        (append temp-nodes (list e))
        (list e)
        )
    )
  )

(check-expect (add-unique (make-node 'n0 empty)
                          (graph-nodes (make-graph 'g0 empty))) (list (node 'n0 '())))

(check-expect (add-unique (make-node 'n0 empty)
                          (graph-nodes (make-graph 'g0 (list (node 'n0 '()))))) (list (node 'n0 '())))

(check-expect (add-unique (make-node 'n0 empty)
                          (graph-nodes (make-graph 'g0 (list (node 'n1 '()))))) (list (node 'n1 '()) (node 'n0 '())))




(define-syntax create
  (syntax-rules ()
    [(create x exp)
     (begin
       (define x exp)
       (set! x exp)
       )
     ]
    )
  )


(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph y)
     (create y (make-graph 'y empty))]
    [(new vertex y)
     (create y (make-node 'y empty))]
    [(new vertex v in g)
     (begin
       (new vertex v) 
       (set-graph-nodes! g (add-unique (quote v) (graph-nodes g))))] 
    )
  )

(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)






(define-syntax edge
  (syntax-rules ()
    [(edge x y)
     (set-node-edges! x (add-unique (quote y) (node-edges x)))]
    )
  )


(define-syntax edges
  (syntax-rules (-> <-> <-)
    [(edges x -> y)
     (edge x y)]
    [(edges x <-> y)
     (begin
       (edge x y)
       (edge y x)
       )]
    [(edges x <- y)
     (edge y x)]
    [(edges x op y op2 ...)
     (begin
       (edges x op y)
       (edges y op2 ...))]
    )
  )

(new vertex n9 in g0)

(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)





(define (contains e loe)
  (if (empty? loe)
      false
      (if (equal? e (my-eval (first loe)))
          true
          (contains e (rest loe))
          )
      )
  )

(check-expect (contains n1 (node-edges n0)) true) 
(check-expect (contains n0 (node-edges n1)) false) 



(define (contains-path n1 n2)
  (local [(define (map-path-node n todo visited)
            (if (equal? n n1)
                true
                (if (member n visited)
                    (map-path-lon todo visited)
                    (map-path-lon (append (node-edges n) todo) (cons n visited)))))
          (define (map-path-lon todo visited)
            (if (empty? todo)
                false
                (map-path-node (my-eval (first todo)) (rest todo) visited)
                )
            )
          ]
    (map-path-node n2 empty empty)
    )
  )

(check-expect (contains-path n2 n0) true) 
(check-expect (contains-path n2 n1) true) 
(check-expect (contains-path n0 n2) true) 
(check-expect (contains-path n0 n9) false) 

  

(define-syntax does
  (syntax-rules (have an a edge bidirectional from ?)
    [(does x have an edge from y ?)
     (if (contains x (node-edges y))
         true
         false
         )]
    [(does x have a bidirectional edge from y ?)
     (if (and (contains x (node-edges y)) (contains y (node-edges x)))
         true
         false
         )]
    [(does x have a path from y ?)
     (if (contains-path x y)
         true
         false
         )]
    )
  )

(does n1 have an edge from n0 ?)
(does n0 have an edge from n1 ?)
(does n2 have a bidirectional edge from n1 ?)

(does n2 have an edge from n0 ?)
(does n2 have a path from n0 ?)

(test)