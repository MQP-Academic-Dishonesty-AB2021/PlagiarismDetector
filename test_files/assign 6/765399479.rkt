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


(define-syntax create
  (syntax-rules ()
    [(create name value)
     (begin (define name value)
            (set! name value))]))







(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph g0)
     (define g0 (make-graph (quote g0) empty))]
    [(new vertex n0 in g0)
     (begin (define n0 (make-node (quote n0) empty))
            (set-graph-nodes! g0 (add-unique (quote n0) (graph-nodes g0))))]))




(define (add-unique sym los)
  (if (member sym los)
      los
      (cons sym los)))

(check-expect (add-unique 'n0 (list 'n1 'n2)) (list 'n0 'n1 'n2))
(check-expect (add-unique 'n1 (list 'n1 'n2)) (list 'n1 'n2))
(test)






(define-syntax edge
  (syntax-rules ()
    [(edge n0 n1)
     (set-node-edges! n0 (add-unique (quote n1) (node-edges n0)))]))


(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges n0) void]
    [(edges n1 -> n2 rest ...)
     (begin (edge n1 n2)
            (edges n2 rest ...))]
    [(edges n1 <- n2 rest ...)
     (begin (edge n2 n1)
            (edges n2 rest ...))]
    [(edges n1 <-> n2 rest ...)
     (begin (edge n1 n2)
            (edge n2 n1)
            (edges n2 rest ...))]))


(new graph moves)
(new vertex left in moves)
(new vertex right in moves)
(new vertex up in moves)
(new vertex down in moves)
(edges left <-> right -> up <- down)

(new graph colors)
(new vertex red in colors)
(new vertex green in colors)
(new vertex blue in colors)
(new vertex orange in colors)
(new vertex teal in colors)
(new vertex teal2 in colors)
(edges orange <- red <-> green <-> blue <-> red)
(edges green -> teal <- blue)
(edges teal <-> teal2)





(define-syntax does
  (syntax-rules (have an edge from a bidirectional ?)
    [(does n1 have an edge from n2 ?)
     (not (false? (member (quote n1) (node-edges n2))))]
    [(does n1 have a bidirectional edge from n2 ?)
     (and (does n1 have an edge from n2 ?)
          (does n2 have an edge from n1 ?))]
    [(does n1 have a path from n2 ?)
     (path? (quote n2) (quote n1))]))

(check-expect (does left have an edge from right ?) true)
(check-expect (does up have an edge from down ?) true)
(check-expect (does down have an edge from up ?) false)
(check-expect (does right have a bidirectional edge from left ?) true)
(check-expect (does left have a bidirectional edge from right ?) true)
(check-expect (does up have a bidirectional edge from down ?) false)

(check-expect (does up have a path from down ?) true)
(check-expect (does left have a path from down ?) false)





(define (path? from to)
  (local [(define (fn-for-node n todo visited)
            (cond [(member to (node-edges (my-eval n))) true]
                  [(member n visited)
                   (fn-for-lon todo visited)]
                  [else (fn-for-lon (append (node-edges (my-eval n)) todo)
                            (cons n visited))]))
          
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (first todo)
                                (rest todo)
                                visited)]))]
    (fn-for-node from empty empty)))



(check-expect (path? 'down 'up) true)
(check-expect (path? 'left 'down) false)
(check-expect (path? 'left 'up) true)
(check-expect (path? 'green 'orange) true)
(check-expect (path? 'teal2 'red) false)
(test)