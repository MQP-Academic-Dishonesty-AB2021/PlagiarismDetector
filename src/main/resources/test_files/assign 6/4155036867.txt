


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
    [(create var val)
     (begin (define var 0)
            (set! var val))]))




(define (add-unique sym los)
  (if (member sym los)
      los
      (cons sym los)))

(define exmp0 1)
(define exmp1 1)
(define exmp2 1)
(define exmp3 1)
(define exmp4 1)
(define loexmp0 (list (quote exmp1) (quote exmp2) (quote exmp3)))
(define loexmp1 empty)


(check-expect (add-unique (quote exmp0) loexmp0)
              (cons (quote exmp0)  loexmp0))

(check-expect (add-unique (quote exmp1) loexmp0)
              loexmp0)

(check-expect (add-unique (quote exmp3) loexmp0)
              loexmp0)

(check-expect (add-unique (quote exmp4) loexmp1)
              (cons (quote exmp4) loexmp1))

(check-expect (add-unique (quote exmp2) loexmp1)
              (cons (quote exmp2) loexmp1))


(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph symbol)
     (create symbol (make-graph (quote symbol) empty))]
    [(new vertex symbol in add-to)
     (begin (create symbol (make-node (quote symbol) empty))
            (set-graph-nodes! add-to (add-unique (quote symbol) (graph-nodes add-to))))]
    ))


(define-syntax edge
  (syntax-rules ()
    [(edge node1 node2)
     (set-node-edges! node1 (add-unique (quote node2) (node-edges node1)))]))


(define-syntax edges
  (syntax-rules (-> <-> <-)
    [(edges node1 -> node2)
     (edge node1 node2)]
    [(edges node1 <- node2)
     (edge node2 node1)]
    [(edges node1 <-> node2)
     (begin (edges node1 -> node2)
            (edges node2 -> node1))]
    [(edges node1 op node2 other ...)
     (begin (edges node1 op node2) (edges node2 other ...))]
    [(edges node) node]
    ))

(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(new vertex n4 in g0)
(new vertex n5 in g0)
(edges n0 <-> n3 -> n2 <- n1 <- n2 -> n5 -> n1)


(define-syntax does
  (syntax-rules (have a an edge bidirectional path from ?)
    [(does node1 have an edge from node2 ?)
     (if (member (quote node1) (node-edges node2)) true false)]
    [(does node1 have a bidirectional edge from node2 ?)
     (and (does node1 have an edge from node2 ?) (does node2 have an edge from node1 ?))]
    [(does node1 have a path from node2 ?)
     (path? node2 node1)]))

(check-expect (does n1 have an edge from n0 ?) false)
(check-expect (does n2 have an edge from n3 ?) true)
(check-expect (does n3 have an edge from n1 ?) false)
(check-expect (does n3 have a bidirectional edge from n2 ?) false)
(check-expect (does n2 have a bidirectional edge from n1 ?) true)








(check-expect (does n2 have a path from n0 ?) true)
(check-expect (does n0 have a path from n2 ?) false)



(define (path? start end)
  (local [(define (path-inner--n node visited todo)
          (cond [(member node visited) (path-inner--lon visited todo)]
                
                [(equal? node (node-name end)) true]
                [(path-inner--lon (cons node visited)
                                  (append (node-edges (my-eval node)) todo))]))
          (define (path-inner--lon visited todo)
            
            (cond [(empty? todo) 2]
                  [else (path-inner--n (first todo) visited (rest todo))]))]
    
    
    (if (equal? (path-inner--n (node-name start) empty empty) 2) false true)))
          

(check-expect (path? n3 n1) true)
(check-expect (path? n1 n3) false)
(check-expect (path? n4 n1) false)
(check-expect (path? n1 n4) false)
(check-expect (path? n0 n3) true)
(check-expect (path? n3 n2) true)
(check-expect (path? n1 n5) true)
(check-expect (path? n5 n0) false)



(test)