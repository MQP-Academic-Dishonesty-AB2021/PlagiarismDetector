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
    [(create name val)
     (begin
       (define name val)
       (set! name val))]))

(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph (quote name) empty))]
    [(new vertex name in val)
     (begin (create name (make-node (quote name) empty))
            (add-unique name val))]))









(check-expect (add-unique (make-node (quote n24) empty)
                          (make-graph (quote g24) empty))
              (make-graph (quote g24) (list (quote n24))))

(check-expect (add-unique n0 g0) g0)


(define (add-unique node val)
  (begin (if (graph? val)
             (if (add-unique-checker node val)
                 empty
                 (set-graph-nodes! val (cons (node-name node)
                                             (graph-nodes val))))
             (if (add-unique-checker node val)
                 empty
                 (set-node-edges! val (cons (node-name node)
                                            (node-edges val)))))
         val))










(check-expect (add-unique-checker (make-node (quote n25) empty)
                                  (make-graph (quote g25) empty))
              false)
(check-expect (add-unique-checker n0 g0) true)


(define (add-unique-checker node val)
  (if (graph? val)
      (list? (member (node-name node) (graph-nodes val))) 
      (list? (member (node-name node) (node-edges val))))) 


(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)

(test)
(displayln " <- This is Part 1")



(new vertex n2 in g0)
(new vertex n9 in g0)

(define-syntax edge
  (syntax-rules ()
    [(edge node1 node2)
     (add-unique node2 node1)]))

(define-syntax edges
  (syntax-rules (-> <-> <-)
    [(edges node1 -> node2)
     (edge node1 node2)]
    [(edges node1 <- node2)
     (edge node2 node1)]
    [(edges node1 <-> node2)
     (begin (edge node1 node2)
            (edge node2 node1))]
    [(edges node1 op node2 opr ...)
     (begin (edges node1 op node2)
            (edges node2 opr ...))]))

(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)




(define-syntax does
  (syntax-rules (have an edge from ? a bidirectional path)
    [(does node1 have an edge from node2 ?)
     (add-unique-checker node1 node2)]
    [(does node1 have a bidirectional edge from node2 ?)
     (and (add-unique-checker node1 node2)
          (add-unique-checker node2 node1))]
    [(does node1 have a path from node2 ?)
     (path? node2 node1)]))


(check-expect (does n1 have an edge from n0 ?) true)

(check-expect (does n0 have an edge from n1 ?) false)

(check-expect (does n2 have a bidirectional edge from n1 ?) true)

(check-expect (does n9 have a bidirectional edge from n1 ?) false)

(test)
(displayln " <- This is Part 3")




(define (path? node1 node2)
  (local [(define (fn-for-node todo visited currentnode)
            (if (list? (member currentnode visited))
                (fn-for-lon todo visited)
                (if (add-unique-checker node2 currentnode)
                    true
                    (fn-for-lon
                     (append (node-edges currentnode) todo)
                     (cons currentnode visited)))))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else (fn-for-node (rest todo)
                                     visited
                                     (my-eval (first todo)))]))]
    (fn-for-node empty empty node1)))


(check-expect (does n2 have a path from n0 ?) true)

(check-expect (does n2 have a path from n9 ?) false)

(test)
(println " <- This is Part 4")




































