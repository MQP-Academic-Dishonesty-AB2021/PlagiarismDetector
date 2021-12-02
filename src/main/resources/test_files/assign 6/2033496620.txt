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











(define (add-unique los node)
  (if (member (node-name node) los)
      los
      (begin (set! los (cons (node-name node) los))
             los)))



(define-syntax create
  (syntax-rules ()
    [(create name value)
     (begin (define name value)
            (set! name value))]))



(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph (quote name) (quote ())))]
    [(new vertex name in graph-name)
     (begin (create name (make-node (quote name) (quote ())))
            (set-graph-nodes! graph-name (add-unique (graph-nodes graph-name) name)))]))


(begin
  (new graph g0)
  (new vertex n1 in g0)
  (new vertex n2 in g0)
  (new vertex n9 in g0)
  (new vertex n0 in g0))


(check-expect (add-unique (list 'n0 'n1 'n2) n9) (list 'n9 'n0 'n1 'n2))
(check-expect (add-unique (list 'n0 'n1 'n2) n0) (list 'n0 'n1 'n2))


(define-syntax edge
  (syntax-rules ()
    [(edge n1 n2)
     (set-node-edges! n1 (add-unique (node-edges n1) n2))]))



(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges n1 n2) (edge n1 n2)]
    [(edges n1 -> n2) (edge n1 n2)]
    [(edges n1 <- n2) (edge n2 n1)]
    [(edges n1 <-> n2)
     (begin
       (edge n1 n2)
       (edge n2 n1))]
    [(edges n1 a1 n2 a2 ...)
     (begin
       (edges n1 a1 n2)
       (edges n2 a2 ...))]))


(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)






(check-expect (path n2 n0) true)

(check-expect (path n2 n9) false)

(define (path goto-node from-node)
  
  
  (local [(define (fn-for-node from-node todo visited)
            (if (member (node-name from-node) visited)
                (fn-for-los todo visited)
                (if (equal? goto-node from-node)
                    true
                    (fn-for-los (append (node-edges from-node) todo)
                            (cons (node-name from-node) visited)))))
          
          (define (fn-for-los todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (my-eval (first todo))
                                (rest todo)
                                visited)]))]
    (fn-for-node from-node empty empty)))


(check-expect (does n1 have an edge from n0 ?) true)

(check-expect (does n0 have an edge from n1 ?) false)

(check-expect (does n2 have a bidirectional edge from n1 ?) true)

(check-expect (does n2 have a path from n0 ?) true)

(check-expect (does n2 have a path from n9 ?) false)


(define-syntax does
  (syntax-rules (have an edge from a bidirectional ? path)
    [(does n1 have an edge from n2 ?)
     (if (member (quote n1) (node-edges n2))
         true
         false)]
    [(does n1 have a bidirectional edge from n2 ?)
     (if (and (member (quote n1) (node-edges n2)) (member (quote n2) (node-edges n1)))
         true
         false)]
    [(does n1 have a path from n2 ?)
     (path n1 n2)]))


(test)