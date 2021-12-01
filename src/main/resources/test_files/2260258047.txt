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













(define (add-unique a b) 
  (cond [(graph? b)
         (if (not (member (node-name a) (graph-nodes b)))
             (set-graph-nodes! b (cons (node-name a) (graph-nodes b)))
             (void))
         ]
        [(node? b)
         (if (not (member (node-name b) (node-edges a)))
             (set-node-edges! a (cons (node-name b) (node-edges a)))
             (void))]))
    


(define-syntax create
  (syntax-rules (graph node)
    [(create graph g)
     (begin
       (define g (make-graph (quote g) empty))
       (set! g g))]
    [(create node n)
     (begin
       (define n (make-node (quote n) empty))
       (set! n n))]))



(define-syntax new
  (syntax-rules (graph vertex)
    [(new graph g)
     (create graph g)]
    [(new vertex n in g)
     (begin
       (create node n)
       (add-unique n g))]))


(define-syntax edge
  (syntax-rules ()
    [(edge n0 n1)
     (add-unique n0 n1)] 
    ))


(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges n1 -> n2) (edge n1 n2)]
    [(edges n1 <- n2) (edge n2 n1)]
    [(edges n1 <-> n2) (begin (edge n1 n2) (edge n2 n1))]
    [(edges n1 op n2 op2 ...)
     (begin (edges n1 op n2) (edges n2 op2 ...))]
    ))


(define-syntax does
  (syntax-rules (bidirectional edge path)
    [(does n1 have an edge from n0 ?)
     (not (false? (member (quote n1) (node-edges n0))))]
    [(does n1 have a bidirectional edge from n0 ?)
     (and
      (does n1 have an edge from n0 ?)
      (does n0 have an edge from n1 ?))]
    [(does n1 have a path from n0 ?)
     (path? n0 n1)]))


(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)

(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n1 have a bidirectional edge from n0 ?) false)
(check-expect (does n2 have a bidirectional edge from n1 ?) true)
(check-expect (does n2 have a path from n0 ?) true)
(check-expect (does n0 have a path from n2 ?) true)
(check-expect (does n9 have a path from n2 ?) true)
(check-expect (does n2 have a path from n9 ?) false)







(check-expect (path? n2 n0) true)
(check-expect (path? n0 n2) true)
(check-expect (path? n9 n2) false)
(check-expect (path? n2 n9) true)

  

(define (path? n0 n1)
  
  
  
  (local [(define (fn-for-node n todo visited)
            (cond
              [(equal? n n1) true]
              [(not (false? (member (node-name n) visited)))
               (fn-for-lon todo visited)]
              [else (fn-for-lon (append todo (node-edges n))
                                (cons (node-edges n) visited))]))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node (my-eval (first todo))
                                (rest todo) visited)]))]
    (fn-for-node n0 empty empty)))
(test)