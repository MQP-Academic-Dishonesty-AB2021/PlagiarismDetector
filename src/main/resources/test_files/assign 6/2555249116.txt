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
  (syntax-rules()
    [(create name val)
     (begin
       (define name val)
       (set! name val))]))






(define (add-unique node lon)
  (cond [(member node lon)
         lon]
        [else
         (cons node lon)]))



(define-syntax new
  (syntax-rules(graph vertex in)
    [(new graph name)
     (define name (make-graph (quote name) empty))]
    [(new vertex v-name in g-name)
     (begin
       (create v-name (make-node (quote v-name) empty))
       (set-graph-nodes! g-name (add-unique
                                 (node-name v-name)
                                 (graph-nodes g-name))))
     ]))



(define-syntax edge
  (syntax-rules ()
    [(edge n n1)
     (set-node-edges! n (add-unique
                         (node-name n1)
                         (node-edges n)))]))







(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges n -> n1)
     (edge n n1)]
    [(edges n1 <- n)
     (edge n n1)]
    [(edges n <-> n1)
     (begin
       (edge n n1)
       (edge n1 n))]
    [(edges n op1 n1 op2 ...)
     (begin
       (edges n op1 n1)
       (edges n1 op2 ...))]))




(define-syntax does
  (syntax-rules (have an edge from ? bidirectional path)
    [(does n have an edge from n1 ?)
     (not (false? (member (quote n) (node-edges n1))))]
    [(does n have a bidirectional edge from n1 ?)
     (and
      (does n have an edge from n1 ?)
      (does n1 have an edge from n ?))]
    [(does n have a path from n1 ?)
     (has-path? n n1)]))





(define (has-path? n n0)
  (local [(define (fn-for-node n0 todo visited)
            (cond [(equal? n0 n) true]
                  [(member n0 visited)
                   (fn-for-lon todo visited)]
                  [else (fn-for-lon (append todo (node-edges n0))
                                    (cons n0 visited))]))
          
          (define (fn-for-lon todo visited)
            (cond
              [(empty? todo) false]
              [else
               (fn-for-node (my-eval (first todo))
                            (rest todo)
                            visited)]))]
    (fn-for-node n0 empty empty)))




(define (path start-node find-node)
  (local [
          
          
          
          
          (define-struct node-and-parent(node parent))


          
          
          
          (define (get-parent node)
            (node-and-parent-parent node))


          
          
          
          
          (define (get-path node path-so-far)
            (if (string=?
                 ""
                 (if (string? (get-parent node))
                     ""
                     (symbol->string
                      (node-name (my-eval (node-and-parent-node (get-parent node)))))))
                path-so-far
                (get-path
                 (get-parent node)
                 (string-append
                  (symbol->string (node-name (my-eval
                                              (node-and-parent-node (get-parent node)))))
                  " -> "
                  path-so-far))))


          
          
          
          
          (define (get-edges start-node-and-parent)
            (map (λ(edge)
                   (make-node-and-parent edge start-node-and-parent))
                 (node-edges (my-eval (node-and-parent-node start-node-and-parent)))))


          
          
          
          
          
          (define (fn-for-node start-node-and-parent todo visited)
            (cond [(equal? (my-eval
                            (node-and-parent-node start-node-and-parent))
                           find-node)
                   (get-path start-node-and-parent
                             (symbol->string
                              (node-name (my-eval
                                          (node-and-parent-node
                                           start-node-and-parent)))))]
                  [(member (node-and-parent-node start-node-and-parent) visited)
                   (fn-for-lon todo visited)]
                  [else (fn-for-lon (append
                                     todo
                                     (get-edges start-node-and-parent))
                                    (cons
                                     (node-and-parent-node start-node-and-parent)
                                     visited))]))


          
          
          
          
          
          (define (fn-for-lon todo visited)
            (cond
              [(empty? todo) ""]
              [else
               (fn-for-node (my-eval (first todo))
                            (rest todo)
                            visited)]))]
    (fn-for-node (make-node-and-parent start-node "") empty empty)))





(new graph g0)

(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(new vertex n9 in g0)

(create n4 (make-node (quote n4) empty))

(edges n1 -> n3 <-> n9 <- n1)
(edges n2 -> n1)



(check-expect (add-unique (node-name n3) (graph-nodes g0)) (graph-nodes g0))
(check-expect (add-unique (node-name n9) (graph-nodes g0)) (graph-nodes g0))
(check-expect (add-unique (node-name n4)
                          (graph-nodes g0))
              (cons (node-name n4) (graph-nodes g0)))



(check-expect (does n1 have an edge from n9 ?) false)
(check-expect (does n3 have an edge from n1 ?) true)
(check-expect (does n2 have a bidirectional edge from n1 ?) false)
(check-expect (does n3 have a bidirectional edge from n9 ?) true)
(check-expect (does n2 have a path from n1 ?) false)
(check-expect (does n1 have a path from n2 ?) true)



(check-expect (has-path? n0 n1) false)
(check-expect (has-path? n1 n9) false)
(check-expect (has-path? n9 n1) true)
(check-expect (has-path? n3 n2) true)


(check-expect (path n1 n3) "n1 -> n3")
(check-expect (path n3 n3) "n3")
(check-expect (path n2 n3) "n2 -> n1 -> n3")
(check-expect (path n3 n1) "")
(test)