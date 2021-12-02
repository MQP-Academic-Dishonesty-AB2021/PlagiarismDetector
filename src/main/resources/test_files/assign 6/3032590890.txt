


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
     (begin (define name val)
            (set! name val))]))







(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph name)
     (create name (make-graph (quote name) (list)))]
    [(new vertex name in agraph)
     (begin (create name (make-node (quote name) (list)))
            (set-graph-nodes! agraph (add-unique (graph-nodes agraph) (quote name))))]))




(check-expect (add-unique empty 12) (list 12)) 
(check-expect (add-unique (list 15) 15) (list 15)) 
(check-expect (add-unique (list 24) 13) (list 13 24)) 
(check-expect (add-unique (list 24 20) 20) (list 24 20)) 

(define (add-unique list value)
  (if (member value list)
      list
      (cons value list)))


(define-syntax edge
  (syntax-rules ()
    [(edge n1 n2)
     (set-node-edges! n1 (add-unique (node-edges n1) (quote n2)))]))


(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges e1 -> e2)
     (edge e1 e2)]
    [(edges e1 <- e2)
     (edge e2 e1)]
    [(edges e1 <-> e2)
     (begin (edge e1 e2)
            (edge e2 e1))]
    [(edges e1 op1 e2 rest ...)
     (begin (edges e1 op1 e2)
            (edges e2 rest ...))]))


(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(new vertex n4 in g0)
(new vertex n5 in g0)
(new vertex n9 in g0)
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2 <- n4 <-> n4 -> n3 -> n5 -> n9)



(check-expect (does n0 have an edge from n1 ?) #f) 
(check-expect (does n1 have an edge from n0 ?) #t) 
(check-expect (does n1 have a bidirectional edge from n2 ?) #t) 

(check-expect (does n2 have a bidirectional edge from n1 ?) #t)

(check-expect (does n9 have an edge from n2 ?) #t)

(check-expect (does n9 have an edge from n0 ?) #t)


(check-expect (does n0 have an edge from n9 ?) #f)


(check-expect (does n0 have a path from n2 ?) #t)
(check-expect (does n9 have a path from n4 ?) #t)
(check-expect (does n4 have a path from n5 ?) #f)
(check-expect (does n4 have a path from n3 ?) #f)

(define-syntax does
  (syntax-rules (have an edge a bidirectional path from ?)
    [(does n0 have an edge from n1 ?)
     (not (false? (member (quote n0) (node-edges n1))))]
    [(does n0 have a bidirectional edge from n1 ?)
     (not (false? (and (member (quote n0) (node-edges n1))
                       (member (quote n1) (node-edges n0)))))]
    [(does n0 have a path from n1 ?)
     (path? (quote n0) (quote n1))]))
     





(check-expect (path? 'n1 'n0) #t) 
(check-expect (path? 'n1 'n9) #f) 
(check-expect (path? 'n4 'n2) #f) 
(check-expect (path? 'n9 'n3) #t) 
(check-expect (path? 'n9 'n4) #t) 
(check-expect (path? 'n4 'n5) #f) 
(check-expect (path? 'n4 'n3) #f) 

(define (path? end start)
  
  
  (local [(define (fn-for-node current todo visited)
            (if (member current visited)
                (fn-for-lon todo visited)
                (if (equal? (node-name current) end)
                    #t
                    (fn-for-lon (append (node-edges current) todo)
                                (cons current visited)))))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) #f]
                  [else (fn-for-node (my-eval (first todo)) (rest todo) visited)]))]
    (fn-for-node (my-eval start) empty empty)))





(check-expect (path n1 n9) "") 
(check-expect (path n1 n0) "n0 -> n1") 
(check-expect (path n4 n5) "") 
(check-expect (path n4 n3) "") 
(check-expect (path n9 n4) "n4 -> n2 -> n9") 
(check-expect (path n5 n4) "n4 -> n3 -> n5") 

(define (path end start)
  
  (local [(define-struct todo-item (node parent))
          (define (build-pairs todo-item)
            (build-list (length (node-edges (todo-item-node todo-item)))
                        (λ (n) (make-todo-item
                                (my-eval (list-ref (node-edges (todo-item-node todo-item))
                                                   n))
                                todo-item))))
          
          
          (define (fn-for-node current todo visited)
            (if (member (todo-item-node current) visited)
                (fn-for-lon todo visited)
                (if (equal? (node-name (todo-item-node current)) (node-name end))
                    current
                    (fn-for-lon (append todo (build-pairs current))
                                (cons (todo-item-node current) visited)))))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) #f]
                  [else (fn-for-node (my-eval (first todo)) (rest todo) visited)]))
          
          
          (define (build-result todo-item string-so-far)
            (if (string? (todo-item-parent todo-item))
                (string-append (symbol->string (node-name (todo-item-node todo-item)))
                               " -> "
                               string-so-far)
                (build-result (todo-item-parent todo-item) (string-append (symbol->string
                                                                           (node-name
                                                                            (todo-item-node
                                                                            todo-item)))
                                                                          " -> "
                                                                          string-so-far))))
          (define result (fn-for-node (make-todo-item start "") empty empty))]
    (if (false? result)
        ""
        (build-result (todo-item-parent result) (symbol->string (node-name (todo-item-node
                                                                            result)))))))
        


(define x 3)
(define y 5)
(set! y 5)









(define z (list y))

(set! y 8)










(define z2 (list (quote y)))
(set! y 11)








(test)