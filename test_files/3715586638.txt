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
    [(create name val)
     (begin 
       (define name val)
       (set! name val))]))






[check-expect [add-unique [quote n0] empty] [list [quote n0]]]

[check-expect [add-unique [quote n0] [list [quote n1]]] [list 'n0 'n1]]

[check-expect [add-unique [quote n0] [list [quote n1] [quote n0]]]
              [list [quote n1] [quote n0]]]


[define [add-unique node lon]
  [cond [[member node lon] lon]
        [else
         [cons node lon]]]]


[define-syntax new
  [syntax-rules [graph vertex in]
    [[new graph name]
     [create name [make-graph [quote name] empty]]]
    [[new vertex n in g]
     [begin
       [create n [make-node [quote n] empty]]
       [set-graph-nodes! g [add-unique [quote n] [graph-nodes g]]]]]]]


[define-syntax edge
  [syntax-rules []
    [[edge n1 n2]
     [set-node-edges! n1 [add-unique [quote n2] [node-edges n1]]]]]]


[define-syntax edges
  [syntax-rules [-> <-> <-]
    [[edges n1 -> n2]
     [edge n1 n2]]
    [[edges n1 <- n2]
     [edge n2 n1]]
    [[edges n1 <-> n2]
     [begin
       [edge n1 n2]
       [edge n2 n1]]]
    [[edges n1 point n2 point2 ...]
     [begin
       [edges n1 point n2]
       [edges n2 point2 ...]]]]]



[check-expect [does n1 have an edge from n0 ?]
              true]
[check-expect [does n0 have an edge from n1 ?]
              false]
[check-expect [does n2 have a bidirectional edge from n1 ?]
              true]
[check-expect [does n0 have a bidirectional edge from n1 ?]
              false]
[check-expect [does n2 have a path from n0 ?]
              true]
[check-expect [does n0 have a path from n9 ?]
              false]
[check-expect [does n9 have a path from n0 ?]
              true]
[check-expect [does n9 have a path from n1 ?]
              true]

[define-syntax does
  [syntax-rules [have an edge from ? a bidirectional path]
    [[does n1 have an edge from n2 ?]
     [if [member [quote n1] [node-edges n2]]
         true
         false]]
    [[does n1 have a bidirectional edge from n2 ?]
     [and [does n1 have an edge from n2 ?]
          [does n2 have an edge from n1 ?]]]
    [[does n1 have a path from n2 ?]
     [check-path n2 n1]]]]






[check-expect [check-path n9 n0] false]

[check-expect [check-path n0 n9] true]

[check-expect [check-path n1 n9] true]


[define [check-path n1 n2]
  
  
  (local [(define (fn-for-node n todo visited) 
            (cond [(member n visited)
                   (fn-for-lon todo visited)]
                  [[equal? n n2] true]
                  {else
                   (fn-for-lon (append (node-edges n) todo)
                               (cons n visited))})) 
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-node [my-eval (first todo)] 
                                (rest todo)
                                visited)]))]
    (fn-for-node n1 empty empty))]


[check-expect [path n9 n0] ""]
[check-expect [path n1 n2] "n1 -> n2"]
[check-expect [path n2 n1] "n2 -> n1"]
[check-expect [path n0 n9] "n0 -> n9"]

[define [path n1 n2]
  
  
  (local [(define (fn-for-node n todo visited current-path) 
            (cond [(member n visited)
                   (fn-for-lon todo visited current-path)]
                  [[equal? n n2] [string-append current-path [symbol->string
                                                              [node-name n]]]]
                  [[member [node-name n2] [node-edges n]]
                   [string-append current-path
                                  [symbol->string [node-name n]]
                                  " -> " [symbol->string [node-name n2]]]]
                  [else
                   (fn-for-lon (append todo (node-edges n))
                               (cons n visited)
                               [string-append current-path
                                              [symbol->string
                                               [node-name n]] " -> "])]))
          (define (fn-for-lon todo visited current-path)
            (cond [(empty? todo) ""]
                  [else
                   (fn-for-node [my-eval (first todo)] 
                                (rest todo)
                                visited
                                current-path)]))]
    (fn-for-node n1 empty empty ""))]

    
[new graph g0]
[new vertex n0 in g0]
[new vertex n1 in g0]
(new vertex n2 in g0)
(new vertex n9 in g0)
(edge n0 n1)
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)


(test)