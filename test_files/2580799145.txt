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
    [(create a b)
     (begin
       (define a b)
       (set! a b))]))



(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph g)
     (create g (make-graph (quote g) empty))]
    [(new vertex n in g)
     (begin
       (create n (make-node (quote n) empty))
       (set-graph-nodes! g (add-unique (quote n) (graph-nodes g))))]))




(define (add-unique node list)
  (if (member node list)
      list
      (cons node list)))



(check-expect (add-unique 7 (list 1 2 5 8)) (list 7 1 2 5 8))
(check-expect (add-unique 7 empty) (list 7))
(check-expect (add-unique "c" (list "C" "A" "B")) (list "c" "C" "A" "B"))
(check-expect (add-unique n0 (list n1  n2)) (list n0 n1 n2))

(check-expect (add-unique 8 (list 1 2 5 8)) (list 1 2 5 8))
(check-expect (add-unique "C" (list "C" "A" "B")) (list "C" "A" "B"))
(check-expect (add-unique n0 (list n1 n2 n0)) (list n1 n2 n0))



(define-syntax edge
  (syntax-rules ()
    [(edge node enode)
     (begin
       (set-node-edges! node (add-unique (quote enode) (node-edges node))))]))


(define-syntax edges
  (syntax-rules(-> <-> <-)
    [(edges n0 <- n1)
     (edge n1 n0)]
    [(edges n0 -> n1)
     (edge n0 n1)]
    [(edges n0 <-> n1)
     (begin (edge n0 n1)
            (edge n1 n0))]
    [(edges n0 o1 n2 o3 n4 ...)
     (begin (edges n0 o1 n2)
            (edges n2 o3 n4 ...))]))



(define-syntax does
  (syntax-rules(have an edge from ? a bidirectional path)
    [(does n0 have an edge from n1 ?)
     (not (false? (member (quote n0) (node-edges n1))))]
    [(does n0 have a bidirectional edge from n1 ?)
     (and (does n1 have an edge from n0 ?)
          (does n0 have an edge from n1 ?))]
    [(does n0 have a path from n1 ?)
     (not (string=? "" (path n0 n1)))]))





(define-syntax path
  (syntax-rules ()
    [(path n0 n1)
     (local [(define (find--node node nf visited todo path)
               
               
               (if (member node visited)
                   (find--lon todo nf visited path)
                   (cond [(eq? node nf) (path->string path)]
                         [else (find--lon (append todo (node-edges node))
                                          nf
                                          (cons node visited)
                                          (if (empty? todo)
                                              path
                                              (remove-last path)))])))
             
             
             (define (find--lon lon nf visited path)
               (cond [(empty? lon) ""]
                     [else
                      (or (find--node (my-eval (first lon))
                                      nf
                                      visited
                                      (rest lon)
                                      (append  path  (list (first lon))))
                          (find--lon (rest lon) nf visited path))]))
             
             
             (define (path->string path)
               (cond [(empty? path) ""]
                     [else (string-append (symbol->string (node-name (my-eval (first path))))
                                          (arrow-helper path)
                                          (path->string (rest path)))]))
             
             (define (arrow-helper lon)
               (if (empty? (rest lon))
                   ""
                   " -> "))

             
             (define (remove-last lox)
               (reverse (rest (reverse lox))))]
       
       (find--node n1 n0 empty empty (list n1)))]))


(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)
(edge n0 n1)
(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)

(check-expect (does n0 have an edge from n1 ?) false) 
(check-expect (does n1 have an edge from n0 ?) true) 
(check-expect (does n9 have an edge from n1 ?) false) 
(check-expect (does n0 have an edge from n0 ?) false) 


(check-expect (does n0 have a bidirectional edge from n1 ?) false) 
(check-expect (does n1 have a bidirectional edge from n0 ?) false) 


(check-expect (does n1 have a bidirectional edge from n2 ?) true) 
(check-expect (does n2 have a bidirectional edge from n1 ?) true) 


(check-expect (does n0 have a bidirectional edge from n0 ?) false) 

(check-expect (does n1 have a path from n0 ?) true) 
(check-expect (does n2 have a path from n0 ?) true) 
(check-expect (does n9 have a path from n0 ?) true) 
(check-expect (does n0 have a path from n9 ?) false) 
(check-expect (does n0 have a path from n0 ?) true)  
(check-expect (does n9 have a path from n9 ?) true) 

(check-expect (path n1 n0) "n0 -> n1") 
(check-expect (path n2 n0) "n0 -> n1 -> n2") 
(check-expect (path n9 n0) "n0 -> n9") 
(check-expect (path n0 n9) "") 
(check-expect (path n0 n0) "n0")  
(check-expect (path n9 n9) "n9") 


(new graph g1)
(new vertex v0 in g1)
(new vertex v1 in g1)
(new vertex v2 in g1)
(new vertex v3 in g1)
(new vertex v4 in g1)
(new vertex v7 in g1)

(edges v0 -> v1 -> v2 -> v3 -> v0 <-> v4 -> v3)
(check-expect (path v0 v4) "v4 -> v0") 
(check-expect (path v7 v4) "") 
(check-expect (path v4 v1) "v1 -> v2 -> v3 -> v0 -> v4") 


(new graph g2)
(new vertex a0 in g2)
(new vertex a1 in g2)
(new vertex a2 in g2)
(new vertex a3 in g2)
(new vertex a4 in g2)

(edges a4 -> a0 -> a1 -> a2 -> a3 -> a1)
(check-expect (path a4 a0) "") 
(check-expect (does a4 have a path from a0 ?) false) 
(check-expect (does a1 have a path from a3 ?) true) 


(test)
