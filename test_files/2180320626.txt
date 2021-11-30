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
    [(create vn val)
     (begin
       (define vn val)
       (set! vn val))]))












(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph gn)
     (create gn
             (make-graph (quote gn)
                         (mutable-set)))]
    [(new vertex vn in gn)
     (begin
       (create vn
               (make-node (quote vn)
                          (mutable-set)))
       (set-add! (graph-nodes gn)
                 (quote vn)))]))


(new graph g0)
(new vertex v0 in g0)
(new vertex v1 in g0)
(new vertex v2 in g0)
(new vertex v3 in g0)
(new vertex v4 in g0)

(check-expect (set-member? (graph-nodes g0) 'v0) #t)
(check-expect (set-member? (graph-nodes g0) 'v1) #t)
(check-expect (set-member? (graph-nodes g0) 'v2) #t)
(check-expect (set-member? (graph-nodes g0) 'v3) #t)
(check-expect (set-member? (graph-nodes g0) 'v4) #t)




(define-syntax edge
  (syntax-rules ()
    [(edge v1 v2) (set-add! (node-edges v1)
                            (quote v2))]))

(edge v0 v1)
(check-expect (set-member? (node-edges v0) 'v1) #t)
(check-expect (set-member? (node-edges v0) 'v2) #f)





(define-syntax edges
  (syntax-rules (-> <-> <-)
    [(_ n0 -> n1 others ...)
     (begin
       (edge n0 n1)
       (edges n1 others ...))]
    [(_ n0 <- n1 others ...)
     (begin
       (edge n1 n0)
       (edges n1 others ...))]
    [(_ n0 <-> n1 others ...)
     (begin
       (edge n0 n1)
       (edge n1 n0)
       (edges n1 others ...))]
    
    
    
    [(_ n) (void)]
    [(_) (void)]))

(edges v1 <-> v2 -> v0 -> v3 <- v2)
(check-expect (set-member? (node-edges v1) 'v2) #t) 
(check-expect (set-member? (node-edges v2) 'v1) #t) 
(check-expect (set-member? (node-edges v1) 'v3) #f) 
(check-expect (set-member? (node-edges v0) 'v3) #t) 










(define-syntax does
  (syntax-rules (have an a bidirectional edge path from ?)
    [(does dst-node have an edge from src-node ?)
     (set-member? (node-edges src-node)
                  (quote dst-node))]
    [(does dst-node have a bidirectional edge from src-node ?)
     (and (does dst-node have an edge from src-node ?)
          (does src-node have an edge from dst-node ?))]
    [(does dst-node have a path from src-node ?)
     (does-path-exist? src-node dst-node)]))

(check-expect (does v0 have an edge from v1 ?) #f) 
(check-expect (does v0 have an edge from v2 ?) #t) 
(check-expect (does v1 have an edge from v0 ?) #t) 
(check-expect (does v2 have a bidirectional edge from v1 ?) #t) 
(check-expect (does v4 have a bidirectional edge from v1 ?) #f) 
(check-expect (does v1 have a bidirectional edge from v0 ?) #f) 

(check-expect (does v1 have a path from v1 ?) #t) 
(check-expect (does v1 have a path from v0 ?) #t) 
(check-expect (does v0 have a path from v1 ?) #t) 
(check-expect (does v4 have a path from v1 ?) #f) 





(define (does-path-exist? src dst)
  (local [(define (fn-for-node n todo visited)
            (cond [(eq? (node-name n)
                        (node-name dst))
                   true]
                  [(member (node-name n) visited)
                   (fn-for-lon todo visited)]
                  [else (fn-for-lon (append (set->list (node-edges n)) todo)
                                    (cons (node-name n) visited))]))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else (fn-for-node (my-eval (first todo))
                                     (rest todo) visited)]))]
    (fn-for-node src empty empty)))





(define (path src dst)
  (local [(define (display lon)
            (string-join (map symbol->string
                              (reverse lon))
                         " -> "))
          (define (pair lon rsf)
            (cond
              [(empty? lon) empty]
              [else
               (cons (cons (first lon)
                           rsf)
                     (pair (rest lon)
                           rsf))]))
          
          (define (fn-for-lon lon todo visited)
            (let ([node (my-eval (first lon))])
              (cond [(eq? (node-name node)
                          (node-name dst))
                     lon]
                    [(member (node-name node)
                             visited)
                     (fn-for-lolon todo
                                   visited)]
                    [else (fn-for-lolon (append todo
                                                (pair (set->list (node-edges node))
                                                      lon))
                                        (cons (node-name node)
                                              visited))])))
          (define (fn-for-lolon todo visited)
            (cond [(empty? todo) false]
                  [else (fn-for-lon (first todo)
                                    (rest todo)
                                    visited)]))]
    (let ([result (fn-for-lon (list (node-name src))
                              empty
                              empty)])
      
      
      
      
      (and result (display result))))) 

(new graph complicated)
(new vertex cv0 in complicated)
(new vertex cv1 in complicated)
(new vertex cv2 in complicated)
(new vertex cv3 in complicated)
(new vertex cv4 in complicated)
(new vertex cv5 in complicated)
(new vertex cv6 in complicated)

(edges cv0 -> cv1 -> cv4 <- cv1 -> cv2 -> cv3 -> cv4 -> cv5 <- cv6 -> cv1)
(edges cv0 -> cv5)

(check-expect (path cv1 cv4) "cv1 -> cv4")
(check-expect (path cv0 cv5) "cv0 -> cv5")
(check-expect (path cv4 cv5) "cv4 -> cv5")
(check-expect (path cv5 cv5) "cv5")
(check-expect (path cv1 cv5) "cv1 -> cv4 -> cv5")
(check-expect (path cv2 cv5) "cv2 -> cv3 -> cv4 -> cv5")
(check-expect (path cv0 cv6) #f)


(test)