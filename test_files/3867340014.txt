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
    [(create n1 a1)
     (begin (define n1 a1)
            (set! n1 n1))]))
   


(check-expect (add-unique 'n1 empty) (list 'n1))
(check-expect (add-unique 'n1 (list 'n1)) (list 'n1))

(define (add-unique s los)
  (cond [(member s los) los]
        [else"n0 -> n2 -> n3"
             (cons s los)]))
             

(define-syntax new
  (syntax-rules [graph vertex in]
    [(new graph name1)
     (create name1 (make-graph (quote name1) (list)))]
    [(new vertex v1 in gr1)
     (begin (create v1 (make-node (quote v1) (list)))
            (set-graph-nodes! gr1 (add-unique (node-name v1) (graph-nodes gr1))))]))
     
(new graph g0)
(new vertex n0 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(new vertex n4 in g0)
(new vertex n5 in g0)


(define-syntax edge
  (syntax-rules ()
    [(edge e1 e2)
     (begin (set-node-edges! e1 (add-unique (node-name e2) (node-edges e1))))]))

(define-syntax edges
  (syntax-rules (-> <- <-> edge)
    [(edges e1 -> e2)
     (begin (set-node-edges! e1 (add-unique (node-name e2) (node-edges e1))) e2)]
    [(edges e1 <- e2)
     (begin (set-node-edges! e2 (add-unique (node-name e1)(node-edges e2))) e1)]
    [(edges e1 <-> e2)
     (begin  (edges e1 <- e2)
             (edges e1 -> e2) e2)]
    [(edges e1 e2)
     (set-node-edges! e1 (add-unique (node-name e2) (node-edges e1)))]
    [(edges e1 o1 e2 o2 ...)
     (edges (edges e1 o1 e2) o2 ...)]))

(edges n0 -> n2 <-> n3 <- n4)
(edges n3 <-> n5)




(check-expect (equal n0 n2) false)
(check-expect (equal n2 n0) true)
(define (equal e1 e2)
  (local [(define (eq e1 e2 visited)
            (cond [(empty? (node-edges e2)) false]
                  [(member (first (node-edges e2)) visited) false]
                  [(equal? e1 (my-eval (first (node-edges e2)))) true]
                  [else
                   (eq e1
                       (my-eval (first (node-edges e2)))
                       (cons (first (node-edges e2))
                             visited))]))]
    (eq e1 e2 empty)))
          

(check-expect (does n2 have an edge from n0 ?) true)
(check-expect (does n2 have a birdirectional edge from n0 ?) false)
(check-expect (does n2 have a birdirectional edge from n3 ?) true)
(check-expect (does n3 have an edge from n5 ?) true)

(check-expect (does n0 have a path from n2 ?) false)
(check-expect (does n2 have a path from n0 ?) true)

(define-syntax does
  (syntax-rules (have an edge from a bidirectional ? path)
    [(does e1 have an edge from e2 ?)
     (if (member (node-name e1) (node-edges e2))
         true
         false)]
    [(does e1 have a birdirectional edge from e2 ?)
     (if (and (member (node-name e1) (node-edges e2))
              (member (node-name e2) (node-edges e1)))
         true
         false)]
    [(does e1 have a path from e2 ?)
     (equal e1 e2)]))
 






(check-expect (path n3 n0) "n0 -> n2 -> n3")
(check-expect (path n0 n2) "")

(define (path e1 e2)
  (local [(define (p e1 e2 visited a)
            (cond [(empty? (node-edges e2)) ""]
                  [(member (first (node-edges e2)) visited) ""]
                  [(equal? e1 (my-eval (first (node-edges e2)))) (string-append a (symbol->string (node-name e1)))]
                  [else
                   (p e1
                      (my-eval (first (node-edges e2)))
                      (cons (first (node-edges e2)) visited)
                      (string-append  a (symbol->string (first (node-edges e2))) " -> "))]))]
    (p e1 e2  empty (string-append (symbol->string (node-name e2)) " -> "))))


(test)
