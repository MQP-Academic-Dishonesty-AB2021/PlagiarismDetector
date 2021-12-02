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
    [(create variable-name variable-data)
     (begin (define variable-name variable-data)
            (set! variable-name variable-data))]))








(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph graph-name)
     (create graph-name (make-graph 'graph-name empty))]
    [(new vertex vertex-name in graph-name)
     (begin (create vertex-name (make-node 'vertex-name empty))
            (add-unique 'vertex-name graph-name))]))







(define (add-unique node base)
  (if (graph? base)
      (if (member node (graph-nodes base))
          "Node already exists in graph"
          (set-graph-nodes! base (cons node (graph-nodes base))))
      (if (member node (node-edges base))
          "Node already exists in edges"
          (set-node-edges! base (cons node (node-edges base))))))






















































          













(define-syntax edge
  (syntax-rules ()
    [(edge base edge)
     (add-unique 'edge base)]))














(define-syntax edges
  (syntax-rules (<- <-> ->)
    [(edges b <- e)
     (edge e b)]
    [(edges b <-> e)
     (begin (edge e b)
            (edge b e))]
    [(edges b -> e)
     (edge b e)]
    [(edges b op1 e op2 ...)
     (begin (edges b op1 e)
            (edges e op2 ...))]))

















(check-expect (does n1 have an edge from n0 ?) #t)
(check-expect (does n0 have an edge from n1 ?) #f)
(check-expect (does n2 have a bidirectional edge from n1 ?) #t)
(check-expect (does n2 have a path from n0 ?) #t)

(check-expect (does s0 have an edge from s6 ?) #f)
(check-expect (does s0 have an edge from s5 ?) #t)
(check-expect (does s3 have an edge from s1 ?) #f)
(check-expect (does s1 have an edge from s3 ?) #t)
(check-expect (does s0 have a bidirectional edge from s5 ?) #t)
(check-expect (does s5 have a bidirectional edge from s0 ?) #t)
(check-expect (does s5 have a path from s6 ?) #f)
(check-expect (does s6 have a path from s3 ?) #t)
(check-expect (does s5 have a path from s0 ?) #t)

(define-syntax does
  (syntax-rules (have edge bidirectional from ?)

    [(does n1 have an edge from n2 ?)
     (not (false? (member 'n1 (node-edges n2))))]

    [(does n1 have a bidirectional edge from n2 ?)
     (not (false? (and (member 'n1 (node-edges n2))
                       (member 'n2 (node-edges n1)))))]

    [(does n1 have a path from n2 ?)
     (path? n2 n1)]))













(check-expect (path? s6  s5) #f)
(check-expect (path? s3  s6) #t)
(check-expect (path? s0  s5) #t)
(check-expect (path? n0  s3) #f)
(check-expect (path? s3  n1) #f)
(check-expect (path? s6  s0) #f)
(check-expect (path? s1  s4) #t)
(check-expect (path? n10 n1) #f)

(define (path? n1 n2)
  (local
    [(define (path--node n todo visited)
       (cond
         [(equal? (my-eval n) n2) true]
         [(member n visited)
          (if (empty? todo)
              false
              (path--lon (rest todo) visited))]
         [else
          (path--lon (append (node-edges (my-eval n)) todo) (cons n visited))]))
     
     (define (path--lon todo visited)
       (cond 
         [(empty? todo) false]
         [else
          (path--node (first todo) (rest todo) visited)]))]
    (path--node n1 empty empty)))













(check-expect (path n0 n10) "n0->n9->n10")
(check-expect (path s0 s5) "s0->s5")
(check-expect (path s0 s3) "s0->s4->s3")
(check-expect (path s1 s5) "s1->s0->s5")
(check-expect (path s3 s6) "s3->s1->s0->s5->s6")
(check-expect (path s5 s6) "s5->s6")
(check-expect (path s0 s1) "s0->s5->s1")
(check-expect (path s6 s5) "")
(check-expect (path n0 s6) "")

(define (path n1 n2)
  (local
    
    [(define (path--node n visited ssf rsf)
       (cond
         [(equal? (my-eval n) n2)
          (local
            [(define str (string-append ssf (symbol->string (node-name n2))))]
            (if (member str rsf)
                false
                str))]
         [(member n visited) false]
         [else
          (path--lon
           (node-edges (my-eval n))
           (cons n visited)
           (string-append ssf (symbol->string n) "->")
           rsf)]))
     
     (define (path--lon lon visited ssf rsf)
       (cond 
         [(empty? lon) false]
         [else
          (local
            [(define try1
               (path--node (first lon) visited ssf rsf))
             (define try2
               (path--lon (rest lon) visited ssf rsf))]
            (if (or try1 try2)
                (or try1 try2)
                false))]))

     (define (recur result)
       (local
         [(define res (path--node (node-name n1) empty "" result))]
         (if (false? res)
             result
             (recur (cons res result)))))

     (define (smallest los)
       (if (empty? los)
           ""
           (first (sort los (λ (s1 s2) (< (path-length s1) (path-length s2)))))))

     (define (path-length str)
       (length (string-split str "->")))]
    
    (smallest (recur empty))))





(new graph g0)

(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n9 in g0)
(new vertex n10 in g0)

(edges n0 -> n1 <-> n2 -> n0 -> n9 <- n2)
(edge n9 n10)


(new graph g1)

(new vertex s0 in g1)
(new vertex s1 in g1)
(new vertex s3 in g1)
(new vertex s4 in g1)
(new vertex s5 in g1)
(new vertex s6 in g1)

(edges s0 -> s4 -> s3 -> s1 -> s0 <-> s5)
(edge s5 s1)
(edge s5 s6)



(test)