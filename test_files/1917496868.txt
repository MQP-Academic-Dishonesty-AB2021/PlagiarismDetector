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












(check-expect (add-unique empty (quote n0)) (list 'n0))
(check-expect (add-unique (list 'n0) (quote n0)) (list 'n0))
(check-expect (add-unique (list 'n0) (quote n1)) (list 'n1 'n0))

(define (add-unique los0 symbol)
  (if (member symbol los0) los0 (cons symbol los0)))




(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph graph-symbol)
     (create graph-symbol (make-graph (quote graph-symbol) empty))]
    [(new vertex node-symbol in graph-symbol)
     (begin (create node-symbol (make-node (quote node-symbol) empty))
            (set-graph-nodes! graph-symbol (add-unique (graph-nodes graph-symbol) (quote node-symbol))))]))


(define-syntax create
  (syntax-rules ()
    [(create var value)
     (begin
       (define var value)
       (set! var value))]))


(define-syntax edge
  (syntax-rules ()
    [(edge n0 n1)
     (set-node-edges! n0 (add-unique (node-edges n0) (quote n1)))]))


(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges n0 -> n1) (edge n0 n1)]
    [(edges n0 <- n1) (edge n1 n0)]
    [(edges n0 <-> n1) (begin (edge n0 n1)
                              (edge n1 n0))]
    [(edges n0 fn n1 fn2 n2 ...)
     (begin (edges n0 fn n1)
            (edges n1 fn2 n2 ...))]))


(new graph g0)
(new vertex n0 in g0)
(new vertex n1 in g0)
(new vertex n2 in g0)
(new vertex n3 in g0)
(edges n0 -> n1 <-> n2)

(new graph g1)
(new vertex n5 in g1)
(new vertex n7 in g1)
(new vertex n9 in g1)
(new vertex n11 in g1)
(edges n5 -> n7 -> n9 -> n5)
(edges n11 -> n11)



(check-expect (have-path? n0 n1) true)
(check-expect (have-path? n1 n0) false)
(check-expect (have-path? n1 n2) true)
(check-expect (have-path? n2 n1) true)
(check-expect (have-path? n0 n2) true)
(check-expect (have-path? n2 n0) false)
(check-expect (have-path? n0 n5) false)
(check-expect (have-path? n5 n5) true)
(check-expect (have-path? n0 n0) false)
(check-expect (have-path? n11 n11) true)

(define (have-path? n0 n1)
  (local [(define (fn-for-node n todo visited)
            (cond [(equal? n n1) true]
                  [else
                   (if (member n visited)
                       (fn-for-lon todo visited)
                       (fn-for-lon (append (node-edges n) todo)
                                   (cons n visited)))]))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [else (fn-for-node (my-eval (first todo)) (rest todo) visited)]))]
    (if (equal? n0 n1)
        (fn-for-node (my-eval (first (node-edges n0))) (rest (node-edges n0)) empty)
        (fn-for-node n0 empty empty))))



(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n2 have an edge from n0 ?) false)
(check-expect (does n1 have a bidirectional edge from n2 ?) true)
(check-expect (does n2 have a path from n0 ?) true)

(define-syntax does
  (syntax-rules (have a an edge bidirectional from ? path)
    [(does n1 have an edge from n0 ?)
     (if (member (quote n1) (node-edges n0)) true false)]
    [(does n1 have a bidirectional edge from n0 ?)
     (and (does n1 have an edge from n0 ?)
          (does n0 have an edge from n1 ?))]
    [(does n1 have a path from n0 ?)
     (have-path? n0 n1)]))



(check-expect (path n0 n1) "n0 -> n1")
(check-expect (path n0 n2) "n0 -> n1 -> n2")
(check-expect (path n5 n5) "n5 -> n7 -> n9 -> n5")
(check-expect (path n5 n0) "")
(check-expect (path n11 n11) "n11 -> n11")

(define (path n0 n1)
  (local [(define (get-path n todo visited str)
            (cond [(equal? n n1) (string-append str (symbol->string (node-name n)))]
                  [(member n visited)
                   (fn-for-lon todo visited str)]
                  [(have-path? n n1)
                   (fn-for-lon (append (node-edges n) todo)
                               (cons n visited) (string-append str (symbol->string (node-name n)) " -> "))]
                  [else
                   (fn-for-lon (append (node-edges n) todo)
                               (cons n visited) str)]))
          (define (fn-for-lon todo visited str)
            (cond [(empty? todo) ""]
                  [else (get-path (my-eval (first todo)) (rest todo) visited str)]))]
    (if (equal? n0 n1)
        (get-path (my-eval (first (node-edges n0))) (rest (node-edges n0)) empty
                  (string-append (symbol->string (node-name n0)) " -> "))
        (get-path n0 empty empty ""))))

(test)

