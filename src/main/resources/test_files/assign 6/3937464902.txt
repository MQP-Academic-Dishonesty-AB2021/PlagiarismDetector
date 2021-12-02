#lang racket


(require test-engine/racket-tests)




(define-struct graph (name nodes) #:transparent #:mutable)


(define-struct node (name edges) #:transparent #:mutable)

(define g0 (make-graph 'g0 '(n0 n1 n2 n3 n4)))

(define n4 (make-node 'n4 '(n3)))
(define n3 (make-node 'n3 '(n4)))
(define n2 (make-node 'n2 '()))
(define n1 (make-node 'n1 '(n2 n3)))
(define n0 (make-node 'n0 '(n1)))




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
    [(create name value)
     (begin
       (define name value)
       (set! name value))]))





(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph a)
     (create a (make-graph 'a empty))]
    [(new vertex a in b)
     (begin
       (create a (make-node 'a empty))
       (cond
         [(not (false? (add-unique (node-name a)(graph-nodes b))))
          
          (set! b
                (graph
                 (graph-name b)
                 (append
                  (graph-nodes b)
                  (list (node-name a)))))]
         [else
          (void)]))]))







(define (add-unique n lst)
  (cond
    [(empty? lst)
     true]
    [(not (false? (equal? n (car lst))))
     false]
    [else
     (add-unique n (rest lst))]))
    






(define-syntax edges
  (syntax-rules (-> <- <->)
    [(edges a -> b)
     (set! a
           (node
            (node-name a)
            (append
             (node-edges a)
             (list (node-name b)))))]
    [(edges a <- b)
     (set! b
           (node
            (node-name b)
            (append
             (node-edges b)
             (list (node-name a)))))]
    [(edges a <-> b)
     (begin
       (set! a
             (node
              (node-name a)
              (append
               (node-edges a)
               (list (node-name b)))))
       (set! b
             (node
              (node-name b)
              (append
               (node-edges b)
               (list (node-name a))))))]))
     







(define (search-list n lst)
    (cond
      [(empty? lst) false]
      [(equal? n (car lst))
       true]
      [else
       (search-list n (rest lst))]))






(define (fn-element n end)
  (begin
    (set! n (my-eval n))
    (if (same-node n end)
        true
        (fn-lon (node-edges n) end))))






(define (same-node x y)
  (equal? x y))






(define (fn-lon lon end)
  (cond
    [(empty? lon) false]
    [(or
      (fn-element (first lon) end )
      (fn-lon (rest lon) end))]))
     




(check-expect (does n1 have an edge from n0 ?) true)
(check-expect (does n0 have an edge from n1 ?) false)
(check-expect (does n3 have a bidirectional edge from n4 ?) true)
(check-expect (does n1 have a bidirectional edge from n0 ?) false)


(define-syntax does
  (syntax-rules (have an edge from)
    [(does x have an edge from y ?)
     (cond
       [(not(false? (search-list (node-name x) (node-edges y))))
        true]
       [else
        false])]
    [(does x have a bidirectional edge from y ?)
     (cond
       [(and
         (not(false? (search-list (node-name y) (node-edges x))))
         (not(false? (search-list (node-name x) (node-edges y))))
         )]
       [else
        false])]
    [(does x have a path from y ?)
     (if (not(false? (fn-element y x)))
         true
         false)]))
