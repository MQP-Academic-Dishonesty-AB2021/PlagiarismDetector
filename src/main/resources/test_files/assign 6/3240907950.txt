



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



(define-syntax-rule (create name value)
  (begin
    (define name value)
    (set! name value)))


(define TN1 (make-node (quote TN1) '()))
(define TN2 (make-node (quote TN2) '()))
(define TN3 (make-node (quote TN3) '(TN1 TN2 TN4)))
(define TN4 (make-node (quote TN4) '(TN2 TN3)))

(define TG1 (make-graph (quote tg1) '()))
(define TG2 (make-graph (quote tg2) (list 'TN2)))




(check-expect (add-unique 'TN1 (graph-nodes TG1)) '(TN1))
(check-expect (add-unique 'TN2 (graph-nodes TG2)) '(TN2))
(check-expect (add-unique 'TN1 (graph-nodes TG2)) '(TN1 TN2))

(define (add-unique node lon)
  (local
    [(define (inner anode ilon)
       (cond
         [(empty? ilon) (cons anode lon)]
         [else
          (if (eq? anode (first ilon))
              lon
              (inner anode (rest ilon)))]
         ))]
    (inner node lon)))





(check-expect (add-unique-lon (node-edges TN2) (graph-nodes TG1)) '())
(check-expect (add-unique-lon (node-edges TN2) (graph-nodes TG2)) '(TN2))
(check-expect (add-unique-lon (node-edges TN3) (graph-nodes TG2)) '(TN4 TN1 TN2))

(define (add-unique-lon lon1 lon2)
  (local
    [(define (inner-lon lontoadd lon)
       (if (empty? lontoadd)
           lon
           (inner-lon (rest lontoadd) (add-unique (first lontoadd) lon))))
     ]
    (inner-lon lon1 lon2)))




(define-syntax new
  (syntax-rules (graph vertex in)
    [(new graph g0)
     (create g0 (make-graph (quote g0) empty))]
    [(new vertex n0)
     (create n0 (make-node (quote n0) empty))]
    [(new vertex n0 in g0)
     (begin
       (new vertex n0)
       
       (set-graph-nodes! g0 (add-unique 'n0 (graph-nodes g0))))]))


(define-syntax edge
  (syntax-rules (<- <-> ->)
    [(edge n0 -> n1)
     (set-node-edges! n0 (add-unique 'n1 (node-edges n0)))]
    [(edge n0 <- n1)
     (set-node-edges! n1 (add-unique 'n0 (node-edges n0)))]
    [(edge n0 <-> n1)
     (begin 
       (edge n0 <- n1)
       (edge n0 -> n1))]
    [(edge n0 arrow n1 n2 ...)
     (begin
       (edge n0 arrow n1)
       (edge  n1 n2 ...))]
    ))



(check-expect (does TN1 have an edge from TN2 ?) false)
(check-expect (does TN3 have an edge from TN2 ?) false)
(check-expect (does TN2 have an edge from TN3 ?) true)
(check-expect (does TN3 have a bidirectional edge from TN4 ?) true)
(check-expect (does TN2 have a bidirectional edge from TN3 ?) false)

(check-expect (does TN1 have a path from TN4 ?) true)
(check-expect (does TN4 have a path from TN1 ?) false)
(check-expect (does TN2 have a path from TN4 ?) true)
(check-expect (does TN2 have a path from TN1 ?) false)
(check-expect (does TN3 have a path from TN4 ?) true) 

(define-syntax does
  (syntax-rules (have an a bidirectional edge path from ?)
    [(does n1 have an edge from n0 ?)
     (not (false? (member (quote n1) (node-edges n0))))]
    [(does n1 have a bidirectional edge from n0 ?)
     (and (does n1 have an edge from n0 ?) (does n0 have an edge from n1 ?) )]
    [(does n1 have a path from n0 ?)
     (local [
             (define (inner? nin todo total)
               (if (does n1 have an edge from nin ?)
                   (inner-lon? (add-unique-lon (node-edges nin) todo) true)
                   (inner-lon? (add-unique-lon (node-edges nin) todo) total)))
             (define (inner-lon? todo total)
               (cond
                 [total true]
                 [(empty? todo) false]
                 [else (inner? (my-eval (first todo)) (rest todo) total)]
                 ))]
       (inner? n0 (node-edges n0) false)
       )]))
              


 
     
(test)