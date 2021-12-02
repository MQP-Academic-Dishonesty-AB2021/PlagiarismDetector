#lang racket
(require test-engine/racket-tests)
(require racket/struct)














































(define-syntax-rule (values->list v)
  (call-with-values (thunk v) list))
(define (vcons ele lox)
  (if (void? ele) lox (cons ele lox)))
(define (endcons x lox) (append lox (list x)))




(define-struct node (name exits) #:transparent #:mutable)





(define (defined? id)
  (not (false? (identifier-binding id))))

(check-expect (defined? #'this-is-not-defined) #f)
(check-expect (defined? #'node) #t)




(define-namespace-anchor namespace-anchor)
(define top (namespace-anchor->namespace namespace-anchor))

(define-syntax-rule (get dat) (namespace-variable-value 'dat #f (thunk #f) top))
(define (get-value dat) (namespace-variable-value dat #f (thunk #f) top))


(define (get-name str) (string-replace (first (string-split (string-append str "/") "/")) "_" " "))
(check-expect (get-name "nate") "nate")
(check-expect (get-name "node/1.1") "node")
(check-expect (get-name "level_1-1") "level 1-1")





(define (create-node id)
  (and (false? (get-value id))
       (or (namespace-set-variable-value! id (make-node (get-name (symbol->string id)) empty)
                                          #f top #t))))



(define (add-exit id exit)
  (create-node id)
  (and (not (false? exit))
       (begin 
         (create-node exit)
         (set-node-exits! (get-value id) (endcons (get-value exit) (node-exits (get-value id))))))(void))


(add-exit 'exit-test1 #f)
(add-exit 'exit-test2 'exit-test1)
(add-exit 'exit-test3 'exit-test3)
(add-exit 'exit-test4 'exit-test3)
(add-exit 'exit-test4 'exit-test2)
(check-expect (get exit-test1) (node "exit-test1" empty))
(check-expect (get exit-test2) (node "exit-test2" (list (get exit-test1))))
(check-expect (get exit-test3) (node "exit-test3" (list (get exit-test3))))
(check-expect (get exit-test4) (node "exit-test4" (list (get exit-test3) (get exit-test2))))

(define (add-exits lod los)
  (local [(define (exits loc len index dex)
            (if (empty? loc) (add-exit (list-ref lod index) #f)
                (begin (cond
                         [(char=? (first loc) #\>) (add-exit (list-ref lod index) (list-ref lod dex))]
                         [(char=? (first loc) #\<) (add-exit (list-ref lod (add1 index)) (list-ref lod (- dex len)))])
                       (exits (rest loc) len index (add1 dex)))))
          (define (path index) (exits (string->list (list-ref los index)) (string-length (list-ref los index)) index (add1 index)))]
    (begin (map path (build-list (length los) identity)))) (void))




(define (to-datum dat)
  (let [(try (symbol->string (syntax->datum dat)))]
    (if (false? (regexp-match #rx"[^<>-]" try))
        try
        (syntax->datum dat))))



(define (split-sytax los)
  (begin
    (cond
      [(string? (first los)) (error "Paths should not be first")]
      [(string? (last los)) (error "Paths should not be last")])
    (define-struct fold (datums paths bool))
    (define (part ele fold) (begin
                              (and (fold-bool fold) (string? ele)
                                   (error "Path objects should not be twice in a row"))
                              (define str (cond
                                            [(string? ele) ele]
                                            [(not (fold-bool fold)) ""]))
                              (make-fold
                               (vcons (cond [(not (string? ele)) ele]) (fold-datums fold))
                               (vcons str (fold-paths fold))
                               (string? ele))))
    (define val (foldr part (make-fold empty empty #t) los))
    (values (fold-datums val) (fold-paths val))))
    



(check-error (split-sytax '("<" split-error split-error))) 
(check-error (split-sytax '(split-error ">"))) 
(check-error (split-sytax '(split-error1 ">" "<" split-error2))) 


(check-expect (values->list (split-sytax '(split-test1 "><" split-test2)))
              (list '(split-test1 split-test2) '("><"))) 
(check-expect (values->list (split-sytax '(split-test1 "><" split-test2 split-test3 "><" split-test4)))
              (list '(split-test1 split-test2 split-test3 split-test4) '("><" "" "><")))
(check-expect (values->list (split-sytax '(split-test1 "><" split-test2 split-test3 split-test4 "><" split-test5)))
              (list '(split-test1 split-test2 split-test3 split-test4 split-test5) '("><" "" "" "><")))

(define-syntax-rule (make-map ids ... )
  (apply add-exits (values->list (split-sytax (list (to-datum #'ids) ...)))))

(make-map map-test1 >< map-test2)
(make-map map-test3 < map-test3)
(make-map map-test4 -> map-test5 <- map-test6)
(make-map map-test7 -->< map-test8 <- map-test9 <-- map-test10)
(check-expect (get map-test1) (make-node "map-test1" (list (get map-test2))))
(check-expect (get map-test2) (make-node "map-test2" (list (get map-test1))))
(check-expect (get map-test3) (make-node "map-test3" (list (get map-test3))))
(check-expect (get map-test4) (make-node "map-test4" (list (get map-test6))))
(check-expect (get map-test5) (make-node "map-test5" empty))
(check-expect (get map-test6) (make-node "map-test6" (list (get map-test4))))
(check-expect (get map-test7) (make-node "map-test7" (list (get map-test10))))
(check-expect (get map-test8) (make-node "map-test8" (list (get map-test7))))
(check-expect (get map-test9) (make-node "map-test9" (list (get map-test7))))
(check-expect (get map-test10) (make-node "map-test10" (list (get map-test7))))
(test)
(newline)


















(make-map start-1 >< node/1.1 >--------->< level_1-1 >< node/1.2 >< level_1-2 >---><
          level_1-3 >< node/1.3 >< house_1-A >< level_1-4 ><----< node/1.4 >< card/1
          >< fort/1 <>--------- door/1 ><< node/1.5 >< node/1.6 >< level_1-5
          >< node/1.7 >< level_1-6 >< node/1.8 >>< house_1-B <- bandit/1 >< castle/1)



(make-map castle/1 >< start-2 >< node/2.1 >---->< node/2.2 >< node/2.3 >< door/2 >< pipe/2 >< house_2-A
          <----- node/2.4 >< level_2-1 >< node/2.5 >< node/2.6 >< node/2.7 >>< card/2 <- level_2-2 >< node/2.8 >< fort/2)

(define (game node)
  (display (string-append* "-------- " (node-name node) " --------\nGo to:" (endcons "\nOther(id):" (map
                                       (λ (index) (string-append "\n" (~a (add1 index) ": " (node-name (list-ref (node-exits node) index)))))
                                       (build-list (length (node-exits node)) identity)))))
  (define (input)
    (define str (read-line (current-input-port)))
    (define num (string->number str))
    (cond
      [(string=? str "q") (void)]
      [(get-value (string->symbol str)) (game (get-value (string->symbol str)))]
      [(or (false? num) (not (positive? num)) (> num (length (node-exits node)))) (begin (display "Try-again:" ) (input))]
      [(game (list-ref (node-exits node) (sub1 num)))]))
  (input))
(display "press q to quit\nthe first option almost always takes you backward\n")
(game (get start-1))






































































































