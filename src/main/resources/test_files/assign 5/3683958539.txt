

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
[define F1 [make-widget "F1" 7 32]]
[define G1 [make-widget "G1" 4 35]]
[define H1 [make-widget "H1" 15 25]]
[define J1 [make-widget "J1" 10 15]]
[define X1 [make-widget "X1" 30 15]]

[define BST1 [make-bst A1 false false]]
[define BST2 [make-bst W1
                       [make-bst D1
                                 [make-bst A1 false false] false]
                       [make-bst Z1 false false]]]
[define BST3 [make-bst A1
                       [make-bst W1 false false]
                       [make-bst Z1 [make-bst D1 false false] false]]]
(define BST4 (make-bst D1
                       (make-bst Z1 (make-bst F1 false false) false)
                       (make-bst A1 false (make-bst W1 false false))))

(define DB-quantity (make-db widget-quantity < = BST3))
(define DB-name (make-db widget-name string<? string=? BST2))
(define DB-name-empty (make-db widget-name string<? string=? false))
(define DB-price (make-db widget-price > = BST4))



[check-expect [find "E1" DB-name] false]

[check-expect [find "W1" DB-name] W1]
[check-expect [find "A1" DB-name] A1]
[check-expect [find "Z1" DB-name] Z1]
[check-expect [find 1 DB-quantity] W1]
[check-expect [find 51 DB-quantity] Z1]
[check-expect [find 5 DB-quantity] D1]
[check-expect [find 16 DB-price] Z1]
[check-expect [find 1 DB-price] W1]



(define (find key db)
  (local [(define (find-inner bst)
            (cond [(false? bst) false]
                  [((db-eq? db) key ((db-field db) (bst-widget bst)))
                   (bst-widget bst)]
                  [else (if ((db-lt? db) key ((db-field db)
                                              (bst-widget bst)))
                            (find-inner (bst-left bst))
                            (find-inner (bst-right bst)))]))]
    (find-inner (db-bst db))))



[check-expect [db-bst [insert A1 DB-name-empty]]
              [make-bst A1 false false]]

[check-expect [db-bst [insert F1 DB-name]]
              [make-bst W1
                        [make-bst D1
                                  [make-bst A1 false false]
                                  [make-bst F1 false false]]
                        [make-bst Z1 false false]]]
[check-expect [db-bst [insert F1 DB-quantity]]
              [make-bst A1
                        [make-bst W1 false false]
                        [make-bst Z1
                                  [make-bst D1 false
                                            [make-bst F1 false false]]
                                  false]]]





(define (insert wid db)
  (local [(define (insert-inner key bst)
            (cond [(false? bst) (make-bst key false false)]
                  [((db-lt? db) [[db-field db] key] ((db-field db)
                                                     (bst-widget bst))) 
                   (make-bst (bst-widget bst)
                             (insert-inner key (bst-left bst))
                             (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst)
                             (bst-left bst)
                             (insert-inner key (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (insert-inner wid (db-bst db)))))







[check-expect [db-bst [insert! A1 DB-name-empty]]
              [make-bst A1 false false]]

[check-expect [db-bst [insert! F1 DB-name]]
              [make-bst W1
                        [make-bst D1
                                  [make-bst A1 false false]
                                  [make-bst F1 false false]]
                        [make-bst Z1 false false]]]
[check-expect [db-bst [insert! F1 DB-quantity]]
              [make-bst A1
                        [make-bst W1 false false]
                        [make-bst Z1
                                  [make-bst D1 false
                                            [make-bst F1 false false]]
                                  false]]]
[check-expect [db-bst [insert! G1 DB-price]]
              (make-bst D1
                        (make-bst Z1 (make-bst F1
                                               [make-bst G1 false false]
                                               false) false)
                        (make-bst A1 false (make-bst W1 false false)))] 
 


(define (insert! wid db)
  (local [(define (insert-inner bst)
            (cond [(false? bst) (make-bst wid false false)]
                  [else (if ((db-lt? db) ((db-field db) wid)
                                         ((db-field db)
                                          (bst-widget bst)))
                            (if (false? (bst-left bst))
                                (begin
                                  (set-bst-left! bst
                                                 (make-bst wid false false))
                                  (db-bst db))
                                (insert-inner (bst-left bst)))
                            (if (false? (bst-right bst))
                                (begin (set-bst-right! bst
                                                       (make-bst wid
                                                                 false false))
                                       (db-bst db))
                                (insert-inner (bst-right bst))))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-inner
                                                    (db-bst db)))))



[define BST-left-left [make-bst J1
                                [make-bst D1
                                          [make-bst A1
                                                    [make-bst W1 false false]
                                                    false]
                                          false]
                                [make-bst H1 false false]]]

[define BST-left-left2 [make-bst J1
                                 [make-bst D1
                                           [make-bst A1
                                                     [make-bst W1 false false]
                                                     false]
                                           false]
                                 [make-bst H1 false false]]]

[define BST-left-left3 [make-bst J1
                                 [make-bst D1
                                           [make-bst A1
                                                     [make-bst W1 false false]
                                                     false]
                                           false]
                                 [make-bst H1 false false]]]

[define BST-left-right [make-bst J1
                                 [make-bst D1
                                           [make-bst W1
                                                     false
                                                     [make-bst A1 false false]]
                                           false]
                                 [make-bst H1 false false]]]
[define BST-right-right [make-bst J1
                                  [make-bst D1 false false]
                                  [make-bst H1
                                            false
                                            [make-bst X1
                                                      false
                                                      [make-bst Z1
                                                                false
                                                                false]]]]]
[define BST-right-left [make-bst J1
                                 [make-bst D1 false false]
                                 [make-bst H1
                                           false
                                           [make-bst Z1
                                                     [make-bst X1 false false]
                                                     false]]]]
[define BSTLLR [make-bst J1
                         [make-bst D1
                                   [make-bst A1
                                             [make-bst W1 false false]
                                             [make-bst F1 false false]]
                                   false]
                         [make-bst H1 false false]]]

[define BSTLLR2 [make-bst J1
                          [make-bst D1
                                    [make-bst A1
                                              [make-bst W1 false false]
                                              [make-bst F1 false false]]
                                    false]
                          [make-bst H1 false false]]]


[define BSTA1 [make-bst G1
                        [make-bst A1
                                  false
                                  [make-bst F1
                                            [make-bst D1 false false]
                                            false]]
                        [make-bst W1
                                  [make-bst J1
                                            [make-bst H1 false false]
                                            [make-bst X1 false false]]
                                  [make-bst Z1 false false]]]]


[define BSTA [make-bst J1
                       [make-bst D1
                                 [make-bst A1 false false]
                                 false]
                       [make-bst H1 false false]]]
[define BSTB [make-bst J1
                       [make-bst D1
                                 [make-bst W1 false false]
                                 false]
                       [make-bst H1 false false]]]
[define BSTC [make-bst J1
                       [make-bst D1 false false]
                       [make-bst H1
                                 false
                                 [make-bst X1 false false]]]]
[define BSTD [make-bst J1
                       [make-bst D1 false false]
                       [make-bst H1
                                 false
                                 [make-bst Z1 false false]]]]
[define BSTE [make-bst F1
                       [make-bst D1
                                 false
                                 false]
                       [make-bst Z1 false false]]]

(define DBA (make-db widget-quantity < = BSTA))
(define DBB (make-db widget-quantity < = BSTB))
(define DBC (make-db widget-quantity < = BSTC))
(define DBD (make-db widget-quantity < = BSTD))
(define DBE (make-db widget-quantity < = BSTE))





[check-expect [db-bst [insert-avl A1 DBE]]
              [make-bst F1
                        [make-bst D1
                                  [make-bst A1 false false]
                                  false]
                        [make-bst Z1 false false]]]


[check-expect [db-bst [insert-avl W1 DBA]] [balance-tree BST-left-left]]

[check-expect [db-bst [insert-avl A1 DBB]] [balance-tree BST-left-right]]

[check-expect [db-bst [insert-avl Z1 DBC]] [balance-tree BST-right-right]]

[check-expect [db-bst [insert-avl X1 DBD]] [balance-tree BST-right-left]]



(define (insert-avl wid db)
  (local [(define (insert-inner key bst)
            (cond [(false? bst) (make-bst key false false)]
                  [((db-lt? db) [[db-field db] key] ((db-field db)
                                                     (bst-widget bst))) 
                   [balance (make-bst (bst-widget bst)
                                      (insert-inner key (bst-left bst))
                                      (bst-right bst))]]
                  [else
                   [balance (make-bst (bst-widget bst)
                                      (bst-left bst)
                                      (insert-inner key (bst-right bst)))]]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (insert-inner wid (db-bst db)))))

  



                        

[check-expect [balance BSTLLR]
              [make-bst D1
                        [make-bst A1
                                  [make-bst W1 false false]
                                  [make-bst F1 false false]]
                        [make-bst J1 false
                                  [make-bst H1 false false]]]]

[check-expect [balance [bst-left BST-left-left]]
              [make-bst A1
                        [make-bst W1 false false]
                        [make-bst D1 false false]]]

(define (balance bst)
  (cond [(false? bst) false]
        [[>= [height-diff bst] 2] 
         [if [<= [height-diff [bst-left bst]] -1]
             [left-right bst]  
             [left bst]]] 
        [[<= [height-diff bst] -2] 
         [if [>= [height-diff [bst-right bst]] 1]
             [right-left bst]  
             [right bst]]] 
        [else bst]))






[check-expect [balance-tree BST2] BST2]


[check-expect [balance-tree BST-left-left2]
              [make-bst J1
                        [make-bst A1
                                  [make-bst W1 false false]
                                  [make-bst D1 false false]]
                        [make-bst H1 false false]]]

[check-expect [balance-tree BST-left-right]
              [make-bst J1
                        [make-bst A1
                                  [make-bst W1 false false]
                                  [make-bst D1 false false]]
                        [make-bst H1 false false]]]

[check-expect [balance-tree BST-right-right]
              [make-bst J1
                        [make-bst D1 false false]
                        [make-bst X1
                                  [make-bst H1
                                            false false]
                                  [make-bst Z1 false false]]]]

[check-expect [balance-tree BST-right-left]
              [make-bst J1
                        [make-bst D1 false false]
                        [make-bst X1
                                  [make-bst H1 false false]
                                  [make-bst Z1 false false]]]]

[check-expect [balance-tree BSTLLR2]
              [make-bst D1
                        [make-bst A1
                                  [make-bst W1 false false]
                                  [make-bst F1 false false]]
                        [make-bst J1 false
                                  [make-bst H1 false false]]]]

[check-expect [balance-tree BSTA1]
              [make-bst G1
                        [make-bst D1
                                  [make-bst A1 false false]
                                  [make-bst F1 false false]]
                        [make-bst W1
                                  [make-bst J1
                                            [make-bst H1 false false]
                                            [make-bst X1 false false]]
                                  [make-bst Z1 false false]]]]

[define [balance-tree bst]
  [cond [[false? bst] false] 
        [else (local [(define cur [make-bst [bst-widget bst]
                                            [balance-tree [bst-left bst]]
                                            [balance-tree [bst-right bst]]])]
                (balance cur))]]]



[check-expect [left [bst-left BST-left-left3]]
              [make-bst A1
                        [make-bst W1 false false]
                        [make-bst D1 false false]]]

(define (left parent)
  (local [(define child (bst-left parent))
          (define inner-child (bst-right child))]
    (begin
      (set-bst-left! parent inner-child)
      (set-bst-right! child parent)
      child)))



[check-expect [right [bst-right BST-right-right]]
              [make-bst X1
                        [make-bst H1
                                  false false]
                        [make-bst Z1 false false]]]

(define (right parent)
  (local [(define child (bst-right parent))
          (define inner-child (bst-left child))]
    (begin
      (set-bst-right! parent inner-child)
      (set-bst-left! child parent)
      child)))




[check-expect [left-right [bst-left BST-left-right]]
              [make-bst A1
                        [make-bst W1 false false]
                        [make-bst D1 false false]]]

(define (left-right parent)
  [left [begin (set-bst-left! parent (right (bst-left parent)))
               parent]])




[check-expect [right-left [bst-right BST-right-left]]
              [make-bst X1
                        [make-bst H1 false false]
                        [make-bst Z1 false false]]]

(define (right-left parent)
  [right [begin (set-bst-right! parent (left (bst-right parent)))
                parent]])






[define [balanced? db]
  [local
    [[define [inner bst]
       [cond [[false? bst] true]
             [[<= [abs [height-diff bst]] 1]
              [and [inner [bst-left bst]]
                   [inner [bst-right bst]]]]
             [else false]]]]
    [inner [db-bst db]]]]










(define DB-List (make-db widget-quantity < = false))


[define [insert-multiple fn low db]
  [cond [[empty? low] db]
        [else 
         [insert-multiple fn [rest low] [fn [first low]
                                            [make-db (db-field db) (db-lt? db)
                                                     (db-eq? db)
                                                     [db-bst db]]]]]]]

[define [time-find]
  [local [[define low [random-widgets 10000 999999]]
          [define db (make-db widget-quantity < =
                              [db-bst [insert-multiple insert! low DB-List]])]]
    [begin
      [time [map [λ [wid] [find [(db-field db) wid] db]] low]]
      "done"]]]

[define [time-find-avl count]
  (if (= count 0) "done"
      [local [[define low [random-widgets 10000 999999]]
              [define db (make-db widget-quantity < =
                                  [db-bst [avl-insert-multiple low DB-List]])]]
        [begin
          [time [map [λ [wid] [find [(db-field db) wid] db]] low]]
          (time-find-avl (sub1 count))]])]








[define [avl-insert-multiple low db]
  [cond [[empty? low] db]
        [else 
         [avl-insert-multiple [rest low]
                              [insert-avl [first low]
                                          [make-db (db-field db) (db-lt? db)
                                                   (db-eq? db)
                                                   [db-bst db]]]]]]]









(define (height b)
  (local
    
    
    [(define (height-helper b d)
       (cond
         [(false? b) d]
         [else
          (max d
               (height-helper (bst-left b) (add1 d))
               (height-helper (bst-right b) (add1 d)))]))]
    (height-helper b 0)))
 




(define (height-diff b)
  (if (false? b)
      0
      (- (height (bst-left b))
         (height (bst-right b)))))








(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))





(define (random-widgets-string num slen nmax)
  (local
    [(define (random-string len)
       (list->string (build-list len (λ(dummy)
                                       (integer->char (+ 97 (random 26)))))))]
    (build-list num
                (λ(dummy)
                  (make-widget
                   (random-string slen) 
                   (random nmax)
                   (random nmax))))))





(define (render b)
  (local
    [
     (define TEXT-SIZE 20)    
     (define TEXT-COLOR1 "pink")
     (define TEXT-COLOR2 "orange")
     (define TAB 8)
     
     
     (define (blanks n)
       (list->string (build-list n (λ(x) #\ ))))
     
     
     (define (get-color d)
       (if (= (modulo d 2) 0)
           TEXT-COLOR1
           TEXT-COLOR2))
     
     
     (define (to-text side w d)
       (text  (string-append (blanks (* d TAB)) side
                             [number->string (widget-quantity w)])
              
              TEXT-SIZE
              (get-color d)))
     
     
     (define (render-helper b d img side)
       (if (false? b)
           img
           (above/align "left"
                        (to-text side (bst-widget b) d)
                        (render-helper (bst-left b) (+ d 1) img "L: ")
                        (render-helper (bst-right b) (+ d 1) img "R: "))))]
    (render-helper b 0 empty-image "T: ")))