

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define-struct widget (name quantity price))





(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 1 1010101))
(define C1 (make-widget "C1" 10 50))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 9 20))
(define V1 (make-widget "V1" 100 100))
(define W1 (make-widget "W1" 1 1))
(define X1 (make-widget "X1" 200 70))
(define Y1 (make-widget "Y1" 52 60))
(define Z1 (make-widget "Z1" 51 16))
(define free-stuff (make-widget "free stuff" 0 0))


(define-struct bst (widget left right))






(define EMPTY false)

(define NO-CHILDREN (make-bst D1 false false))

(define BST1 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst Z1
                                 (make-bst W1 false false)
                                 false)))

(define BST2 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst Z1 false false)))

(define LEFT (make-bst Z1
                       (make-bst W1
                                 (make-bst D1
                                           (make-bst A1 false false)
                                           false)
                                 false)
                       false))

(define RIGHT (make-bst A1
                        false
                        (make-bst D1
                                  false
                                  (make-bst W1
                                            false
                                            (make-bst Z1 false false)))))


(define BST1Q (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1 false false)
                                  false)))

(define RIGHTQ (make-bst W1
                         false
                         (make-bst A1
                                   false
                                   (make-bst D1
                                             false
                                             (make-bst Z1 false false)))))


(define BST1P (make-bst A1
                        (make-bst W1 false false)
                        (make-bst V1
                                  (make-bst Y1 false false)
                                  false)))

(define LEFTP (make-bst V1
                        (make-bst Y1
                                  (make-bst D1
                                            (make-bst W1 false false)
                                            false)
                                  false)
                        false))














(define-struct db (field lt? eq? bst))


(define DB1 (make-db widget-name string<? string=? false)) 
(define DB2 (make-db widget-quantity < = BST1Q)) 
(define DB3 (make-db widget-price < = BST1P)) 
(define DB4 (make-db widget-name string<? string=? BST1)) 









(check-expect (find "lmao" DB1) false) 
(check-expect (find "hi" DB4) false) 
(check-expect (find "A1" DB4) A1) 
(check-expect (find "Z1" DB4) Z1) 
(check-expect (find "W1" DB4) W1) 


(check-expect (find 5467654 (make-db widget-quantity < = EMPTY)) false) 
(check-expect (find 8271374 DB2) false) 
(check-expect (find 1 DB2) W1) 
(check-expect (find 51 DB2) Z1) 
(check-expect (find 5 DB2) D1) 


(check-expect (find -928374 (make-db widget-price < = EMPTY)) false) 
(check-expect (find 0 DB3) false) 
(check-expect (find 1 DB3) W1) 
(check-expect (find 100 DB3) V1) 
(check-expect (find 60 DB3) Y1) 


(define (find key db)
  (local [(define bst (db-bst db))
          (define field (db-field db))]
    
    (cond [(false? bst) false]
          [else (cond [((db-eq? db) key (field (bst-widget bst))) (bst-widget bst)]

                      
                      [((db-lt? db) key (field (bst-widget bst)))
                       (find key (make-db field
                                          (db-lt? db)
                                          (db-eq? db)
                                          (bst-left bst)))]

                      
                      [else (find key (make-db field
                                               (db-lt? db)
                                               (db-eq? db)
                                               (bst-right bst)))])])))








(check-expect (db-bst 
               (insert D1 (make-db widget-name string<? string=? EMPTY))) NO-CHILDREN)

(check-expect (db-bst 
               (insert A1 (make-db widget-name string<? string=? NO-CHILDREN))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert W1 (make-db widget-name string<? string=? NO-CHILDREN)))
              (make-bst D1 false (make-bst W1 false false)))

(check-expect (db-bst 
               (insert V1 (make-db widget-name string<? string=? BST1)))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Z1
                                  (make-bst W1
                                            (make-bst V1 false false)
                                            false)
                                  false)))

(check-expect (db-bst 
               (insert Y1 (make-db widget-name string<? string=? BST1)))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Z1
                                  (make-bst W1
                                            false
                                            (make-bst Y1 false false))
                                  false)))


(check-expect (db-bst 
               (insert D1 (make-db widget-quantity < = EMPTY))) NO-CHILDREN)

(check-expect (db-bst 
               (insert A1 (make-db widget-quantity < = NO-CHILDREN))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert Z1 (make-db widget-quantity < = NO-CHILDREN)))
              (make-bst D1 false (make-bst Z1 false false)))

(check-expect (db-bst 
               (insert free-stuff (make-db widget-quantity < = BST1Q)))
              (make-bst A1
                        (make-bst W1
                                  (make-bst free-stuff false false)
                                  false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            false)
                                  false)))

(check-expect (db-bst 
               (insert V1 (make-db widget-quantity < = BST1Q)))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            false)
                                  (make-bst V1 false false))))


(check-expect (db-bst 
               (insert D1 (make-db widget-price < = EMPTY))) NO-CHILDREN)

(check-expect (db-bst 
               (insert A1 (make-db widget-price < = NO-CHILDREN))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert Y1 (make-db widget-price < = NO-CHILDREN)))
              (make-bst D1 false (make-bst Y1 false false)))

(check-expect (db-bst 
               (insert Z1 (make-db widget-price < = BST1P)))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst V1
                                  (make-bst Y1
                                            (make-bst Z1 false false)
                                            false)
                                  false)))

(check-expect (db-bst 
               (insert X1 (make-db widget-price < = BST1P)))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst V1
                                  (make-bst Y1
                                            false
                                            (make-bst X1 false false))
                                  false)))


(define (insert widget db)
  (local [(define (make-new-bst bst)
            (cond 
              [(false? bst) (make-bst widget false false)] 
              [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
               (make-bst (bst-widget bst) (make-new-bst (bst-left bst)) (bst-right bst))]
              [else
               (make-bst (bst-widget bst) (bst-left bst) (make-new-bst (bst-right bst)))]))]
    
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (make-new-bst (db-bst db)))))