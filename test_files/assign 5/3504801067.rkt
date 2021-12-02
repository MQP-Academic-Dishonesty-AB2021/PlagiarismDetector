

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define L1 (make-widget "LL23" 6 10)) 
(define B1 (make-widget "B" 17 12))   

(define-struct bst (widget left right))





(define BST1 false)
(define BST2 (make-bst Z1 false false))
(define BST3 (make-bst W1 false BST2))
(define BST4 (make-bst D1 false false))
(define BST5 (make-bst L1 BST4 BST3))

(define BST6 (make-bst L1
                       (make-bst D1
                                 (make-bst W1 false false)
                                 false)
                       (make-bst Z1 false false)))

(define-struct db (field lt? eq? bst))












(define DB-quantity (make-db widget-quantity < = false))
(define DB-name (make-db widget-name string<? string=? false))

(define db1 (make-db widget-name string<? equal? BST5))
(define db2 (make-db widget-quantity < = BST6))
(define db3 (make-db widget-quantity < = false))








(check-expect (find "Z1" db1) Z1) 
(check-expect (find "A1" db1) false) 
(check-expect (find 1 db2) W1) 
(check-expect (find 6 db2) L1) 
(check-expect (find 11 db3) false) 

(define (find key db)
  (local
    [(define (find key bst) 
       (cond [(false? bst) false]
             
             [((db-lt? db) key ((db-field db) (bst-widget bst)))
              (find key (bst-left bst))]
             
             [((db-eq? db) key ((db-field db) (bst-widget bst)))
              (bst-widget bst)]
             
             [else
              (find key (bst-right bst))]))]
    
    
    (find key (db-bst db))))










(check-expect (db-bst (insert W1 DB-name)) (make-bst W1 false false)) 
(check-expect (db-bst (insert W1 DB-quantity)) (make-bst W1 false false)) 
(check-expect (db-bst (insert A1 db2)) (make-bst L1
                                                 (make-bst D1
                                                           (make-bst W1 false
                                                                     (make-bst A1 false false))
                                                           false)
                                                 (make-bst Z1 false false))) 

(define (insert widget db)
         
  (local [(define (update-db-bst db bst)
            (make-db (db-field db) (db-lt? db) (db-eq? db) bst))
          
          (define (insert-ins widget bst) 
            (cond [(false? bst) (make-bst widget false false)] 
                  [else
                   (if ((db-lt? db) ((db-field db) widget)
                       ((db-field db) (bst-widget bst))) 
                       
                       (make-bst (bst-widget bst)
                                 (insert-ins widget (bst-left bst))
                                 (bst-right bst))
                       
                       (make-bst (bst-widget bst)
                                 (bst-left bst)
                                 (insert-ins widget (bst-right bst))))]))]
    
    
    (update-db-bst db (insert-ins widget (db-bst db)))))