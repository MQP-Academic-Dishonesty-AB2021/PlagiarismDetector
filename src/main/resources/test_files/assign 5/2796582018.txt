

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(define-struct widget (name quantity price))




(define W1 (make-widget "W1" 1 1))
(define Y1 (make-widget "Y1" 60 2))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 2 9))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))




(define BST1 (make-bst D1 (make-bst A1 false false)
                       (make-bst Z1 false false)))
(define BST2 (make-bst D1 (make-bst A1 false false)
                       (make-bst Z1 (make-bst W1 false false) false)))

(define-struct db (field lt? eq? bst))




(define DB-quantity (make-db widget-quantity < = BST1))

(define DB-name (make-db widget-name string<? string=? BST1))

(define DB-price (make-db widget-price < = BST1))







(check-expect
 (find "D1" DB-name) D1)  
(check-expect
 (find "D1" DB-name) (make-widget "D1" 5 5)) 
(check-expect
 (find "W1" DB-name) false) 
(check-expect
 (find "Z1" DB-name) Z1) 

(check-expect (find 12 DB-quantity) false) 
(check-expect (find 5 DB-quantity) D1)     
(check-expect (find 51 DB-quantity) Z1)    
(check-expect (find 2 DB-quantity) A1)     

(define (find x db)
  (local [(define (fn-for-wid x db)
            (cond
              [(false? (db-bst db)) false]
              
              [((db-eq? db) x ((db-field db) (bst-widget (db-bst db))))
               (bst-widget (db-bst db))]
              
              [((db-lt? db) x ((db-field db) (bst-widget (db-bst db))))
               (fn-for-wid x (make-db (db-field db)
                                      (db-lt? db) (db-eq? db)
                                      (bst-left (db-bst db))))]
              
              [(not ((db-lt? db) x ((db-field db)(bst-widget (db-bst db)))))
               (fn-for-wid x (make-db (db-field db)
                                      (db-lt? db) (db-eq? db)
                                      (bst-right (db-bst db))))]
              
              [else (error "How did we get here?")]))]

    (fn-for-wid x db)))






(check-expect                               
 (db-bst (insert Y1 DB-quantity))
 (make-bst D1 (make-bst A1 false false)
           (make-bst Z1 false (make-bst Y1 false false))))

(check-expect                               
 (db-bst (insert W1 DB-quantity))
 (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
           (make-bst Z1 false false)))

(check-expect                               
 (db-bst (insert D1 DB-name)) BST1)

(check-expect                               
 (db-bst (insert B1 DB-name))
 (make-bst D1 (make-bst A1 false (make-bst B1 false false))
           (make-bst Z1 false false)))


(check-expect (db-bst (insert W1 DB-name))  
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 (make-bst W1 false false) false )))


(check-expect (db-bst (insert B1 DB-price))
              (make-bst D1 (make-bst A1 false false)
                       (make-bst Z1 (make-bst B1 false false) false)))

(define (insert x db)
  (local [
          (define (bst-insert x db bst)
            (cond
              [(false? bst) (make-bst x false false)] 
    
              [((db-eq? db) ((db-field db) x)
                            ((db-field db) (bst-widget bst)))
               bst]  
   

              [((db-lt? db) ((db-field db) x)
                            ((db-field db) (bst-widget bst))
                            )
               (make-bst
                (bst-widget bst)
                (bst-insert x db (bst-left bst))
                (bst-right bst))]
    
              [else 
               (make-bst
                (bst-widget bst)
                (bst-left bst)
                (bst-insert x db (bst-right bst)))]))]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (bst-insert x db (db-bst db)))
    ))
