

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)

(define-struct widget (name quantity price))



(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))



(define BST1 (make-bst W1 false
                       (make-bst A1 false
                                 (make-bst Z1 false false))))
(define BST2 (make-bst D1 false false))
(define BST3 (make-bst W1 false (make-bst Z1 false false)))
(define BST4 (make-bst Z1 (make-bst A1 false false) false))
(define BST5 (make-bst W1 (make-bst A1 false
                                    (make-bst D1 false false))
                       (make-bst Z1 false false)))
    




(define-struct db (field lt? eq? bst))





(define DB-quantity-false (make-db widget-quantity < = false))
(define DB-quantity (make-db widget-quantity < = BST1))
(define DB-name (make-db widget-name string<? string=? BST4))
(define DB-price (make-db widget-price < = BST5))




(check-expect (find 2 DB-quantity) A1)                   
(check-expect (find 7 DB-quantity) false)                
(check-expect (find 2 DB-quantity-false) false)          
(check-expect (find "A1" DB-name) A1)                    
(check-expect (find 16 DB-price) Z1)                     



(define (find key db)
  (local
    [(define BST-DB (db-bst db))  

     (define lt (db-lt? db))      
     (define eq (db-eq? db))      
     (define field (db-field db)) 

     (define (smaller? key bst0)  
       (lt key (field (bst-widget bst0))))
     
     (define (same? key bst0)     
       (eq key (field (bst-widget bst0))))
    
     (define (find-inner key bst)
       (cond [(false? bst) false]
             [(same? key bst)
              (bst-widget bst)]
             [(smaller? key bst)
              (find-inner key (bst-left bst))]
             [else
              (find-inner key (bst-right bst))]))]

    (find-inner key BST-DB)))



(check-expect (db-bst (insert D1 DB-quantity))          
              (make-bst W1 false (make-bst A1 false
                                  (make-bst Z1 (make-bst D1 false false)
                                            false))))
(check-expect (db-bst (insert A1 DB-quantity-false))    
              (make-bst A1 false false))
(check-expect (db-bst (insert D1 DB-name))              
              (make-bst Z1 (make-bst A1 false
                                     (make-bst D1 false false))
                        false))



(define (insert key db)
  (local
    [(define BST-DB (db-bst db))  

     (define lt (db-lt? db))      
     (define field (db-field db)) 

     (define (smaller? key bst0)  
       (lt (field key) (field (bst-widget bst0))))

     (define (create-db bst)      
       (make-db (db-field db)
                (db-lt? db)
                (db-eq? db)
                bst))
    
     (define (insert-inner key bst)
       (cond [(false? bst)                 
              (make-bst key false false)]
             [(smaller? key bst)           
              (make-bst (bst-widget bst)
                        (insert-inner key (bst-left bst))
                        (bst-right bst))]
             [else                         
              (make-bst (bst-widget bst)
                        (bst-left bst)
                        (insert-inner key (bst-right bst)))]))]

    (create-db (insert-inner key BST-DB))))