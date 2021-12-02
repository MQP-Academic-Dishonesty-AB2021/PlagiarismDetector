

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))




(require 2htdp/image)

(define-struct widget (name quantity price))


(define W1 (make-widget "W1" 1 1))
(define W1.1 (make-widget "W1" 0 .5))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))



(define W1-NODE (make-bst W1 false false))
(define W1-BST (make-bst W1 false (make-bst W1.1 false false)))
(define W1-BST-FLIP (make-bst W1.1 false (make-bst W1 false false)))
(define Z1-NODE (make-bst Z1 false false))
(define ALPHABET (make-bst W1 
                           (make-bst A1 false (make-bst D1 false false))
                           (make-bst Z1 false false)))
(define QUANTABET (make-bst A1 
                           (make-bst W1 false false)
                           (make-bst D1 false (make-bst Z1 false false))))








(define-struct db (field lt? eq? bst))

(define DB-NAME (make-db widget-name string<? string=? false))
(define DB-NAME-2 (make-db widget-name string<? string=? ALPHABET))
(define DB-NAME-W0 (make-db widget-name string<? string=? W1-NODE))
(define DB-NAME-W1 (make-db widget-name string<? string=? W1-BST))
(define DB-NAME-W1-FLIP (make-db widget-name string<? string=? W1-BST-FLIP))
(define DB-QUANTITY (make-db widget-quantity < = false))
(define DB-QUANTITY-2 (make-db widget-quantity < = QUANTABET))







(check-expect (find "" DB-NAME) false) 
(check-expect (find "" DB-NAME-2) false) 
(check-expect (find "W1" DB-NAME-2) W1) 
(check-expect (find 51 DB-QUANTITY-2) Z1) 
(check-expect (find "W1" DB-NAME-W1) W1) 
(check-expect (find "W1" DB-NAME-W1-FLIP) W1.1) 
(check-expect (find 0 DB-QUANTITY-2) false) 

(define (find x db) 
  (local [
          
          
          (define (left? x db)
            ((db-lt? db) x ((db-field db) (bst-widget (db-bst db)))))
          
          
          
          (define (dbequal? x db)
            ((db-eq? db) ((db-field db) (bst-widget (db-bst db))) x))]
         (cond [(false? (db-bst db)) false]
               [(dbequal? x db) (bst-widget (db-bst db))]
               [(left? x db) (find x (make-db
                                      (db-field db)
                                      (db-lt? db)
                                      (db-eq? db)
                                      (bst-left (db-bst db))))]
               [else 
                (find x (make-db 
                              (db-field db)
                              (db-lt? db)
                              (db-eq? db)
                              (bst-right (db-bst db))))])))
                        
               

               




(check-expect (db-bst (insert W1.1 DB-NAME-W0)) W1-BST)

(check-expect (db-bst (insert W1 DB-NAME))
                       (make-bst W1 false false)) 
(check-expect (db-bst (insert (make-widget "" 0 0) DB-NAME-2)) 
               (make-bst W1 
                (make-bst A1 
                           (make-bst (make-widget "" 0 0) false false)
                           (make-bst D1 false false))
                           (make-bst Z1 false false)))


(define (insert widget db)
  (local [(define (insert-abstract bst)
            (cond [(false? bst) (make-bst widget false false)]
                  
                  [((db-lt? db) ((db-field db) widget)
                                ((db-field db) (bst-widget bst)))
                   
                    (make-bst 
                     (bst-widget bst)
                     (insert-abstract (bst-left bst))
                     (bst-right bst))]
                  [else 
                        
                   (make-bst 
                     (bst-widget bst)
                     (bst-left bst)
                     (insert-abstract (bst-right bst)))]))]
  (make-db (db-field db)
           (db-lt? db)
           (db-eq? db)
           (insert-abstract (db-bst db))))) 