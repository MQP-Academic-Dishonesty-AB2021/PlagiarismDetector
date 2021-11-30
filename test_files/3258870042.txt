

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment-5-part-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define-struct widget (name quantity price))




(define-struct bst (widget left right))




(define-struct db (field lt? eq? bst))



(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 53 2))

(define BST1 (make-bst A1 false false))
(define BST2 (make-bst Z1 false false))
(define BST3 (make-bst W1 false BST2))
(define BST4 (make-bst D1 BST1 BST3))

(define DB-quantity (make-db widget-quantity < = BST4))
(define DB-name (make-db widget-name string<? string=? BST4))
(define DB-name2 (make-db widget-name string<? string=? false))
(define DB-name3 (make-db widget-name string<? string=? BST1))
(define DB-price (make-db widget-price < = BST4))






(check-expect (find "W1" DB-name) W1)
(check-expect (find "B1" DB-name) false)
(check-expect (find 2 DB-quantity) A1)
(check-expect (find "D1" DB-name2) false)
(check-expect (find 16 DB-price) Z1)

(define  (find key db)
  (local [(define (find-in-bst bst)
            (cond
              [(false? bst) false]
              [((db-eq? db) key ((db-field db) (bst-widget bst)))
               (bst-widget bst)]
              [((db-lt? db) key ((db-field db) (bst-widget bst)))
               (find-in-bst (bst-left bst))]
              [else (find-in-bst (bst-right bst))]))]
    (find-in-bst (db-bst db))))





(check-expect (db-bst (insert Z1 DB-name3)) 
              (make-bst A1
                        false
                        (make-bst Z1 false false)))
(check-expect (db-bst (insert B1 DB-name)) 
              (make-bst D1
                        (make-bst A1 false (make-bst B1 false false))
                        BST3))
(check-expect (db-bst (insert B1 DB-price))
              (make-bst D1
                        (make-bst A1 (make-bst B1 false false) false)
                        BST3))

(define (insert widget db)
  (local [(define (insert-in-bst bst)
            (cond
              [(false? bst)
               (make-bst widget false false)]
              [((db-lt? db) ((db-field db) widget)
                            ((db-field db) (bst-widget bst)))
               (make-bst (bst-widget bst)
                         (insert-in-bst (bst-left bst))
                         (bst-right bst))]
              [else
               (make-bst (bst-widget bst)
                         (bst-left bst)
                         (insert-in-bst (bst-right bst)))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-in-bst (db-bst db)))))