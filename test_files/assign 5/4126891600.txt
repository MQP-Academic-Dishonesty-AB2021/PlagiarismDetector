

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))





(define-struct bst (widget left right))











(define-struct db (field lt? eq? bst))




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define L1 (make-widget "L1" 7 10))
(define E1 (make-widget "E1" 13 2))
(define C1 (make-widget "C1" 3 5))












(define BST1 (make-bst D1
                       (make-bst A1
                                 #f
                                 (make-bst W1 #f #f))
                       (make-bst Z1 #f #f)))










(define BST2 (make-bst A1
                       #f
                       (make-bst Z1
                                 (make-bst D1
                                           #f
                                           (make-bst W1 #f #f))
                                 #f)))
                                 



(define BST3 (make-bst D1 #f #f))







(define BST4 (make-bst D1
                       (make-bst A1 #f #f)
                       (make-bst W1 #f #f)))







(define BST5 (make-bst L1
                       (make-bst A1 #f #f)
                       (make-bst Z1 #f #f)))
                      




(define DB1 (make-db widget-name string<? string=? BST1))
(define DB2 (make-db widget-name string<? string=? BST3))
(define DB3 (make-db widget-name string<? string=? BST4))

(define DB4 (make-db widget-quantity > = BST2))
(define DB5 (make-db widget-quantity > = BST4))
(define DB6 (make-db widget-quantity > = BST5))

(define DB7 (make-db widget-price < = BST1))
(define DB8 (make-db widget-price < = BST3))
(define DB9 (make-db widget-price < = BST5))










(check-expect (find "A1" DB1) A1)
(check-expect (find "D1" DB2) D1)
(check-expect (find "A1" DB2) false)
(check-expect (find "W1" DB3) W1)
(check-expect (find "A1" DB3) A1)
(check-expect (find "L1" DB3) false)

(check-expect (find 3 DB7) A1)
(check-expect (find 3 DB8) false)
(check-expect (find 5 DB8) D1)
(check-expect (find 2 DB4) A1)


(define (find x db)
  (cond
    [(false? (db-bst db)) false]
    [((db-eq? db)
      x
      ((db-field db) (bst-widget (db-bst db))))
     (bst-widget (db-bst db))]
    [((db-lt? db)
      x
      ((db-field db) (bst-widget (db-bst db))))
     (find x (make-db
              (db-field db) (db-lt? db) (db-eq? db) (bst-left (db-bst db))))]
    [else
     (find x (make-db
              (db-field db) (db-lt? db) (db-eq? db) (bst-right (db-bst db))))]))










(check-expect (db-bst (insert L1 DB3))
              (make-bst D1
                        (make-bst A1 #f #f)
                        (make-bst W1 (make-bst L1 #f #f) #f)))


(check-expect (db-bst (insert C1 DB4))
               (make-bst A1
                         (make-bst C1 #f #f)
                         (make-bst Z1
                                   (make-bst D1
                                             #f
                                             (make-bst W1 #f #f))
                                   #f)))


(check-expect (db-bst (insert E1 DB9))
              (make-bst L1
                        (make-bst A1
                                  (make-bst E1 #f #f)
                                  #f)
                        (make-bst Z1 #f #f)))

(define (insert w db)
  (local
    [(define (insert0 bst)
       (cond [(false? bst) (make-bst w #f #f)]
             [((db-lt? db) ((db-field db) w) ((db-field db) (bst-widget bst)))
              (make-bst (bst-widget bst) (insert0 (bst-left bst)) (bst-right bst))]
             [else
              (make-bst (bst-widget bst) (bst-left bst) (insert0 (bst-right bst)))]))]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (insert0 (db-bst db)))))