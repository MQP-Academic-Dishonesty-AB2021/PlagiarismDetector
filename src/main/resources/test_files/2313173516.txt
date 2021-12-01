

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment5-Part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))




(define-struct bst (widget left right))






(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 0 6))
(define Z2 (make-widget "Z2" 10 15))

(define BST-D1 (make-bst D1 (make-bst B1 (make-bst A1 false false)
                                      (make-bst Z1 false false)) false))
(define BST-A1 (make-bst A1 false false))
(define BST-B1 (make-bst B1 false false))
(define BST-D1s (make-bst D1 (make-bst A1 false false) (make-bst W1 false false)))
(define BST-D1c (make-bst D1 (make-bst A1 false false) (make-bst W1 false
                                                                 (make-bst Z1 false false))))
(define BST-Z2-QUANTITY (make-bst Z2 (make-bst D1 (make-bst A1 false false) false)
                                  (make-bst Z1 false false)))
(define BST-B1-PRICE (make-bst B1 (make-bst A1 false (make-bst D1 false false))
                               (make-bst Z2 false (make-bst Z1 false false))))
(define BST-A1-PRICE-REVERSE (make-bst A1 (make-bst D1 (make-bst Z2 (make-bst Z1 false false)
                                                                 (make-bst B1 false false)) false)
                                       (make-bst W1 false false)))



(define-struct db (field lt? eq? bst))



(check-expect (find "A1" (make-db widget-name string<? string=? false)) false)

(check-expect (find "A1" (make-db widget-name string<? string=? BST-A1)) A1) 

(check-expect (find 2 (make-db widget-quantity < = BST-Z2-QUANTITY)) A1)

(check-expect (find 3 (make-db widget-price < = BST-B1-PRICE)) A1)

(check-expect (find 6 (make-db widget-price > = BST-A1-PRICE-REVERSE)) B1)




(define (find k db)
  (local[(define (find-bst bst)
           (cond[(false? (db-bst db)) false]
                [((db-eq? db) k ((db-field db) (bst-widget bst))) (bst-widget bst)]
                [((db-lt? db) k ((db-field db) (bst-widget bst))) (find-bst  (bst-left bst))]
                [else (find-bst (bst-right bst))]))]
    (find-bst (db-bst db))))



(check-expect (db-bst (insert A1 (make-db widget-name string<? string=? false))) BST-A1)

(check-expect (db-bst (insert B1 (make-db widget-name string<? string=? BST-A1)))
              (make-bst A1 false BST-B1))

(check-expect (db-bst (insert A1 (make-db widget-quantity < =
                                          (make-bst Z2 (make-bst D1 false false)
                                                    (make-bst Z1 false false))))) BST-Z2-QUANTITY)

(check-expect (db-bst (insert Z1 (make-db widget-price < =
                                          (make-bst B1 (make-bst A1 false (make-bst D1 false false))
                                                    (make-bst Z2 false false))))) BST-B1-PRICE)



(check-expect
 (db-bst (insert B1 (make-db widget-price > =
                             (make-bst A1 (make-bst D1 (make-bst
                                                        Z2 (make-bst Z1 false false) false) false)
                                       (make-bst W1 false false)))))
 BST-A1-PRICE-REVERSE)




(define (insert widget db)
  (local [
          (define (insert-bst bst)
            (cond
              [(false? bst) (make-bst widget false false)]
              [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
               (make-bst (bst-widget bst) (insert-bst (bst-left bst)) (bst-right bst))]
              [else (make-bst (bst-widget bst) (bst-left bst) (insert-bst (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-bst (db-bst db)))))

