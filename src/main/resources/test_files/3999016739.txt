

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(define-struct widget (name quantity price))




(define-struct bst (widget left right))











(define-struct db (field less-than? eq? bst))

 


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define P1 (make-widget "P1" 37 10))
(define R1 (make-widget "R1" 4 9))
(define B1 (make-widget "B1" 7 7))

(define BST-quantity (make-bst D1
                               (make-bst A1
                                         (make-bst W1 false false)
                                         false)
                               (make-bst P1
                                         (make-bst B1 false false)
                                         (make-bst Z1 false false))))
(define BST-name (make-bst R1
                           (make-bst B1
                                     (make-bst A1 false false)
                                     (make-bst D1 false false))
                           (make-bst W1
                                     false
                                     (make-bst Z1 false false))))
(define BST-price (make-bst D1
                            (make-bst A1
                                      (make-bst W1 false false)
                                      false)
                            (make-bst R1
                                      false
                                      (make-bst P1
                                                false
                                                (make-bst Z1 false false)))))

(define DB-quantity (make-db widget-quantity < = BST-quantity))
(define DB-name (make-db widget-name string<? string=? BST-name))
(define DB-price (make-db widget-price < = BST-price))
(define DB-empty (make-db widget-quantity < = false))





(check-expect (find 2 DB-quantity) A1)

(check-expect (find 10 DB-price) P1)

(check-expect (find "P1" DB-name) false)

(check-expect (find 7 DB-price) false)

(check-expect (find 7 DB-empty) false)

(define (find value db)
  (local
    [(define (find-inner value bst)
       (cond [(false? bst) false]
             [((db-eq? db) value ((db-field db) (bst-widget bst))) (bst-widget bst)]
             [((db-less-than? db) value ((db-field db) (bst-widget bst)))
              (find-inner value (bst-left bst))]
             [else
              (find-inner value (bst-right bst))]))]
    (find-inner value (db-bst db))))






(check-expect (db-bst (insert R1 DB-quantity))
              (make-bst D1 
                        (make-bst A1 (make-bst W1 false false) (make-bst R1 false false)) 
                        (make-bst P1 (make-bst B1 false false) (make-bst Z1 false false))))



(check-expect (db-bst (insert B1 DB-price))
              (make-bst D1 
                        (make-bst A1 (make-bst W1 false false) false) 
                        (make-bst R1 
                                  (make-bst B1 false false) 
                                  (make-bst P1 false (make-bst Z1 false false)))))

(check-expect (db-bst (insert P1 DB-name))
              (make-bst R1 
                        (make-bst B1 (make-bst A1 false false) 
                           (make-bst D1 false (make-bst P1 false false))) 
                        (make-bst W1 false (make-bst Z1 false false))))

(check-expect (db-bst (insert B1 (make-db widget-price < = false))) 
              (make-bst B1 false false))

(define (insert widget db)
  (local [(define (insert-inner widget bst)
            (cond [(false? bst) 
                   (make-bst widget false false)]
                  [((db-less-than? db)
                    ((db-field db) widget)
                    ((db-field db) (bst-widget bst)))
                   (make-bst
                    (bst-widget bst)
                    (insert-inner widget (bst-left bst))
                    (bst-right bst))]
                  [else 
                   (make-bst
                    (bst-widget bst)
                    (bst-left bst)
                    (insert-inner widget (bst-right bst)))]))]         
    (make-db (db-field db) (db-less-than? db) (db-eq? db) (insert-inner widget (db-bst db)))))

