

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname VenatArjun_AdlerBernhardt_Assignment5Part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))




(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))








(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define B1 (make-widget "B1" 5 2))
(define V1 (make-widget "V1" 9 4))
(define I1 (make-widget "I1" 2 5))

(define BST1name (make-bst D1 (make-bst A1 false false)
                           (make-bst Z1 (make-bst W1 false false) false)))
(define BST2name (make-bst W1 (make-bst A1 false
                                        (make-bst D1 false false))
                           (make-bst Z1 false false)))
(define BST1price (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
                               (make-bst Z1 false false)))                            
(define BST1quantity (make-bst A1 (make-bst W1 false false)
                               (make-bst Z1
                                         (make-bst D1 false false)
                                         false)))
(define BST2quantity (make-bst Z1 (make-bst A1
                                            (make-bst W1 false false)
                                            (make-bst D1 false false))
                               false
                               ))

(define DB-quantity1 (make-db widget-quantity < = BST1quantity))
(define DB-quantity2 (make-db widget-quantity < = BST2quantity))
(define DB-name1 (make-db widget-name string<? string=? BST1name))
(define DB-name2 (make-db widget-name string<? string=? BST2name))
(define DB-price1 (make-db widget-price < = BST1price))




(check-expect (find "D1" DB-name1) D1) 
(check-expect (find "A1" DB-name2) A1) 
(check-expect (find 51 DB-quantity1) Z1) 
(check-expect (find 9 DB-quantity2) false) 


(define (find x db)
  (local [
          (define (smaller? k b)
            ((db-lt? db) k ((db-field db) (bst-widget b))))

          (define (same? k b)
            ((db-eq? db) k ((db-field db) (bst-widget b))))
          

          (define (get-bst k b)
            (cond
              [(false? b) false]
              [(smaller? k b) (get-bst k (bst-left b))]
              [(same? k b) (bst-widget b)]
              [else
               (get-bst k (bst-right b))]
              ))
          ]

    (get-bst x (db-bst db))
    )
  )





(check-expect (db-bst (insert V1 DB-quantity2))
              (make-bst Z1 (make-bst A1
                                     (make-bst W1 false false)
                                     (make-bst D1 false (make-bst V1 false false)))
                           false
                        )) 

(check-expect (db-bst (insert B1 DB-name2))
              (make-bst W1 (make-bst A1 false
                                        (make-bst D1 (make-bst B1 false false) false))
                           (make-bst Z1 false false))) 

 
(check-expect (db-bst (insert I1 DB-name2))
              (make-bst W1 (make-bst A1 false
                                      (make-bst D1 false (make-bst I1 false false)))
                          (make-bst Z1 false false))) 


(check-expect (db-bst (insert B1 DB-price1))
              (make-bst D1 (make-bst A1 (make-bst W1 false (make-bst B1 false false)) false)
                           (make-bst Z1 false false))) 
               

(define (insert w db)
  (local [(define (smaller? k b)
            ((db-lt? db) k ((db-field db) (bst-widget b))))

          
          

          

          
          (define (get-bst v b)
            (if (false? b)
                (make-bst v false false)
                (if (smaller? ((db-field db) v) b)
                    (make-bst (bst-widget b) (get-bst v (bst-left b)) (bst-right b))
                    (make-bst (bst-widget b) (bst-left b) (get-bst v (bst-right b))))
                ))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (get-bst w (db-bst db)))
    ))



