

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab5_part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








(define-struct widget (name quantity price))






(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define E5 (make-widget "E5" 7 20))


(define-struct bst (widget left right))




(define BST-NAME-MAIN (make-bst A1
                                false
                                (make-bst W1
                                          (make-bst D1 false false)
                                          (make-bst Z1 false false))))
(define BST-QUANTITY-MAIN (make-bst A1
                                    (make-bst W1 false false)
                                    (make-bst Z1
                                              (make-bst D1 false false)
                                              false)))
(define BST-PRICE-MAIN (make-bst A1
                                 (make-bst W1 false false)
                                 (make-bst Z1
                                           (make-bst D1 false false)
                                           false)))


(define-struct db (field lt? eq? bst))










(define DB-NAME-FALSE (make-db widget-name string<? string=? false))
(define DB-NAME (make-db widget-name string<? string=? BST-NAME-MAIN))

(define DB-QUANTITY-FALSE (make-db widget-quantity < = false))
(define DB-QUANTITY (make-db widget-quantity < = BST-QUANTITY-MAIN))

(define DB-PRICE-FALSE (make-db widget-price < = false))
(define DB-PRICE (make-db widget-price < = BST-PRICE-MAIN))










(check-expect (find 1 DB-PRICE) W1)


(check-expect (find 78 DB-PRICE) false)


(check-expect (find "D1" DB-NAME) D1)


(check-expect (find "E5" DB-NAME) false)


(check-expect (find 51 DB-QUANTITY) Z1)


(check-expect (find 0 DB-QUANTITY) false)

(define (find param db)
  (local [
          
          
          
          (define (compare? comparison-fn? db param b)
            ((comparison-fn? db) param ((db-field db) (bst-widget b))))

          (define (find b)
            (cond
              [(false? b) false]
              [(compare? db-eq? db param b) (bst-widget b)]
              [(compare? db-lt? db param b) (find (bst-left b))]
              [else
               (find (bst-right b))]))]
    
    (find (db-bst db))))







(check-expect (db-bst (insert E5 DB-NAME))
              (make-bst A1
                        false
                        (make-bst W1
                                  (make-bst D1
                                            false
                                            (make-bst E5 false false))
                                  (make-bst Z1 false false))))


(check-expect (db-bst (insert E5 DB-NAME-FALSE))
              (make-bst E5 false false))


(check-expect (db-bst (insert E5 DB-QUANTITY))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            (make-bst E5 false false))
                                  false)))


(check-expect (db-bst (insert E5 DB-QUANTITY-FALSE))
              (make-bst E5 false false))


(check-expect (db-bst (insert E5 DB-PRICE))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1 false false)
                                  (make-bst E5 false false))))


(check-expect (db-bst (insert E5 DB-PRICE-FALSE))
              (make-bst E5 false false))

(define (insert wid db)
  (local [
          
          
          
          (define (insert-left? db wid b)
            ((db-lt? db) ((db-field db) wid) ((db-field db) (bst-widget b))))

          (define (insert b)
            (cond
              [(false? b) (make-bst wid false false)]
              [(insert-left? db wid b) (make-bst (bst-widget b)
                                                 (insert (bst-left b))
                                                 (bst-right b))]
              [else
               (make-bst (bst-widget b)
                         (bst-left b)
                         (insert (bst-right b)))]))]
    
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert (db-bst db)))))