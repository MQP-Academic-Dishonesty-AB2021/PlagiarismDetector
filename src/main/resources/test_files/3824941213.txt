

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require racket/math)
(define-struct widget (name quantity price))




(define-struct bst (widget left right))






(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 6))
(define G1 (make-widget "G1" 14 9))
(define B1 (make-widget "B1" 8 2))



(define BSTname (make-bst D1
                          (make-bst A1
                                    false
                                    false)
                          (make-bst W1
                                    false
                                    (make-bst Z1 false false))))

(define BSTquantity (make-bst D1
                              (make-bst A1
                                        (make-bst W1 false false)
                                        false)
                              (make-bst G1
                                        false
                                        (make-bst Z1 false false))))

(define BSTprice (make-bst D1
                           (make-bst A1
                                     (make-bst W1 false false)
                                     false)
                           (make-bst G1
                                     false
                                     (make-bst Z1 false false))))









(define-struct db (field lt? eq? bst))

(define DBname (make-db widget-name string<=? string=? BSTname))

(define DBquantity (make-db widget-quantity < = BSTquantity))

(define DBprice (make-db widget-price < = BSTprice))

 






(check-expect (find "A1" DBname) A1)
(check-expect (find 51 DBquantity) Z1)
(check-expect (find 16 DBprice) Z1)

(check-expect (find 879257482672 DBprice) false)



(define (find field db)
  (local [
          (define (searchBst field bst)
            (cond
              [(false? bst) false]
              [((db-eq? db) field ((db-field db) (bst-widget bst)))
               (bst-widget bst) 
               ]
              [((db-lt? db) field ((db-field db) (bst-widget bst)))
               (searchBst field (bst-left bst))
               ]
              [else
               (searchBst field (bst-right bst))
               ]
              )
            )
          ]
    (searchBst field (db-bst db))
    )

  )




(check-expect (db-bst (insert B1 DBname))
              (make-bst D1
                          (make-bst A1
                                    false
                                    (make-bst B1
                                    false
                                    false))
                          (make-bst W1
                                    false
                                    (make-bst Z1 false false))))
(check-expect (db-bst (insert B1 DBquantity))
              (make-bst D1
                              (make-bst A1
                                        (make-bst W1 false false)
                                        false)
                              (make-bst G1
                                        (make-bst B1
                                    false
                                    false)
                                        (make-bst Z1 false false))))
(check-expect (db-bst (insert B1 DBprice))
              (make-bst D1
                           (make-bst A1
                                     (make-bst W1 false (make-bst B1 false false))
                                     false)
                           (make-bst G1
                                     false
                                     (make-bst Z1 false false))))


(define (insert widget db)
  (local
    [
     (define (insertBst widget bst)
       (cond [(false? bst) (make-bst widget false false)]
             [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
              (make-bst (bst-widget bst)
                        (insertBst widget (bst-left bst))
                        (bst-right bst)
                        )
              ]
             [(false? ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst))))
              (make-bst (bst-widget bst)
                        (bst-left bst)
                        (insertBst widget (bst-right bst))
                        )
              ]
             )
       )
     ]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (insertBst widget (db-bst db))
     )
    )
  )