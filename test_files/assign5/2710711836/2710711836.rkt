

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(define-struct widget (name quantity price))




(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define simple-bst (make-bst W1 false false))
(define test-bst (make-bst D1 (make-bst A1 false false) (make-bst W1 false false)))
(define complex-bst (make-bst W1 (make-bst D1 (make-bst A1 false false) false) 
                                 (make-bst Z1 false false)))

(define DB-quantity1 (make-db widget-quantity < = simple-bst))
(define DB-quantity2 (make-db widget-quantity < = test-bst))
(define DB-name1 (make-db widget-name string<? string=? simple-bst))
(define DB-name2 (make-db widget-name string<? string=? complex-bst))



(define (find val db)
  (local [

    
    (define (find-leaf left?)
      (find val (make-db (db-field db)
          (db-lt? db)
          (db-eq? db)
          (if left? (bst-left (db-bst db)) (bst-right (db-bst db)))
        )
      )
    )
  ]
    (cond
      [(false? (db-bst db)) false]
      [((db-eq? db) val ((db-field db) (bst-widget (db-bst db))))
        (bst-widget (db-bst db))
      ]
      [((db-lt? db) val ((db-field db) (bst-widget (db-bst db))))
        (find-leaf true) 
      ]
      [(not ((db-lt? db) val ((db-field db) (bst-widget (db-bst db)))))
        (find-leaf false) 
      ]
      [else
        false
      ]
    )
  )
)

(check-expect (find 1 DB-quantity1) W1) 
(check-expect (find 3 DB-quantity1) false) 
(check-expect (find 2 DB-quantity2) A1) 
(check-expect (find "Z1" DB-name2) Z1) 



(define (insert widget db)
  (local [
      
    
    (define (new-db-bst left?)
      (db-bst (insert widget (make-db (db-field db)
            (db-lt? db)
            (db-eq? db)
            (if left?
              (bst-left (db-bst db))
              (bst-right (db-bst db))
            )
          )
        )
      )
    )

    
    (define (new-make-db left?)
      (make-db (db-field db)
        (db-lt? db)
        (db-eq? db)
        (make-bst (bst-widget (db-bst db))
          (if left? (new-db-bst true) (bst-left (db-bst db)))
          (if left? (bst-right (db-bst db)) (new-db-bst false))
        )
      )
    )
  ]
    (cond
      [(false? (db-bst db))
        (make-db (db-field db) (db-lt? db) (db-eq? db) (make-bst widget false false))
      ]
      [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db))))
        (new-make-db true) 
      ]
      [(not ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db)))))
        (new-make-db false) 
      ]
    )
  )
)

(check-expect (db-bst (insert Z1 DB-name1)) (make-bst W1 false (make-bst Z1 false false))) 
(check-expect (db-bst (insert D1 DB-quantity1)) (make-bst W1 false (make-bst D1 false false))) 
(check-expect (db-bst (insert Z1 DB-quantity2)) 
              (make-bst D1 (make-bst A1 false false) 
                           (make-bst W1 false (make-bst Z1 false false)))) 