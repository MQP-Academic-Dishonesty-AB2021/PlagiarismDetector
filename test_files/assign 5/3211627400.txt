

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part2_DanielB_BenjaminS final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define P1 (make-widget "P1" 0 0))

(define-struct bst (widget left right))








(define-struct db (field lt? eq? bst))

(define db-name (make-db widget-name string<? string=?
                         (make-bst D1 (make-bst A1 false false)
                                   (make-bst W1 false (make-bst Z1 false false)))))
(define db-quantity (make-db widget-quantity < =
                             (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
                                        (make-bst Z1 false false))))
(define db-price (make-db widget-price > =
                          (make-bst D1 
                                    false
                                    (make-bst A1 false (make-bst W1 false false)))))

(define (find widget db)
  (cond
    [(false? (db-bst db)) false]
    [((db-eq? db) ((db-field db) widget) ((db-field db) (bst-widget (db-bst db)))) widget]
    [else (if (not (false? (find widget (make-db (db-field db) (db-lt? db)
                                                 (db-eq? db) (bst-left (db-bst db))))))
              widget
              (find widget (make-db (db-field db) (db-lt? db) (db-eq? db)
                                    (bst-right (db-bst db)))))]
    )
  )
(check-expect (find D1 db-name) D1)
(check-expect (find P1 db-name) false)
(check-expect (find Z1 db-name) Z1)
(check-expect (find Z1 db-price) false)
(check-expect (find A1 db-quantity) A1)


(define (insert widget db) 
  (local [(define (helper widget bst)
            (cond
              [(false? bst) (make-bst widget false false)]
              [else (if ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst))) 
                        (make-bst (bst-widget bst) (helper widget (bst-left bst)) (bst-right bst))
                        (make-bst (bst-widget bst) (bst-left bst) (helper widget (bst-right bst))))]
              )
            )]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (helper widget (db-bst db)))
    ))
(define test-db (make-db widget-quantity < = false))
(check-expect (db-bst (insert Z1 db-price))
                        (make-bst D1 (make-bst Z1 false false)
                                  (make-bst A1 false (make-bst W1 false false))))
(check-expect (db-bst (insert P1 db-name)) (make-bst D1 (make-bst A1 false false)
                                   (make-bst W1 (make-bst P1 false false)
                                             (make-bst Z1 false false))))
(check-expect (db-bst (insert A1 (insert D1 (insert Z1 test-db))))
              (make-bst Z1 (make-bst D1 (make-bst A1 false false) false) false))
