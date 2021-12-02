

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))



(define-struct bst (widget left right))









(define-struct db (field lt? eq? bst))

(Natural/String Natural/String -> Boolean false/bst)

(define DB-name (make-db widget-name string<? string=? (make-bst W1 (make-bst A1 false false)
                                                                 (make-bst Z1 false false))))
(define DB-quantity (make-db widget-quantity < =
                             (make-bst W1 false (make-bst A1 false
                                                          (make-bst Z1 false false)))))
(define DB-quantityf (make-db widget-quantity < = false))






(check-expect (find 51 DB-quantity) (make-widget "Z1" 51 16))
(check-expect (find 51 DB-quantityf) false)
(check-expect (find "W1" DB-name) (make-widget "W1" 1 1))
(check-expect (find "A1" DB-name) (make-widget "A1" 2 3))
 
(define (find X DB)
  (local [(define (db->widgetchar db)
            ((db-field db) (bst-widget (db-bst db))))]
    (cond [(false? (db-bst DB)) false]
          [((db-eq? DB) X (db->widgetchar DB))
           (bst-widget (db-bst DB))]
          [((db-lt? DB) X (db->widgetchar DB))
           (find X (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (bst-left (db-bst DB))))]
          [else
           (find X (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (bst-right (db-bst DB))))])))
         






(check-expect (db-bst (insert D1 DB-name))
              (make-bst W1 (make-bst A1 false (make-bst D1 false false))
                        (make-bst Z1 false false)))
(check-expect (db-bst (insert D1 DB-quantity))
              (make-bst W1 false (make-bst A1 false (make-bst Z1
                                                              (make-bst D1 false false) false))))
(check-expect (db-bst (insert D1 DB-name))
              (make-bst W1 (make-bst A1 false (make-bst D1 false false))
                        (make-bst Z1 false false)))

          
(define (insert w DB)
  (local [(define (insert w bst)
            (cond [(false? bst)
                   (make-bst w false false)]
                  [((db-lt? DB) ((db-field DB) w) ((db-field DB) (bst-widget bst)))
                   (make-bst (bst-widget bst) (insert w (bst-left bst)) (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst) (bst-left bst) (insert w (bst-right bst)))]))]
    (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (insert w (db-bst DB)))))
