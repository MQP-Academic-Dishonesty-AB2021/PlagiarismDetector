

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))






(define-struct db (field lt? eq? bst))
(define DB1 (make-db widget-name string<? string=? (make-bst W1 false false)))
(define DB2 (make-db widget-name string<? string=? false))
(define DB3 (make-db widget-quantity < = (make-bst D1 (make-bst A1 false false)
                                                   (make-bst Z1 false false))))
(define DB4 (make-db widget-price < = (make-bst Z1 (make-bst D1
                                                             (make-bst A1
                                                                       (make-bst W1 false false)
                                                                       false) false) false)))




(check-expect (find "D1" DB1) false) 
(check-expect (find "W1" DB1) W1) 
(check-expect (find 3 DB3) false) 
(check-expect (find 3 DB4) A1) 
(check-expect (find "W1" DB2) false) 

(define (find x db)
  (local [(define (find-bst bst)
   (cond [(false? bst) false]
         [((db-eq? db) x ((db-field db) (bst-widget bst))) (bst-widget bst)] 
         [((db-lt? db) x ((db-field db) (bst-widget bst))) (find-bst (bst-left bst))]
         [else (find-bst (bst-right bst))]))]
    (find-bst (db-bst db))))



(check-expect (db-bst (insert W1 DB2)) (make-bst W1 false false))

(check-expect (db-bst (insert A1 DB1)) (make-bst W1 (make-bst A1 false false) false))

(check-expect (db-bst (insert W1 DB3)) (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
                                                 (make-bst Z1 false false)))

(check-expect (db-bst (insert Z1 DB1)) (make-bst W1 false (make-bst Z1 false false)))

(define (insert widget db)
  (local [(define (insert-bst bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
                   (make-bst (bst-widget bst) (insert-bst (bst-left bst)) (bst-right bst))]
                  [else (make-bst (bst-widget bst) (bst-left bst) (insert-bst (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-bst (db-bst db)))))