

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(define-struct widget (name quantity price))








(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))





(define DB-quantity (make-db widget-quantity < = false))






(define DB-name-blank (make-db widget-name string<? string=? false))




(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 100 100))
(define C1 (make-widget "C1" 999 999))
(define D1 (make-widget "D1" 5 5))
(define W1 (make-widget "W1" 1 1))
(define X1 (make-widget "X1" 420 420))
(define Z1 (make-widget "Z1" 51 16))
(define E1 (make-widget "E1" 6 7))

(define BST7 (make-bst B1 false false))
(define BST4 (make-bst A1 false BST7))
(define BST5 (make-bst D1 false false))
(define BST2 (make-bst C1 BST4 BST5))
(define BST6 (make-bst X1 false false))
(define BST3 (make-bst Z1 BST6 false))
(define BST1 (make-bst W1 BST2 BST3)) 

(define DB-name (make-db widget-name string<? string=? BST1))



(define (find key db)
  (local [(define lt-func (db-lt? db))
          (define eq-func (db-eq? db))
          (define field-func (db-field db))
          (define (find-internal bst)
            (cond
              [(false? bst) false]
              [(eq-func key
                            (field-func (bst-widget bst)))
               (bst-widget bst)]
              [(lt-func key
                            (field-func (bst-widget bst)))
               (find-internal (bst-left bst))]
              [else
               (find-internal (bst-right bst))]))]
    (find-internal (db-bst db))))

(check-expect (find "C1" DB-name) C1) 
(check-expect (find "W1" DB-name) W1) 
(check-expect (find "Z1" DB-name) Z1) 
(check-expect (find "Q99" DB-name) false) 

(check-expect (db-bst (insert E1 (db--set-bst DB-name BST3)))
              (make-bst Z1
                        (make-bst X1
                                  (make-bst E1 false false)
                                  false)
                        false)) 
(check-expect (db-bst (insert E1 (db--set-bst DB-name BST7)))
              (make-bst B1
                        false
                        (make-bst E1 false false))) 
(check-expect (db-bst (insert E1 (db--set-bst DB-name BST4)))
              (make-bst A1
                        false
                        (make-bst B1
                                  false
                                  (make-bst E1
                                            false
                                            false)))) 
(check-expect (db-bst (insert E1 (db--set-bst DB-name BST2)))
              (make-bst C1
                        BST4
                        (make-bst D1
                                  false
                                  (make-bst E1
                                            false
                                            false)))) 
(check-expect (db-bst (insert D1 (db--set-bst DB-name BST2)))
              BST2) 



(define (insert widget db)
  (local [(define lt-func (db-lt? db))
          (define eq-func (db-eq? db))
          (define field-func (db-field db))
          (define (insert-internal bst)
            (cond
              [(false? bst) (make-bst widget
                                      false
                                      false)]
              [(lt-func (field-func widget)
                        (field-func (bst-widget bst)))
               (make-bst (bst-widget bst)
                         (insert-internal (bst-left bst))
                         (bst-right bst))]
              [(eq-func (field-func widget)
                        (field-func (bst-widget bst)))
               bst]
              [else (make-bst (bst-widget bst)
                              (bst-left bst)
                              (insert-internal (bst-right bst)))]))]
    (db--set-bst db (insert-internal (db-bst db)))))



(define (db--set-bst db bst)
  (make-db (db-field db)
           (db-lt? db)
           (db-eq? db)
           bst))