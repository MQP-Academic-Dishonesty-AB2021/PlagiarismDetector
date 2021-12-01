

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))




(define-struct bst (widget left right))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define test-list-widgets (list W1 Z1 A1 D1))

(define EXAMPLE-T1 (make-bst D1
                             (make-bst A1 false false)
                             (make-bst Z1
                                       (make-bst W1 false false)
                                       false)))
(define EXAMPLE-T2 (make-bst D1
                             (make-bst A1 false false)
                             (make-bst Z1
                                       false
                                       false)))
(define EXAMPLE-T3 (make-bst D1
                             false
                             (make-bst Z1
                                       (make-bst W1 false false)
                                       false)))
(define EXAMPLE-T4 false)







(define-struct db (field lt? eq? bst))

(define DB-quantity (make-db widget-quantity < = false))
(define DB-name (make-db widget-name string<? string=? false))
(define DB-price (make-db widget-price < = false))






(define (find key db)
  (local [(define (same? bst)
            ((db-eq? db) key ((db-field db) (bst-widget bst))))
          (define (smaller? bst)
            ((db-lt? db) key ((db-field db) (bst-widget bst))))
          (define (find bst)
            (cond [(false? bst) false]
                  [(same? bst)
                   (bst-widget bst)]
                  [(smaller? bst)
                   (find (bst-left bst))]
                  [else
                   (find (bst-right bst))]))]
    (find (db-bst db))))


(define EXAMPLE-D1 (make-db
                    (db-field DB-name)
                    (db-lt? DB-name)
                    (db-eq? DB-name)
                    EXAMPLE-T1))

(define EXAMPLE-D2 (make-db
                    (db-field DB-name)
                    (db-lt? DB-name)
                    (db-eq? DB-name)
                    EXAMPLE-T2))

(define EXAMPLE-D3 (make-db
                    (db-field DB-name)
                    (db-lt? DB-name)
                    (db-eq? DB-name)
                    EXAMPLE-T3))

(define EXAMPLE-D4 (make-db
                    (db-field DB-name)
                    (db-lt? DB-name)
                    (db-eq? DB-name)
                    EXAMPLE-T4))


(check-expect (find "D1" EXAMPLE-D1) D1)   
(check-expect (find "A1" EXAMPLE-D2) A1)   
(check-expect (find "W1" EXAMPLE-D3) W1)   
(check-expect (find "A1" EXAMPLE-D3) false)
(check-expect (find "A1" EXAMPLE-D4) false)



                       



(define (insert widget db)
  (local [(define (smaller? bst)
            ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst))))
          (define (insert-bst bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [(smaller? bst)
                   (make-bst (bst-widget bst) (insert-bst (bst-left bst)) (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst) (bst-left bst) (insert-bst (bst-right bst)))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-bst (db-bst db)))))


(check-expect
 (db-bst (insert A1 (make-db widget-name string<? string=? (make-bst Z1 false false))))
 (make-bst Z1 (make-bst A1 false false) false))


(check-expect
 (db-bst (insert Z1 (make-db widget-name string<? string=? (make-bst A1 false false))))
 (make-bst A1 false (make-bst Z1 false false)))


(check-expect
 (db-bst (insert Z1 (make-db widget-name string<? string=? (make-bst A1 false
                                                                     (make-bst D1 false false)))))
 (make-bst A1 false (make-bst D1 false (make-bst Z1 false false))))


(check-expect
 (db-bst (insert A1 (make-db widget-name string<? string=? false)))
 (make-bst A1 false false))



