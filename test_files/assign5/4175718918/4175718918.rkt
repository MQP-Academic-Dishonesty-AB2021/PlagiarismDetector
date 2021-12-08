

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment5_PART2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define-struct widget (name quantity price))




(define-struct bst (widget left right))










(define-struct db (field lt? eq? bst))




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 23 10))
(define C1 (make-widget "C1" 4 4))


(define BST5 (make-bst Z1 false false))
(define BST4 (make-bst C1 false false))
(define BST3 (make-bst W1 false BST5))
(define BST2 (make-bst A1 false false))
(define BST1 (make-bst B1 BST2 BST4))
(define BST0 (make-bst D1 BST1 BST3))


(define BSTQ3 BST5)
(define BSTQ2 (make-bst B1 false BSTQ3))
(define BSTQ1 BST2)
(define BSTQ0 (make-bst C1 BSTQ1 BSTQ2))



(define BSTP5 (make-bst A1 false false))
(define BSTP4 (make-bst C1 BSTP5 false))
(define BSTP3 (make-bst Z1 false false))
(define BSTP2 (make-bst B1 false BSTP3))
(define BSTP1 (make-bst W1 false BSTP4))
(define BSTP0 (make-bst D1 BSTP1 BSTP2))


(define DB-quantity (make-db widget-quantity < = BSTQ0))
(define DB-name (make-db widget-name string<? string=? BST0))
(define DB-empty (make-db widget-quantity < = false))
(define DB-price (make-db widget-price < = BSTP2))





(check-expect (find-db "Z1" DB-name) Z1)
(check-expect (find-db "D1" DB-name) D1)
(check-expect (find-db "" DB-name) false)
(check-expect (find-db "D1" DB-empty) false)

(check-expect (find-db 23 DB-quantity) B1)
(check-expect (find-db 2 DB-quantity) A1)
(check-expect (find-db 0 DB-quantity) false)
(check-expect (find-db 5 DB-empty) false)

(check-expect (find-db 16 DB-price) Z1)
(check-expect (find-db 420 DB-price) false)
(check-expect (find-db 69 DB-empty) false)

(define (find-db val db)
  (cond
    [(false? (db-bst db)) false]
    [(same? val db) (bst-widget (db-bst db))]
    [(smaller? val db)
     (find-db val (make-db (db-field db) (db-lt? db) (db-eq? db) (bst-left (db-bst db))))]
    [(not(smaller? val db))
     (find-db val (make-db (db-field db) (db-lt? db) (db-eq? db) (bst-right (db-bst db))))]))






(check-expect (smaller? "A" DB-name) true)   
(check-expect (smaller? "Z" DB-name) false)

(define (smaller? v db) 
  ((db-lt? db) v  ((db-field db) (bst-widget (db-bst db)))))






(check-expect (same? "D1" DB-name) true)
(check-expect (same? "XYZ" DB-name) false)

(define (same? v db)
  ((db-eq? db) v ((db-field db) (bst-widget (db-bst db)))))





(check-expect (db-bst (insert A1 DB-empty)) (make-bst A1 false false))
(check-expect (db-bst (insert W1 DB-quantity)) (make-bst C1                                                
                                                         (make-bst A1
                                                                   (make-bst W1 false false)
                                                                   false)
                                                         (make-bst B1
                                                                   false                                                 
                                                                   (make-bst Z1 false false ))))

(check-expect (db-bst (insert D1 DB-quantity)) (make-bst C1
                                                         (make-bst A1 false false)
                                                         (make-bst B1                                                        
                                                          (make-bst D1 false false)
                                                          (make-bst Z1 false false))))
(check-expect (db-bst (insert D1 DB-price)) (make-bst B1
                                                      (make-bst D1 false false)
                                                      (make-bst Z1 false false)))
             

(define (insert val db)
  (local[
         
         
         (define (insert-inner val db)          
           (make-db (db-field db) (db-lt? db) (db-eq? db)
                    (insert-value val (db-bst db))))

         
         
         (define (smaller? w b)
           ((db-lt? db) ((db-field db) w) ((db-field db) (bst-widget b))))

         
         
         
         (define (insert-value w b)
           (cond
             [(false? b) (make-bst w false false)]   
             [(smaller? w b) 
              (make-bst (bst-widget b)
                        (insert-value w (bst-left b))
                        (bst-right b))]
             [else                                   
              (make-bst (bst-widget b)
                        (bst-left b)
                        (insert-value w (bst-right b)))]))
         ]
    (insert-inner val db)))