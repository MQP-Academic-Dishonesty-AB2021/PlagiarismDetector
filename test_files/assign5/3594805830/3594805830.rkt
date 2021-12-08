

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 10 10))
(define MANY (make-widget "MANY" 500 500))


(define-struct bst (widget left right))




(define BST0 (make-bst W1 false false))
(define BSTZ (make-bst Z1 false false))
(define BST1 (make-bst Z1 BST0 false))
(define BST2 (make-bst A1 false false))
(define BST3 (make-bst D1 BST2 BST1))
(define BSTC (make-bst C1 false false))
(define BST4 (make-bst D1 (make-bst A1 false BSTC) BST1))
(define BSTM (make-bst MANY false false))

(define BSTQ (make-bst D1 (make-bst A1 BST0 false)
                       (make-bst Z1 BSTC false)))
(define BSTQ2 (make-bst D1 (make-bst A1 BST0 false)
                       (make-bst Z1 BSTC BSTM)))

(define BSTP (make-bst D1 (make-bst Z1 false BSTC)
                       (make-bst A1 false BST0)))
(define BSTP2 (make-bst D1 (make-bst Z1 BSTM BSTC)
                       (make-bst A1 false BST0)))





(define-struct db (field lt? eq? bst))


(define DB0 (make-db widget-name string<? string=? false))
(define DB-name (make-db widget-name string<? string=? BST4))
(define DB-name2 (make-db widget-name string<? string=? BST3))
(define DB-quantity (make-db widget-quantity < = BSTQ))


(define DB-price (make-db widget-price > = false))
(define DB-price2 (make-db widget-price > = BSTP))





(define (find key db)
  (local [
          (define (same? key bst)
            ((db-eq? db) key ((db-field db) (bst-widget bst))))

          (define (smaller? key bst)
            ((db-lt? db) key ((db-field db) (bst-widget bst))))

          (define (find-key bst)
            (cond [(false? bst) false]
                  [(same? key bst) (bst-widget bst)]
                  [(smaller? key bst)
                   (find-key (bst-left bst))]
                  [else
                   (find-key (bst-right bst))]))
          ]
    (find-key (db-bst db))))

(check-expect (find "W1" DB0) false)    
(check-expect (find "W1" DB-name) W1)   
(check-expect (find 10 DB-quantity) C1) 
(check-expect (find 3 DB-price2) A1)    




(define (insert item db)
  (local [
          (define (smaller? item bst)
            ((db-lt? db) ((db-field db) item)
                         ((db-field db) (bst-widget bst))))


          (define (insert-item bst)
            (cond [(false? bst) (make-bst item false false)]
                  [(smaller? item bst)
                   (make-bst (bst-widget bst)
                             (insert-item (bst-left bst))
                             (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst)
                             (bst-left bst)
                             (insert-item (bst-right bst)))]))
          ]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (insert-item (db-bst db)))))

(check-expect (db-bst (insert W1 DB0)) BST0)            
(check-expect (db-bst (insert C1 DB-name2)) BST4)       
(check-expect (db-bst (insert MANY DB-quantity)) BSTQ2) 
(check-expect (db-bst (insert MANY DB-price2)) BSTP2)   
  

