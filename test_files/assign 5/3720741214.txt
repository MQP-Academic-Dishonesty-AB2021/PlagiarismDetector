

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(define-struct widget (name quantity price))




(define-struct bst (widget left right))




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 2 3))



(define BST0 false)


(define BST1 (make-bst
              W1 false false))


(define BST2 (make-bst
              D1
              (make-bst
               A1 false false)
              (make-bst
               Z1 false false)))


(define BST3 (make-bst
              D1
              (make-bst
               A1 false false)
              (make-bst
               Z1
               (make-bst
                W1 false false)
               false)))


(define BST5 
  (make-bst
   A1
   (make-bst W1 false false)
   (make-bst Z1 false false)))


(define-struct db (field lt? eq? bst))










(define DB-quantity (make-db widget-quantity < = false))

(define DB-name (make-db widget-name string<? string=? false))

(define DB-price (make-db widget-price < = BST5))

(define DB-name-BST3 (make-db widget-name string<? string=? BST3))








(check-expect (find (widget-quantity W1) DB-quantity) false)


(check-expect (find (widget-name W1) DB-name-BST3) W1)


(check-expect (find (widget-price Z1) DB-price) Z1)

(define (find key db)
  (local [
          
          (define (find-inner key bst)
            
            (cond [(false? bst) false]
                  [((db-eq? db) key
                                ((db-field db) (bst-widget bst)))
                   (bst-widget bst)]
                  [else
                   (if ((db-lt? db) key ((db-field db) (bst-widget bst)))
                       (find-inner key (bst-left bst))
                       (find-inner key (bst-right bst)))]))]
    (find-inner key (db-bst db))))






(check-expect (db-bst(insert W1 DB-quantity)) BST1)


(check-expect (db-bst (insert B1 DB-name-BST3)) BST4)

(define BST4 (make-bst
              D1
              (make-bst
               A1 false
               (make-bst B1 false false))
              (make-bst
               Z1
               (make-bst
                W1 false false)
               false)))


(check-expect (db-bst (insert B1 DB-price)) BST6)

(define BST6 (make-bst
              A1
              (make-bst W1 false false)
              (make-bst Z1
                        (make-bst B1 false false)
                        false)))


(define (insert value db)
  (local [
          
          (define (insert-inner value bst)
            (cond [(false? bst) (make-bst value false false)]
                  [else (if ((db-lt? db) ((db-field db) value)
                                         ((db-field db) (bst-widget bst)))
                            (make-bst (bst-widget bst)
                                      (insert-inner value (bst-left bst))
                                      (bst-right bst))
                            (make-bst (bst-widget bst)
                                      (bst-left bst)
                                      (insert-inner value (bst-right bst))))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-inner value (db-bst db)))))





