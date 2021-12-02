

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 5 Part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))











(define-struct widget (name quantity price))







(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 6 4))




(define-struct bst (widget left right))







(define BST1 (make-bst W1 false false))
(define BST-by-name (make-bst W1
                              (make-bst A1 false false)
                              (make-bst Z1 false false)))
(define BST-by-price (make-bst D1
                               (make-bst Z1 false false)
                               (make-bst A1 false false)))

(define BST-by-quantity (make-bst A1
                                  (make-bst W1 false false)
                                  (make-bst Z1
                                            (make-bst D1 false false)
                                            false)))





(define-struct db (field lt? eq? bst))













(define DB0 (make-db widget-name string<? string=? false))


(define DB1 (make-db widget-name string<? string=? BST1))


(define DB2 (make-db widget-name string<? string=? BST-by-name))


(define DB3 (make-db widget-price
                     >
                     (λ(n p)
                       (integer? (/ n p)))
                     BST-by-price))


(define DB4 (make-db widget-quantity < = BST-by-quantity))














(check-expect (find (widget-name W1) DB0)
              false) 

(check-expect (find (widget-name W1) DB2)
              W1) 

(check-expect (find (widget-name Z1) DB2)
              Z1) 


(check-expect (find (widget-price W1) DB3)
              false) 
(check-expect (find (widget-price A1) DB3)
              A1) 
(check-expect (find (widget-price Z1) DB3)
              Z1) 


(check-expect (find (widget-quantity E1) DB4)
              false) 
(check-expect (find (widget-quantity D1) DB4)
              D1) 
(check-expect (find (widget-quantity Z1) DB4)
              Z1) 


(define (find key db)
  (local [
          (define (find-bst bst)
            (cond [(false? bst) false]
                  [(same? db key bst)
                   (bst-widget bst)]
                  [(smaller? db key bst)
                   (find-bst (bst-left bst))]
                  [else
                   (find-bst (bst-right bst))]))]
    (find-bst (db-bst db))))

          










(check-expect (db-bst (insert Z1 DB0))
              (make-bst Z1 false false)) 

(check-expect (db-bst (insert A1 DB1))
              (make-bst W1
                        (make-bst A1 false false)
                        false)) 

(check-expect (db-bst (insert Z1 DB1))
              (make-bst W1
                        false
                        (make-bst Z1 false false))) 

(check-expect (db-bst (insert D1 DB2))
              (make-bst W1
                        (make-bst A1 false
                                  (make-bst D1 false false))
                        (make-bst Z1 false false))) 

(check-expect (db-bst (insert W1 DB3))
              (make-bst D1
                        (make-bst Z1
                                  false
                                  false)
                        (make-bst A1
                                  false
                                  (make-bst W1 false false)))) 

(check-expect (db-bst (insert E1 DB4))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1 false
                                            (make-bst E1 false false))
                                  false))) 
              



(define (insert key db)
  (local [(define (insert-bst bst) 
            (cond
              [(false? bst) (make-bst key false false)]
              [(same? db ((db-field db) key) bst) bst]
              [(smaller? db ((db-field db) key) bst)
               (make-bst
                (bst-widget bst)
                (insert-bst (bst-left bst))
                (bst-right bst))]
              [else
               (make-bst
                (bst-widget bst)
                (bst-left bst)
                (insert-bst (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-bst (db-bst db)))))















(check-expect (same? DB0 "W1" false) false) 
(check-expect (same? DB0 "W1" BST1) (string=? "W1" "W1")) 
(check-expect (same? DB2 "X1" BST-by-name) (string=? "X1" "W1")) 
(check-expect (same? DB2 "Z1" BST-by-name) (string=? "Z1" "W1")) 
(check-expect (same? DB3 5 BST-by-price) (integer? (/ 5 5))) 
(check-expect (same? DB3 6 BST-by-price) (integer? (/ 6 5))) 
(check-expect (same? DB4 2 BST-by-quantity) (= 2 2)) 
(check-expect (same? DB4 1 BST-by-quantity) (= 1 2)) 



(define (same? db key bst)
  (cond [(false? bst) false]
        [else
         ((db-eq? db) key ((db-field db) (bst-widget bst)))]))














(check-expect (smaller? DB0 "W1" false) false) 
(check-expect (smaller? DB0 "W1" BST1) (string<? "W1" "W1")) 
(check-expect (smaller? DB0 "abc" BST1) (string<? "abc" "W1")) 
(check-expect (smaller? DB2 "X1" BST-by-name) (string<? "X1" "W1")) 
(check-expect (smaller? DB2 "123" BST-by-name) (string<? "123" "W1")) 
(check-expect (smaller? DB3 6 BST-by-price) (> 6 5)) 
(check-expect (smaller? DB3 4 BST-by-price) (> 4 5)) 
(check-expect (smaller? DB4 3 BST-by-quantity) (< 3 2)) 
(check-expect (smaller? DB4 1 BST-by-quantity) (< 1 2)) 


(define (smaller? db key bst)
  (cond [(false? bst) false]
        [else
         ((db-lt? db) key ((db-field db) (bst-widget bst)))]))




