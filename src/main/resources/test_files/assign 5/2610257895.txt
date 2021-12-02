

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Ass 5 part 2 No Insert|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))









(define-struct widget (name quantity price))




(define-struct bst (widget left right))










(define-struct db (field lt? eq? bst))




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define X1 (make-widget "X1" 21 45))
(define Y1 (make-widget "Y1" -21 45))


(define BSTn (make-bst D1
                       (make-bst A1 false false)
                       (make-bst W1 false false)))
(define BSTn+X1 (make-bst D1
                          (make-bst A1 false false)
                          (make-bst W1
                                    false
                                    (make-bst X1 false false))))

(define BSTq (make-bst A1
                       (make-bst W1 false false)
                       (make-bst Z1 false false)))
(define BSTq+X1 (make-bst A1
                          (make-bst W1 false false)
                          (make-bst Z1
                                    (make-bst X1 false false)
                                    false)))
(define BSTq+Y1 (make-bst A1
                          (make-bst W1
                                    (make-bst Y1 false false)
                                    false)
                          (make-bst Z1 false false)))

(define BSTp (make-bst D1
                       (make-bst A1 false false)
                       (make-bst Z1 false false)))
(define BSTp+X1 (make-bst D1
                          (make-bst A1 false false)
                          (make-bst Z1
                                    false
                                    (make-bst X1 false false))))
(define BSTp+X1+X1 (make-bst D1 
                             (make-bst A1 false false)
                             (make-bst Z1
                                       false
                                       (make-bst X1
                                                 false
                                                 (make-bst X1 false false)))))


(define DB-quantity (make-db widget-quantity < = BSTq))
(define DB-name (make-db widget-name string<? string=? BSTn))
(define DB-price (make-db widget-price < = BSTp))











(check-expect (find "" DB-name) false)

(check-expect (find "W1" DB-name) W1)

(check-expect (find "X1" DB-name) false)



(check-expect (find -173 DB-price) false)

(check-expect (find 16 DB-price) Z1)

(check-expect (find 45 DB-price) false)

(check-expect (find -173 DB-quantity) false)

(check-expect (find 51 DB-quantity) Z1)

(check-expect (find 45 DB-price) false)
              
  

(define (find val db)
  (cond
    [(false? (db-bst db)) false]
    [(db-compare? db-eq? db val)
     (bst-widget (db-bst db))]
    [(db-compare? db-lt? db val)
     (find val (make-db
                (db-field db)
                (db-lt? db)
                (db-eq? db)
                (bst-left (db-bst db))))]
    [else
     (find val (make-db
                (db-field db)
                (db-lt? db)
                (db-eq? db)
                (bst-right (db-bst db))))]))










(check-expect (db-bst (insert X1 DB-name)) BSTn+X1)
(check-expect (db-bst (insert X1 DB-quantity)) BSTq+X1)
(check-expect (db-bst (insert Y1 DB-quantity)) BSTq+Y1)
(check-expect (db-bst (insert X1 DB-price)) BSTp+X1)

(check-expect
 (db-bst (insert X1 (make-db widget-price < = BSTp+X1))) BSTp+X1+X1)

 

(define (insert widget db)
  (local [
          
          
          (define (db-lessthan? db bst)
            ((db-lt? db) ((db-field db) widget)
                         ((db-field db) (bst-widget bst))))
          
          
          (define (insert bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [(db-lessthan? db bst)
                   (make-bst
                    (bst-widget bst)
                    (insert (bst-left bst))
                    (bst-right bst))]
        
                  [else
                   (make-bst
                    (bst-widget bst)
                    (bst-left bst)
                    (insert (bst-right bst)))]))]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (insert (db-bst db)))))














(check-expect (db-compare? db-eq? DB-quantity 2) true) 
(check-expect (db-compare? db-eq? DB-quantity 3) false) 
(check-expect (db-compare? db-lt? DB-price 2) true) 
(check-expect (db-compare? db-lt? DB-price 5) false) 
(check-expect (db-compare? db-lt? DB-price 487) false) 

(check-expect (db-compare? db-eq? DB-name "D1") true) 
(check-expect (db-compare? db-eq? DB-name "Sussy Baka") false) 
(check-expect (db-compare? db-lt? DB-name "Amogus") true) 
(check-expect (db-compare? db-lt? DB-name "D1") false) 
(check-expect (db-compare? db-lt? DB-name "") true) 

  

(define (db-compare? db-fn db val)
  ((db-fn db) val ((db-field db) (bst-widget (db-bst db)))))
