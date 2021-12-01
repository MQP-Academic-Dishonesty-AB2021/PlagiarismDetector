

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 2 4))

(define-struct bst (widget left right))




(define TREE1 (make-bst W1 
                        (make-bst D1 (make-bst A1 false false) false) (make-bst Z1 false false)))
(define TREE2 (make-bst Z1 
                        (make-bst A1 false (make-bst D1 false false)) false))
(define TREE3 (make-bst A1 
                        false (make-bst D1 false (make-bst W1 false (make-bst Z1 false false)))))



(define-struct db (field lt? eq? bst))

(define DB-price (make-db widget-price < = TREE1))    

(define DB-quantity (make-db widget-quantity < = TREE2))    

(define DB-name (make-db widget-name string<? string=? TREE3))    





(check-expect (find "W1" DB-name) W1) 
(check-expect (find 1 DB-quantity) false) 

(define (find key db)
  (local [
          (define eqfn (db-eq? db))
          (define smfn (db-lt? db))
          (define dbfield (db-field db))
          (define (inner key bst)
            (cond
              [(eqfn key (dbfield (bst-widget bst))) (bst-widget bst)]
              [(smfn key (dbfield (bst-widget bst))) (if (false? (bst-left bst))
                                                         false
                                                         (inner key (bst-left bst)))]
              [(not (smfn key (dbfield (bst-widget bst))))
               (if (false? (bst-right bst))
                   false
                   (inner key (bst-right bst)))]))
          ]
    (inner key (db-bst db))))




(check-expect (db-bst (insert W1 DB-quantity))  
              (make-bst Z1 
                        (make-bst A1 
                                  (make-bst W1 false false)
                                  (make-bst D1 false false)) false))
(check-expect (db-bst (insert C1 DB-price)) 
              (make-bst W1 
                        (make-bst D1 (make-bst A1 false false) false) 
                        (make-bst Z1 (make-bst C1 false false) false)))
(check-expect (db-bst (insert W1 DB-price)) 
              (make-bst W1 
                        (make-bst D1 (make-bst A1 false false) false) 
                        (make-bst Z1 false false)))
  
(define (insert widget db)
  (local [
          (define eqfn (db-eq? db))
          (define smfn (db-lt? db))
          (define dbfield (db-field db))
          (define (inner widget bst)
            (cond
              [(false? bst) (make-bst widget false false)]
              [(eqfn(dbfield widget) (dbfield (bst-widget bst))) bst]
              [(smfn (dbfield widget) (dbfield (bst-widget bst)))
               (make-bst
                (bst-widget bst)
                (inner widget (bst-left bst))
                (bst-right bst))]
              [(not (smfn (dbfield widget) (dbfield (bst-widget bst))))
               (make-bst
                (bst-widget bst)
                (bst-left bst)
                (inner widget (bst-right bst)))]))]
    (make-db dbfield smfn eqfn (inner widget (db-bst db)))))