

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))




(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))











(define W1 (make-widget "W1" 1 1))
(define W2 (make-widget "Y1" 51 16))
(define W3 (make-widget "B1" 2 3))
(define W4 (make-widget "D1" 5 5))

(define W5 (make-widget "A1" 0 0))
(define W6 (make-widget "Z1" 100 100))

(define W7 (make-widget "X1" 3 3))



(define B0 (make-bst W4 false false))

(define B1
  (make-bst W1
            false
            (make-bst W2 false false)))
(define B2
  (make-bst W1
            (make-bst W3
                      false
                      (make-bst W4 false false))
            (make-bst W2
                      false
                      false)))

(define B3
  (make-bst W1
            (make-bst
             W3
             (make-bst W5 false false)
             (make-bst W4 false false))
            (make-bst W2 false false)))

(define B4
  (make-bst W1
            (make-bst
             W3
             false
             (make-bst W4 false false))
            (make-bst
             W2
             false
             (make-bst W6 false false))))

(define B5
  (make-bst W1
            (make-bst
             W3
             false
             (make-bst W4 false false))
            (make-bst
             W2
             (make-bst W7 false false)
             false)))


(define DB0 (make-db widget-name string<? string=? B0))
(define DB1 (make-db widget-name string<? string=? B1))
(define DB2 (make-db widget-name string<? string=? B2))
(define DB3 (make-db widget-quantity < = B1))
(define DBempty (make-db widget-price > = false))







(check-expect (find "D1" DB0) W4) 
(check-expect (find "Y1" DB2) W2)
(check-expect (find 51 DB3) W2) 

(check-expect (find "A0" DB0) false) 
(check-expect (find "L20" DB2) false) 
(check-expect (find 50 DB3) false) 


(define (find val db)
  (local [(define (find-name b)
            (cond
              [(false? b) false] 
              [((db-eq? db) val ((db-field db) (bst-widget b))) 
               (bst-widget b)]
              [((db-lt? db) val ((db-field db) (bst-widget b))) 
               (find-name (bst-left b))]
              [else                              
               (find-name (bst-right b))]))]
    (find-name (db-bst db))))






(check-expect (db-bst (insert W4 DBempty)) B0) 

(check-expect
 (db-bst
  (insert
   W5
   (make-db widget-name string<? string=? B2)))
 B3) 

(check-expect
 (db-bst
  (insert
   W6
   (make-db widget-name string<? string=? B2)))
 B4) 

(check-expect
 (db-bst
  (insert
   W7
   (make-db widget-name string<? string=? B2)))
 B5) 



(define (insert w db)
  (local
    [(define (insert-name b)
       (cond
         [(false? b) (make-bst w false false)]
         [((db-lt? db) ((db-field db) w) ((db-field db) (bst-widget b)))
          (make-bst
           (bst-widget b)
           (insert-name (bst-left b))
           (bst-right b))]
         [else
          (make-bst
           (bst-widget b)
           (bst-left b)
           (insert-name (bst-right b)))]))]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (insert-name (db-bst db)))))
