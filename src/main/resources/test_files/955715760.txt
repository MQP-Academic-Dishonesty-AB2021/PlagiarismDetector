

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))


(define-struct bst (widget left right))




(define TW1 (make-bst W1 #f #f))
(define TZ1 (make-bst Z1 #f #f))
(define TA1 (make-bst A1 #f #f))
(define TD1 (make-bst D1 #f #f))


(define T0 (make-bst W1 TA1 TZ1))
(define T1 (make-bst A1 #f TW1))
(define T2 (make-bst W1 #f TW1))
(define T3 (make-bst D1 TA1 #f))
(define T4 (make-bst W1
                     (make-bst A1
                               #f
                               TD1)
                     TZ1))





(define-struct db (field lt? eq? bst))

(define DB-price (make-db widget-price > = false))
(define DB-quantity (make-db widget-quantity < = false))
(define DB-price2 (make-db widget-price < = false))
(define DB-name (make-db widget-name string<? string=? false))


(define (insert w db)
  
  (local [(define (insert--bst w b dbfield dblt? dbeq?)
            (cond [(eq? b #f) (make-bst w #f #f)]
                  [(dblt? (dbfield w) (dbfield (bst-widget b)))
                   (make-bst
                    (bst-widget b)
                    (insert--bst w (bst-left b) dbfield dblt? dbeq?)
                    (bst-right b))]
                  [else
                   (make-bst
                    (bst-widget b)
                    (bst-left b)
                    (insert--bst w (bst-right b) dbfield dblt? dbeq?))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert--bst w
                          (db-bst db)
                          (db-field db)
                          (db-lt? db)
                          (db-eq? db)))))


(check-expect (db-bst (insert A1 DB-name))
              (make-bst A1
                        #f
                        #f))

(check-expect (db-bst (insert W1 (insert W1 DB-name)))
              (make-bst W1
                        #f
                        (make-bst W1 #f #f)))

(check-expect (db-bst (insert A1 (insert W1 DB-name)))
              (make-bst W1
                        (make-bst A1 #f #f)
                        #f))

(check-expect (db-bst (insert Z1 (insert W1 DB-name)))
              (make-bst W1
                        #f
                        (make-bst Z1 #f #f)))

(check-expect (db-bst (insert D1 (insert A1 (insert W1 DB-name))))
              (make-bst W1
                        (make-bst A1
                                  #f
                                  (make-bst D1 #f #f))
                        #f))


(check-expect (db-bst (insert W1 (insert A1 DB-quantity)))
              (make-bst A1
                        (make-bst W1 #f #f)
                        #f))

(check-expect (db-bst (insert D1 (insert A1 DB-quantity)))
              (make-bst A1
                        #f
                        (make-bst D1 #f #f)))

(check-expect (db-bst (insert D1 (insert A1 (insert W1 DB-quantity))))
              (make-bst W1
                        #f
                        (make-bst A1
                                  #f
                                  (make-bst D1 #f #f))))


(check-expect (db-bst (insert W1 (insert A1 DB-price)))
              (make-bst A1
                        #f
                        (make-bst W1 #f #f)))

(check-expect (db-bst (insert D1 (insert A1 DB-price)))
              (make-bst A1
                        (make-bst D1 #f #f)
                        #f))

(check-expect (db-bst (insert D1 (insert A1 (insert W1 DB-price))))
              (make-bst W1
                        (make-bst A1
                                  (make-bst D1 #f #f)
                                  #f)
                        #f))
                        
                        



(define (find val db)
  (if (not (false? (db-bst db)))
      (local
        
        [(define (find bst)
           (local
             [(define (smaller? val)
                ((db-lt? db) val
                             ((db-field db) (bst-widget bst))))
              (define (same? val)
                ((db-eq? db) val
                             ((db-field db) (bst-widget bst))))]
             (cond [(same? val) (bst-widget bst)]
                   [(smaller? val) (find (bst-left bst))]
                   [else (find (bst-right bst))])))]
        (find (db-bst db)))
      false))

(check-expect (find "W1" DB-quantity) false)
(check-expect (find "Z1" DB-name) false)
(check-expect (find "W1"
                    (make-db
                     widget-name
                     string<?
                     string=? T0))W1)
(check-expect (find 51
                    (make-db
                     widget-quantity
                     <
                     =
                     T4)) Z1)
(check-expect (find 3
                    (make-db
                     widget-price
                     <
                     =
                     T3)) A1)