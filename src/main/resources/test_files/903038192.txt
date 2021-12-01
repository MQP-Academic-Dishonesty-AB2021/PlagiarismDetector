

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))



(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 6 90))
(define C1 (make-widget "C1" 4 76))
(define D1 (make-widget "D1" 5 5))
(define K1 (make-widget "K1" 7 4))
(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))


(define-struct bst (widget left right))




(define bstA (make-bst A1 false false))
(define bstW (make-bst W1 false false))
(define bstD (make-bst D1 bstA bstW))
(define bstB (make-bst B1 bstA (make-bst D1 false false)))










(define-struct db (field lt? eq? bst))

(define DB-quantity (make-db widget-quantity < = false))
(define DB-price (make-db widget-price < = bstD))
(define DB-name (make-db widget-name string<? string=? bstD))
(define DB-name1 (make-db widget-name string<? string=? bstB))





(check-expect (bst->low bstA) (list A1))
(check-expect (bst->low false) empty)
(check-expect (bst->low bstD) (list W1 A1 D1))

(define (bst->low bst)
  (local [(define (inner bst todo rsf)
            (cond [(false? bst)
                   (if(empty? todo)
                      rsf
                      (inner (first todo) (rest todo) rsf))]
                  [else
                   (if (false? (bst-right bst))
                       (inner (bst-left bst) todo (cons (bst-widget bst) rsf))
                       (inner (bst-left bst)
                              (cons (bst-right bst) todo)
                              (cons (bst-widget bst) rsf)))]))]
    (inner bst empty empty)))






(check-expect (db-bst (insert DB-price  B1))
              (make-bst D1
                        bstA
                        (make-bst W1
                                  false
                                  (make-bst B1 false false))))
(check-expect (db-bst (insert DB-name K1 ))
              (make-bst D1
                        bstA
                        (make-bst W1
                                  (make-bst K1 false false)
                                  false
                                  )))

(check-expect (db-bst (insert DB-quantity K1 ))
              (make-bst K1 false false))


(define (insert DB widget)
  (cond
    [(false? (db-bst DB))
     (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (make-bst widget false false))]
    [((db-lt? DB)
      ((db-field DB) widget)
      ((db-field DB) (bst-widget (db-bst DB))))
     (make-db
      (db-field DB)
      (db-lt? DB)
      (db-eq? DB)
      (make-bst
       (bst-widget (db-bst DB))
       (db-bst(insert
               (make-db
                (db-field DB)
                (db-lt? DB)
                (db-eq? DB)
                (bst-left (db-bst DB)))
               widget))
       (bst-right (db-bst DB))))]
    [else
     (make-db
      (db-field DB)
      (db-lt? DB)
      (db-eq? DB)
      (make-bst
       (bst-widget (db-bst DB))
       (bst-left (db-bst DB))
       (db-bst (insert
                (make-db
                 (db-field DB)
                 (db-lt? DB)
                 (db-eq? DB)
                 (bst-right (db-bst DB)))
                widget))))]))
 




(check-expect (db-bst (sort-db-bst DB-price (bst->low (db-bst DB-price)))) (make-bst A1 bstW (make-bst D1 false false)))


(define (sort-db-bst db low) 
  (local
    [(define low0 (bst->low (db-bst db)))
     (define middle-low (if (empty? low0)
                            empty
                            (list-ref low0 (floor (/ (length low0) 2)))))
     (define low-new (cons middle-low low0))
     (define db-clean (make-db (db-field db)
                               (db-lt? db)
                               (db-eq? db)
                               (if (empty? middle-low)
                                   false
                                   (make-bst middle-low false false))))
     (define (create-bst-inner db low)
       (cond [(empty? low) db]
             [(false? (db-bst db)) db]
             [(string=?
               (widget-name (first low))
               (widget-name middle-low))
              (create-bst-inner db (rest low))]
             [else
              (create-bst-inner
               (insert db (first low))
               (rest low))]))
     ]
    (create-bst-inner db-clean low-new)
    ))





(check-expect (find 3 DB-price) A1)
(check-expect (find "D1" DB-name) D1)
(check-expect (find 1 DB-price) W1)
(check-expect (find 1 DB-quantity) false)





(define (find val DB)
  (local
    [(define sortedDB (sort-db-bst DB (bst->low (db-bst DB))))
     
     (define (search-db val DB) 
       (cond
         [(false? (db-bst DB)) 
          false]
         [((db-eq? DB) val ((db-field DB) (bst-widget (db-bst DB))) )
          (bst-widget (db-bst DB))]
         [((db-lt? DB) val ((db-field DB) (bst-widget (db-bst DB))) )
          (find val
                     (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (bst-left (db-bst DB))))]
         [else
          (find val
                     (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (bst-right (db-bst DB))))]))]
    (search-db val sortedDB)))

