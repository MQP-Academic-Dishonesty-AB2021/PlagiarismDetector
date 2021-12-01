

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2.rkt|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))




(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))










(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 3 5))
(define D1 (make-widget "D1" 5 7))



(check-expect (find "A1" (make-db  widget-name string<? string=? false)) false)
(check-expect (find "A1" (make-db widget-name string<? string=?
                                  (make-bst B1 false false))) false)
(check-expect (find "A1" (make-db widget-name string<? string=?
                                  (make-bst A1 false false))) A1)
(check-expect (find "A1" (make-db widget-name string<? string=?
                                  (make-bst B1
                                            (make-bst A1 false false) false))) A1)
(check-expect (find (widget-quantity A1) (make-db widget-quantity < =
                                                  (make-bst B1
                                                            (make-bst A1 false false) false))) A1)
(check-expect (find "D1" (make-db widget-name string<? string=?
                                  (make-bst B1 false
                                            (make-bst D1 false false)))) D1)
(check-expect (find (widget-price Z1) (make-db widget-price < =
                                               (make-bst B1
                                                         (make-bst A1 false false)
                                                         (make-bst D1 false false)))) false)
(define (find key db)
  (local [(define (smaller? key bst)
            ((db-lt? db) key ((db-field db) (bst-widget bst))))
          (define (same? key bst)
            ((db-eq? db) key ((db-field db) (bst-widget bst))))
          (define (find-bst bst)
            (cond [(false? bst) false]
                  [(same? key bst) (bst-widget bst)]
                  [else
                   (if (smaller? key bst)
                       (find-bst (bst-left bst))
                       (find-bst (bst-right bst)))])
            )]
    (find-bst (db-bst db)))
  )




(check-expect (db-bst (insert A1
                              (make-db widget-name string<? string=? false)))
              (make-bst A1 false false))
(check-expect (db-bst (insert A1
                              (make-db widget-name string<? string=?
                                       (make-bst B1 false false))))
              (make-bst B1
                        (make-bst A1 false false)
                        false))
(check-expect (db-bst (insert D1
                              (make-db widget-price > = (make-bst B1 false false))))
              (make-bst B1
                        (make-bst D1 false false)
                        false))
(check-expect (db-bst (insert W1
                              (make-db widget-quantity < =
                                       (make-bst B1
                                                 (make-bst A1 false false)
                                                 (make-bst D1 false false)))))
              (make-bst B1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  false)
                        (make-bst D1
                                  false
                                  false)))

(define (insert widget db)
  (local [(define (smaller? widget bst)
            ((db-lt? db)
             ((db-field db) widget)
             ((db-field db) (bst-widget bst))))
          (define (insert-bst bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [(smaller? widget bst)
                   (make-bst (bst-widget bst)
                             (insert-bst (bst-left bst))
                             (bst-right bst))]
                  [else (make-bst (bst-widget bst)
                                  (bst-left bst)
                                  (insert-bst (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (insert-bst (db-bst db)))))

