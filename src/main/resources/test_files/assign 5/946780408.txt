

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))




(define-struct widget (name quantity price))

(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 25 25))

 


(define-struct bst (widget left right))



(define BSTA1 (make-bst A1 false false))
(define BSTW1 (make-bst W1 false false))
(define BSTZ1 (make-bst Z1 BSTW1 false))
(define BSTD1 (make-bst D1 BSTA1 BSTZ1))




(define BSTA2 (make-bst A1 false false))
(define BSTW2 (make-bst W1 false BSTA2))
(define BSTZ2 (make-bst Z1 false false))
(define BSTD2 (make-bst D1 BSTW2 BSTZ2))




(define BSTA3 (make-bst A1 false false))
(define BSTW3 (make-bst W1 BSTA3 false))
(define BSTZ3 (make-bst Z1 false false))
(define BSTD3 (make-bst D1 BSTZ3 BSTW3))




(define BSTA4 (make-bst A1 false false))
(define BSTZ4 (make-bst Z1 false false))
(define BSTW4 (make-bst W1 BSTZ4 false))
(define BSTD4 (make-bst D1 BSTW4 BSTA4))




 


(define-struct db (field lt? eq? bst))









(define DB-quantity (make-db widget-quantity < = false))
(define DB-name (make-db widget-name string<? string=? false))
(define DB1 (make-db widget-name string<? string=? BSTD1))
(define DB2 (make-db widget-quantity < = BSTD2))
(define DB3 (make-db widget-price > = BSTD3))
(define DB4 (make-db widget-name string>? string=? BSTD4))

(define DB-AVL1 (make-db widget-quantity < =
                         (make-bst (make-widget "10" 10 0)
                                   (make-bst (make-widget "5" 5 0)
                                             (make-bst (make-widget "-10" -10 0)
                                                       false
                                                       false)
                                             (make-bst (make-widget "7" 7 0)
                                                       false
                                                       false))
                                   (make-bst (make-widget "15" 15 0)
                                             false
                                             false))))
(define DB-AVL2 (make-db widget-quantity < =
                         (make-bst (make-widget "10" 10 0)
                                   (make-bst (make-widget "5" 5 0)
                                             false
                                             false)
                                   (make-bst (make-widget "15" 15 0)
                                             (make-bst (make-widget "12" 12 0)
                                                       false
                                                       false)
                                             (make-bst (make-widget "20" 20 0)
                                                       false
                                                       false)))))








(define (height b)
  (local
    
    
    [(define (height-helper b d)
       (cond
         [(false? b) d]
         [else
          (max d
               (height-helper (bst-left b) (add1 d))
               (height-helper (bst-right b) (add1 d)))]))]
    (height-helper b 0)))






(define (height-diff b)
  (if (false? b)
      0
      (- (height (bst-left b))
         (height (bst-right b)))))





(check-expect (db-bst (insert! C1 DB1))
              (make-bst D1
                        (make-bst A1
                                  false
                                  (make-bst C1
                                            false
                                            false))
                        BSTZ1))

(check-expect (db-bst (insert! C1 DB2))
              (make-bst D1
                        BSTW2
                        (make-bst Z1
                                  (make-bst C1
                                            false
                                            false)
                                  false)))

(check-expect (db-bst (insert! C1 DB3))
              (make-bst D1
                        (make-bst Z1
                                  (make-bst C1
                                            false
                                            false)
                                  false)
                        BSTW3))

(check-expect (db-bst (insert! C1 DB4))
              (make-bst D1
                        BSTW4
                        (make-bst A1
                                  (make-bst C1
                                            false
                                            false)
                                  false)))

(check-expect (db-bst (insert! C1 DB-name))
              (make-bst C1
                        false
                        false))



(define (insert! widget db)
  (local [(define (insert!2 bst)
            (cond [(false? bst) (set-db-bst! db (make-bst widget false false))]
                  [(or ((db-eq? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst)))
                       ((db-lt? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst))))
                   (if (false? (bst-left bst))
                       (set-bst-left! bst (make-bst widget false false))
                       (insert!2 (bst-left bst)))]
                  [else
                   (if (false? (bst-right bst))
                       (set-bst-right! bst (make-bst widget false false))
                       (insert!2 (bst-right bst)))]))]
    (begin (insert!2 (db-bst db)) db)))





(check-expect (db-bst (insert-avl (make-widget "-20" -20 0)
                                  DB-AVL1))
              (make-bst (make-widget "5" 5 0)
                        (make-bst (make-widget "-10" -10 0)
                                  (make-bst (make-widget "-20" -20 0)
                                            false
                                            false)
                                  false)
                        (make-bst (make-widget "10" 10 0)
                                  (make-bst (make-widget "7" 7 0)
                                            false
                                            false)
                                  (make-bst (make-widget "15" 15 0)
                                            false
                                            false))))

(check-expect (db-bst (insert-avl (make-widget "25" 25 0)
                                  DB-AVL2))
              (make-bst (make-widget "15" 15 0)
                        (make-bst (make-widget "10" 10 0)
                                  (make-bst (make-widget "5" 5 0)
                                            false
                                            false)
                                  (make-bst (make-widget "12" 12 0)
                                            false
                                            false))
                        (make-bst (make-widget "20" 20 0)
                                  false
                                  (make-bst (make-widget "25" 25 0)
                                            false
                                            false))))



(define (insert-avl widget db)
  (local [(define (insert2 bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [(or ((db-eq? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst)))
                       ((db-lt? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst))))
                   (make-bst (bst-widget bst)
                             (balance (insert2 (bst-left bst)))
                             (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst)
                             (bst-left bst)
                             (balance (insert2 (bst-right bst))))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (balance (insert2 (db-bst db))))))





(check-expect (db-bst (insert-avl! (make-widget "-20" -20 0)
                                   DB-AVL1))
              (make-bst (make-widget "5" 5 0)
                        (make-bst (make-widget "-10" -10 0)
                                  (make-bst (make-widget "-20" -20 0)
                                            false
                                            false)
                                  false)
                        (make-bst (make-widget "10" 10 0)
                                  (make-bst (make-widget "7" 7 0)
                                            false
                                            false)
                                  (make-bst (make-widget "15" 15 0)
                                            false
                                            false))))

(check-expect (db-bst (insert-avl! (make-widget "25" 25 0)
                                   DB-AVL2))
              (make-bst (make-widget "15" 15 0)
                        (make-bst (make-widget "10" 10 0)
                                  (make-bst (make-widget "5" 5 0)
                                            false
                                            false)
                                  (make-bst (make-widget "12" 12 0)
                                            false
                                            false))
                        (make-bst (make-widget "20" 20 0)
                                  false
                                  (make-bst (make-widget "25" 25 0)
                                            false
                                            false))))



(define (insert-avl! widget db)
  (local [(define (insert2! bst)
            (cond [(false? bst) (set-db-bst! db (make-bst widget false false))]
                  [(or ((db-eq? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst)))
                       ((db-lt? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst))))
                   (if (false? (bst-left bst))
                       (set-bst-left! bst (balance (make-bst widget false false)))
                       (insert2! (bst-left bst)))]
                  [else
                   (if (false? (bst-right bst))
                       (set-bst-right! bst (balance (make-bst widget false false)))
                       (insert2! (bst-right bst)))]))]
    (begin (insert2! (db-bst db))
           (set-db-bst! db (balance (db-bst db)))
           db)))




(check-expect (balance 
               (make-bst (make-widget "5" 5 0)
                         (make-bst (make-widget "-10" -10 0)
                                   (make-bst (make-widget "-20" -20 0)
                                             false
                                             false)
                                   false)
                         false))
              (make-bst (make-widget "-10" -10 0)
                        (make-bst (make-widget "-20" -20 0)
                                  false
                                  false)
                        (make-bst (make-widget "5" 5 0)
                                  false
                                  false)))



(define (balance bst)
  (local [(define diff (height-diff bst))]
    (cond [(>= diff 2) 
           (local [(define newRoot (bst-left bst))]
             (if (positive? (height-diff newRoot))
                 (rotate-left bst) 
                 (rotate-left (make-bst (bst-widget bst)
                                        (rotate-right newRoot)
                                        (bst-right bst)))))] 
          [(<= diff -2) 
           (local [(define newRoot (bst-right bst))]
             (if (negative? (height-diff newRoot))
                 (rotate-right bst)
                 (rotate-right (make-bst (bst-widget bst)
                                         (bst-left bst)
                                         (rotate-left newRoot)))))]
          [else
           bst])))





(check-expect (rotate-left (make-bst W1
                                     (make-bst A1
                                               false
                                               false)
                                     false))
              (make-bst A1
                        false
                        (make-bst W1
                                  false
                                  false)))

(check-expect (rotate-left (make-bst D1
                                     (make-bst W1
                                               (make-bst Z1
                                                         false
                                                         false)
                                               false)
                                     (make-bst A1
                                               false
                                               false)))
              (make-bst W1
                        (make-bst Z1
                                  false
                                  false)
                        (make-bst D1
                                  false
                                  (make-bst A1
                                            false
                                            false))))



(define (rotate-left bst)
  (local [(define newRoot (bst-left bst))]
    (make-bst (bst-widget newRoot)
              (bst-left newRoot)
              (make-bst (bst-widget bst)
                        (bst-right newRoot)
                        (bst-right bst)))))





(check-expect (rotate-right (make-bst D1
                                      false
                                      (make-bst W1
                                                false
                                                false)))
              (make-bst W1
                        (make-bst D1
                                  false
                                  false)
                        false))

(check-expect (rotate-right (make-bst D1
                                      (make-bst A1
                                                false
                                                false)
                                      (make-bst W1
                                                false
                                                (make-bst Z1
                                                          false
                                                          false))))
              (make-bst W1
                        (make-bst D1
                                  (make-bst A1
                                            false
                                            false)
                                  false)
                        (make-bst Z1
                                  false
                                  false)))



(define (rotate-right bst)
  (local [(define newRoot (bst-right bst))]
    (make-bst (bst-widget newRoot)
              (make-bst (bst-widget bst)
                        (bst-left bst)
                        (bst-left newRoot))
              (bst-right newRoot))))







(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))

(define (build-tree low db fn)
  (foldr fn db low))






(define (insert widget db)
  (local [(define (insert2 bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [(or ((db-eq? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst)))
                       ((db-lt? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst))))
                   (make-bst (bst-widget bst)
                             (insert2 (bst-left bst))
                             (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst)
                             (bst-left bst)
                             (insert2 (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert2 (db-bst db)))))

(define (time-insert n)
  (local [(define DB (make-db widget-quantity < = false))
          (define widgets (random-widgets n (* n 4)))]
    (begin (time (build-tree widgets DB insert))
           (set-db-bst! DB false)
           (time (build-tree widgets DB insert!))
           "done")))




(define (find key db)
  (cond [(false? (db-bst db)) false]
        [((db-eq? db) key ((db-field db) (bst-widget (db-bst db))))
         (bst-widget (db-bst db))]
        [((db-lt? db) key ((db-field db) (bst-widget (db-bst db))))
         (find key (make-db (db-field db) (db-lt? db) (db-eq? db)
                            (bst-left (db-bst db))))]
        [else
         (find key (make-db (db-field db) (db-lt? db) (db-eq? db)
                            (bst-right (db-bst db))))]))

(define (time-find n)
  (local [(define widgets (random-widgets n (* 4 n)))
          (define DB-BST (build-tree widgets
                                     (make-db widget-quantity < = false)
                                     insert!))
          (define DB-AVL (build-tree widgets
                                     (make-db widget-quantity < = false)
                                     insert-avl))
          (define (findAll db widgets)
            (cond [(empty? widgets) (void)]
                  [else
                   (begin (find ((db-field db) (first widgets)) db)
                          (findAll db (rest widgets)))]))]
    (begin (time (findAll DB-BST widgets))
           (time (findAll DB-AVL widgets))
           "done")))