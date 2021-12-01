

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ASSIGNMENT5_PART2[wipwipwip]|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget (name quantity price))




(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))


(define (smaller? x db b)
  (cond
    [((db-lt? db) ((db-field db)x) ((db-field db)(bst-widget (db-bst db))))
     true]
    [else
     false])) 


(define (same? x db)
  (cond
    [((db-eq? db) ((db-field db)x) ((db-field db)(bst-widget (db-bst db))))
     true]
    [else
     false]))





(define (find x db)
  (local
    [(define b
       (make-bst
        (bst-widget (db-bst db))
        (bst-left (db-bst db))
        (bst-right (db-bst db))))
     
     (define (find-name x db)
       (cond
         [(false? b) false]
         [else
          (cond
            [(same? x db) (bst-widget b)]
            [(false? (smaller? x db)) (find-name x db (bst-right b))]
            [(smaller? x db) (find-name x db (bst-left b))])]))]
    
    (find-name x db)))






(define (insert w db)
  (local
    [(define b
       (make-bst
        (bst-widget (db-bst db))
        (bst-left (db-bst db))
        (bst-right (db-bst db))))

     (define op (db-lt? db))
     (define field (db-field db))

     (define (smaller? x bst)
       (cond
         [(op (field x) (field(bst-widget bst)))
          true]
         [else
          false]))

     (define (insert-w w bst)
       (cond
         [(false? bst)
          (make-bst w false false)]
         [else
          (cond
            [(smaller? w bst)
             (make-bst
              (bst-widget bst)
              (insert-w w (bst-left bst))
              (bst-right bst))]
            [else
             (make-bst
              (bst-widget bst)
              (bst-left bst)
              (insert-w w (bst-right bst)))])]))
           
          

     (define (insert-db w db)
       (cond
         [(false? b)
          (make-db
           (db-field db)
           (db-lt? db)
           (db-eq? db)
           (db-bst
            (make-bst w false false)))]
         [else
          (make-db
              (db-field db)
              (db-lt? db)
              (db-eq? db)
              (db-bst
               (insert-w w (db-bst db))))]))]
    
    (insert-db w db)))
    
        
