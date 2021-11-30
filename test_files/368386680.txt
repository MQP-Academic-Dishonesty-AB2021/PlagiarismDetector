

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))








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






(define (insert-avl w db)
  (local
    [(define b (db-bst db))

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
            [(smaller? w bst)
             (cond
               [(false? (bst-left bst))
                (set! bst ((bst-widget bst) (bst-left (make-bst w false false)) (bst-right bst)))]
               [else
                (set! bst ((bst-widget bst) (bst-left (make-bst w false false)) (bst-right bst)))
                (balance bst)
                (set! bst ((bst-widget bst) (bst-left insert-w w (bst-left bst)) (bst-right bst)))])]
            [else
             (cond
               [(false? (bst-right bst))
                (set! bst ((bst-widget bst) (bst-right (make-bst w false false)) (bst-right bst)))]
               [else
                (set! bst ((bst-widget bst) (bst-right (make-bst w false false)) (bst-right bst)))
                (balance bst)
                (set! bst ((bst-widget bst) (bst-right bst)(bst-right insert-w w (bst-right bst))))])]))


     (define (insert-db w db)
       (cond
         [(false? b)
          (set! db ((db-field) (db-lt?) (db-eq?) (make-bst w false false)))]
         [else
          (set! db ((db-field) (db-lt?) (db-eq?) (insert w (db-bst db))))]))]
    
    (insert-db w db)))





(define (balance bst)
  (cond
    [(= 0 (height-diff bst))
     bst]
    [(< 0 (height-diff bst))
     (...)]
    [(> 0 (height-diff bst))
     (...)]))
  

