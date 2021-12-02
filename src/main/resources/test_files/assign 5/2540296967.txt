

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

(define AVL1 (make-bst A1
                       (make-bst Z1
                                 false
                                 false)
                       false))
(define AVL2 (make-bst Z1
                       (make-bst W1 false false)
                       false))

(define-struct db (field lt? eq? bst))

(define DB-price (make-db widget-price < = TREE1))    

(define DB-quantity (make-db widget-quantity < = TREE2))    

(define DB-name (make-db widget-name string<? string=? TREE3))    

(define DB-quantity-avl (make-db widget-quantity < = AVL1))
(define DB-quantity-avl2 (make-db widget-quantity < = AVL2))




(check-expect (db-bst (insert! W1 DB-quantity))  
              (make-bst Z1 
                        (make-bst A1 
                                  (make-bst W1 false false)
                                  (make-bst D1 false false)) false))
(check-expect (db-bst (insert! C1 DB-price)) 
              (make-bst W1 
                        (make-bst D1 (make-bst A1 false false) false) 
                        (make-bst Z1 (make-bst C1 false false) false)))
(check-expect (db-bst (insert! A1 DB-name)) 
              (make-bst A1 
                        false 
                        (make-bst D1 
                                  false 
                                  (make-bst W1 
                                            false 
                                            (make-bst Z1 false false)))))


(define (insert! widget db)
  (local
    [(define eqfn (db-eq? db))
     (define smfn (db-lt? db))
     (define dbfield (db-field db))
     (define (inner widget bst)
       (cond
         [(false? bst) (make-bst widget false false)]
         [(eqfn (dbfield widget) (dbfield (bst-widget bst))) bst]
         [(smfn (dbfield widget) (dbfield (bst-widget bst)))
          (begin
            (set-bst-left! bst (inner widget (bst-left bst)))
            bst)
          ]
         [(not (smfn (dbfield widget) (dbfield (bst-widget bst))))
          (begin
            (set-bst-right! bst (inner widget (bst-right bst)))
            bst)]))]
    (begin
      (set-db-bst! db (inner widget (db-bst db)))
      db)))


(check-expect (db-bst (insert-avl W1 DB-quantity-avl))
              (make-bst Z1
                        (make-bst W1 false false)
                        (make-bst A1 false false)))

(check-expect (db-bst (insert-avl C1 DB-quantity-avl2))
              (make-bst W1
                        (make-bst C1 false false)
                        (make-bst Z1 false false)))

(check-expect (db-bst (insert-avl Z1 DB-quantity-avl2)) (db-bst DB-quantity-avl2))



(define (insert-avl widget db)
  (local [
          (define eqfn (db-eq? db))
          (define smfn (db-lt? db))
          (define dbfield (db-field db))
          (define (inner widget bst)
            (cond
              [(false? bst) (make-bst widget false false)]
              [(eqfn (dbfield widget) (dbfield (bst-widget bst))) bst]
              [(smfn (dbfield widget) (dbfield (bst-widget bst)))
               (balance (make-bst
                         (bst-widget bst)
                         (inner widget (bst-left bst))
                         (bst-right bst)))]
              [(not (smfn (dbfield widget) (dbfield (bst-widget bst))))
               (balance (make-bst
                         (bst-widget bst)
                         (bst-left bst)
                         (inner widget (bst-right bst))))]))]
    (make-db dbfield smfn eqfn (inner widget (db-bst db)))))


(check-expect (balance (make-bst A1
                                 (make-bst Z1
                                           (make-bst W1 false false)
                                           false)
                                 false))
              (make-bst Z1
                        (make-bst W1 false false)
                        (make-bst A1 false false)))

(check-expect (balance (make-bst A1
                                 (make-bst Z1
                                           false
                                           (make-bst W1 false false))
                                 false))
              (make-bst Z1
                        (make-bst W1 false false)
                        (make-bst A1 false false)))

(check-expect (balance (make-bst A1
                                 false
                                 (make-bst Z1
                                           false
                                           (make-bst W1 false false))))
              (make-bst Z1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))

(check-expect (balance (make-bst A1
                                 false
                                 (make-bst Z1
                                           (make-bst W1 false false)
                                           false)))
              (make-bst Z1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))



(define (balance bst)
  (local
    [(define (left-left-rot bst)
       (make-bst (bst-widget (bst-left bst)) (make-bst (bst-widget (bst-left (bst-left bst))) false false) (make-bst (bst-widget bst) false false)))
     (define (left-right-rot bst)
       (make-bst (bst-widget (bst-left bst)) (make-bst (bst-widget (bst-right (bst-left bst))) false false) (make-bst (bst-widget bst) false false)))
     (define (right-right-rot bst)
       (make-bst (bst-widget (bst-right bst)) (make-bst (bst-widget bst) false false) (make-bst (bst-widget (bst-right (bst-right bst))) false false)))
     (define (right-left-rot bst)
       (make-bst (bst-widget (bst-right bst)) (make-bst (bst-widget bst) false false) (make-bst (bst-widget (bst-left (bst-right bst))) false false)))]
    (cond 
      [(= (height-diff bst) 2)
       (if (false? (bst-left (bst-left bst)))
           (left-right-rot bst)
           (left-left-rot bst))]
      [(= (height-diff bst) -2)
       (if (false? (bst-right (bst-right bst)))
           (right-left-rot bst)
           (right-right-rot bst))]
      [else bst])))


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


(define (random-widgets num max)
  (build-list num
              (lambda(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))


(define (build-tree low)
  (foldr insert-name false low))


(define (insert-name widget bst)
  (cond
    [(false? bst) (make-bst widget false false)]
    [(smaller? (widget-name widget) bst)
     (make-bst
      (bst-widget bst)
      (insert-name widget (bst-left bst))
      (bst-right bst))]
    [(not (smaller? (widget-name widget) bst))
     (make-bst
      (bst-widget bst)
      (bst-left bst)
      (insert-name widget (bst-right bst)))]
    [(same? (widget-name widget) bst) bst]))


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


(define (smaller? key tree) 
  (string<? key (widget-name (bst-widget tree))))


(define (same? key tree) 
  (string=? key (widget-name (bst-widget tree))))



(define (time-find)
  (local
    [(define DATABASE (make-db widget-name string<? string=? (build-tree (random-widgets 10000 99999999))))
     (define (find-helper widget top bst)
       (begin
         (find (widget-name widget) top)
         (cond
           [(false? bst) false]
           [else
            (begin
              (if (not (false? (bst-left bst)))
                  (find-helper (bst-widget bst) top (bst-left bst)) false)
              (if (not (false? (bst-right bst)))
                  (find-helper (bst-widget bst) top (bst-right bst)) false))])))
     (define (find-helper-avl widget top bst)
       (begin
         (find (widget-name widget) top)
         (cond
           [(false? bst) false]
           [else
            (begin
              (if (not (false? (bst-left bst)))
                  (find-helper (bst-widget bst) top (balance (bst-left bst))) false)
              (if (not (false? (bst-right bst)))
                  (find-helper (bst-widget bst) top (balance (bst-right bst))) false))])))]
    (begin 
      (time (find-helper (bst-widget (db-bst DATABASE)) DATABASE (db-bst DATABASE)))
      (time (find-helper-avl (bst-widget (db-bst DATABASE)) DATABASE (db-bst DATABASE)))
      "Done")))



(define (time-insert)
  (local [
          (define low (random-widgets 250000 999999))
          (define (helper low db ins)
            (cond
              [(empty? low) false]
              [else
               (helper (rest low) (ins (first low) db) ins)]))
          ]
    (begin (time (helper low (make-db widget-name string<? string=? (make-bst (first low) false false)) insert))
           (time (helper low (make-db widget-name string<? string=? (make-bst (first low) false false)) insert))
           "Done")))