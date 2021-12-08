

#reader(lib "htdp-advanced-reader.ss" "lang")((modname Assignment5_P3_v6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))




(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))



(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))


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
(define (insert widget db)
  (local [(define (db-lessthan? db bst)
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
(define (db-compare? db-fn db val)
  ((db-fn db) val ((db-field db) (bst-widget (db-bst db)))))



(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define BSTq (make-bst A1
                       (make-bst W1 false false)
                       (make-bst Z1 false false)))
(define BSTp (make-bst D1
                       (make-bst A1 false false)
                       (make-bst Z1 false false)))
(define BSTn (make-bst D1
                       (make-bst A1 false false)
                       (make-bst W1 false false)))
(define BSTq-test (make-bst A1
                            (make-bst W1 false false)
                            (make-bst Z1 false false)))
(define BSTq-2 (make-bst A1
                         (make-bst W1 false false)
                         (make-bst Z1 false false)))
(define BSTp-test (make-bst D1
                            (make-bst A1 false false)
                            (make-bst Z1 false false)))
(define BSTn-test (make-bst D1
                            (make-bst A1 false false)
                            (make-bst W1 false false)))


(define A2 (make-widget "A2" 20 1)) 
(define B2 (make-widget "B2" 10 1))
(define C2 (make-widget "C2" 50 1))
(define D2 (make-widget "D2" 40 1))
(define E2 (make-widget "E2" 60 1))
(define F2 (make-widget "F2" 30 1)) 



(define BSTrl-1 
  (make-bst A2
            (make-bst B2 false false)
            (make-bst C2
                      (make-bst D2 (make-bst F2 false false) false)
                      (make-bst E2
                                false
                                false))))
(define BSTrl-1b 
  (make-bst
   D2
   (make-bst A2
             (make-bst B2 false false)
             (make-bst F2 false false))
   (make-bst C2
             false
             (make-bst E2 false false))))

(define BSTrl-2
  (make-bst A2
            false
            (make-bst C2
                      (make-bst D2 false false)
                      false)))
(define BSTrl-2b
  (make-bst
   D2
   (make-bst A2 false false)
   (make-bst C2 false false)))


(define BSTrr-1
  (make-bst B2
            false
            (make-bst A2
                      false
                      (make-bst F2 false false))))
(define BSTrr-1b
  (make-bst
   A2
   (make-bst B2 false false)
   (make-bst F2 false false)))

(define BSTrr-2
  (make-bst B2
            false
            (make-bst A2
                      false
                      (make-bst F2
                                false
                                (make-bst D2 false
                                          (make-bst C2 false false))))))
(define BSTrr-2b
  (make-bst
   F2
   (make-bst
    B2
    false
    (make-bst A2 false false))
   (make-bst
    D2
    false
    (make-bst C2 false false))))

(define BSTrr-2-test
  (make-bst B2
            false
            (make-bst A2
                      false
                      (make-bst F2
                                false
                                (make-bst D2 false
                                          (make-bst C2 false false))))))


(define BSTlr-1
  (make-bst D2
            (make-bst A2
                      false
                      (make-bst F2 false false))
            false))
(define BSTlr-1b
  (make-bst
   F2
   (make-bst A2 false false)
   (make-bst D2 false false)))

(define BSTlr-2
  (make-bst E2
            (make-bst F2
                      (make-bst A2 false false)
                      (make-bst D2
                                false
                                (make-bst C2 false false)))
            false))
(define BSTlr-2b
  (make-bst
   D2
   (make-bst
    F2
    (make-bst A2 false false) false)
   (make-bst E2 false false)))
                      


(define BSTll-1
  (make-bst C2
            (make-bst F2
                      (make-bst A2
                                (make-bst B2 false false)
                                false)
                      (make-bst D2 false false))
            (make-bst E2 false false)))
(define BSTll-1-2
  (make-bst C2
            (make-bst F2
                      (make-bst A2
                                (make-bst B2 false false)
                                false)
                      (make-bst D2 false false))
            (make-bst E2 false false)))
(define BSTll-1b
  (make-bst
   F2
   (make-bst
    A2
    (make-bst B2 false false) false)
   (make-bst
    C2
    (make-bst D2 false false)
    (make-bst E2 false false))))

(define BSTll-2
  (make-bst F2
            (make-bst A2
                      (make-bst B2 false false)
                      false)
            false))
(define BSTll-2b
  (make-bst
   A2
   (make-bst B2 false false)
   (make-bst F2 false false)))


(define DB-quantity (make-db widget-quantity < = BSTq))
(define DB-quantity-2 (make-db widget-quantity < = BSTq-2))
(define DB-name (make-db widget-name string<? string=? BSTn))
(define DB-price (make-db widget-price < = BSTp))
(define DB-empty (make-db widget-price < = false))
(define DB-empty-2 (make-db widget-price < = false))
(define DB-rl (make-db widget-quantity < = BSTrl-1))
(define DB-lr (make-db widget-quantity < = BSTlr-1))
(define DB-rr (make-db widget-quantity < = BSTrr-2))
(define DB-ll (make-db widget-quantity < = BSTll-1))
(define DB-ll-2 (make-db widget-quantity < = BSTll-1-2))


(define DB-quantity-test (make-db widget-quantity < = BSTq-test))
(define DB-name-test (make-db widget-name string<? string=? BSTn-test))
(define DB-price-test (make-db widget-price < = BSTp-test))
(define DB-empty-test (make-db widget-price < = false))
(define DB-quantity-test-2 (make-db widget-quantity < = BSTrr-2-test))





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








(check-expect (db-bst (insert! Z1 DB-name-test))
              (make-bst
               D1
               (make-bst A1 false false)
               (make-bst W1
                         false
                         (make-bst Z1 false false))))

(check-expect (db-bst (insert! D1 DB-quantity-test))
              (make-bst
               A1
               (make-bst W1 false false)
               (make-bst Z1
                         (make-bst D1 false false)
                         false)))

(check-expect (db-bst (insert! Z1 DB-quantity-test-2))
              (make-bst
               B2 false
               (make-bst
                A2 false
                (make-bst
                 F2 false
                 (make-bst
                  D2 false
                  (make-bst
                   C2 false
                   (make-bst
                    Z1 false false)))))))

(check-expect (db-bst (insert! Z1 DB-empty-test))
              (make-bst Z1 false false))

 

(define (insert! widget db) 
  (local [(define new-node (make-bst widget false false))
          (define (insert-bst! bst parent) 
            (cond 
              [(and (false? bst) (false? parent)) (set-db-bst! db new-node)]
              [(false? bst)
               (if ((db-lt? db) ((db-field db) widget)
                                ((db-field db) (bst-widget parent)))
                   
                   (set-bst-left! parent new-node)
                   
                   (set-bst-right! parent new-node))] 
              [((db-lt? db) ((db-field db) widget)
                            ((db-field db) (bst-widget bst)))
               (insert-bst! (bst-left bst) bst)]
              [else
               (insert-bst! (bst-right bst) bst)]))]
    (begin
      (insert-bst! (db-bst db) (db-bst db))
      db)))







(check-expect (db-bst (insert-avl D1 DB-quantity))
              (make-bst
               A1
               (make-bst W1 false false)
               (make-bst Z1 (make-bst D1 false false) false)))

(check-expect 
 (db-bst (insert-avl D1 DB-rr))
 (make-bst
  F2
  (make-bst
   B2
   (make-bst D1 false false)
   (make-bst A2 false false))
  (make-bst
   D2
   false
   (make-bst
    C2 false false))))

(check-expect
 (db-bst (insert-avl A1 DB-rl))
 (make-bst
  A2
  (make-bst B2
            (make-bst A1 false false) false)
  (make-bst
   C2
   (make-bst D2 (make-bst F2 false false) false)
   (make-bst E2 false false))))

(check-expect
 (db-bst (insert-avl Z1 DB-lr))
 (make-bst
  D2
  (make-bst A2 false
            (make-bst F2 false false))
  (make-bst Z1 false false)))

(check-expect
 (db-bst (insert-avl A1 DB-ll))
 (make-bst
  F2
  (make-bst B2
            (make-bst A1 false false)
            (make-bst A2 false false))
  (make-bst C2
            (make-bst D2 false false)
            (make-bst E2 false false))))
                          

(check-expect (db-bst (insert-avl A1 DB-empty))
              (make-bst A1 false false))


  

(define (insert-avl widget db)
  
  
  (local [(define (insert bst)
            (cond [(false? bst) (make-bst widget false false)]
        
                  [((db-lt? db) ((db-field db) widget)
                                ((db-field db) (bst-widget bst)))
                   (make-bst
                    (bst-widget bst)
                    (balance (insert (bst-left bst)))
                    (bst-right bst))]
        
                  [else
                   (make-bst
                    (bst-widget bst)
                    (bst-left bst)
                    (balance (insert (bst-right bst))))]))]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (balance (insert (db-bst db))))))







(check-expect (db-bst (insert-avl! A1 DB-empty-2))
              (make-bst A1 false false))


(check-expect (db-bst (insert-avl! D1 DB-quantity-2))
              (make-bst
               A1
               (make-bst W1 false false)
               (make-bst Z1 (make-bst D1 false false) false)))

(check-expect
 (db-bst (insert-avl! A1 DB-ll-2))
 (make-bst
  F2
  (make-bst A2
            (make-bst B2 (make-bst A1 false false) false)
            false)
  (make-bst C2
            (make-bst D2 false false)
            (make-bst E2 false false))))

  

(define (insert-avl! widget db)
  (local [(define bst (db-bst (insert widget db)))]
    (begin
      (set-db-bst! db (balance bst))
      db)))








(check-expect (balance false) false)

(check-expect (balance BSTp) BSTp)

(check-expect (balance BSTn) BSTn)

(check-expect (balance BSTrr-1) BSTrr-1b)

(check-expect (balance BSTrr-2) BSTrr-2b)

(check-expect (balance BSTrl-1) BSTrl-1b)

(check-expect (balance BSTrl-2) BSTrl-2b)

(check-expect (balance BSTll-1) BSTll-1b)

(check-expect (balance BSTll-2) BSTll-2b)

(check-expect (balance BSTlr-1) BSTlr-1b)

(check-expect (balance BSTlr-2) BSTlr-2b) 

  

(define (balance bst) 
  (local [(define (call-balance bst)
            (cond
              [(<= (height-diff bst) -2) (balance bst (bst-right bst))]
              [(>= (height-diff bst) 2) (balance bst (bst-left bst))]
              [else bst]))
          (define (check-bst bst) 
            (if (false? bst)
                (make-bst false false false)
                bst))
          (define (balance grandparent parent)
            (if (negative? (height-diff grandparent)) 
                (if (negative? (height-diff parent)) 
                    
                    (call-balance
                     (make-bst
                      (bst-widget grandparent)
                      (bst-left grandparent)
                      (make-bst (bst-widget (bst-right parent))
                                (make-bst (bst-widget parent)
                                          (bst-left parent)
                                          false)
                                (bst-right (bst-right parent))))) 
                    
                    
                    (call-balance
                     (make-bst 
                      (bst-widget (bst-left parent))
                      (make-bst (bst-widget grandparent)
                                (bst-left grandparent)
                                (bst-left (bst-left parent))) 
                      (make-bst (bst-widget parent)
                                false
                                (bst-right parent)))))
                (if (positive? (height-diff parent)) 
                    
                    (call-balance
                     (make-bst
                      (bst-widget parent)
                      (make-bst (bst-widget (bst-left parent))
                                (bst-left (check-bst (bst-left parent)))
                                (bst-right (bst-left parent))) 
                      (make-bst (bst-widget grandparent)
                                (bst-right parent)
                                (bst-right grandparent))))

                    
                    (call-balance
                     (make-bst
                      (bst-widget (bst-right parent)) 
                      (make-bst (bst-widget parent)
                                (bst-left parent)
                                (bst-left (bst-right parent)))
                      (make-bst (bst-widget grandparent)
                                false
                                (bst-right grandparent)))))))]
    (call-balance bst)))









(check-expect (db-bst (generate-tree insert (list A1 A2 Z1) DB-empty))
              (make-bst A1
                        (make-bst A2 false false)
                        (make-bst Z1 false false)))

(check-expect (db-bst (generate-tree insert-avl (list A1 A2 Z1) DB-empty))
              (make-bst A1
                        (make-bst A2 false false)
                        (make-bst Z1 false false)))             

(check-expect (db-bst (generate-tree insert (list) DB-empty)) false)

  

(define (generate-tree fn low db)
  (local [(define (ins widget db-) (fn widget db-))
          (define (ins-low low db-)
            (cond
              [(empty? low) db-]
              [else (ins-low (rest low) (ins (first low) db-))]))]
    (ins-low low db)))



(define low-i (random-widgets 250000 1000000))

(define low-f (random-widgets 10000 100000))

(define get-number (λ(n) (string->number (widget-name n))))

(define DB-init (make-db get-number < = false))

(define DB-AVL-f (generate-tree insert-avl low-f DB-init))

(define DB-f (generate-tree insert! low-f DB-init))

(define BST-low-f (db-bst DB-f))

(define BST-AVL-f (db-bst DB-AVL-f))







(check-expect (db-bst (time-insert insert (list A1 A2 Z1)))
              (make-bst A1
                        false
                        (make-bst A2 false
                                  (make-bst Z1 false false))))

(check-expect (db-bst (time-insert insert-avl (list A1 A2 Z1)))
              (make-bst A2
                        (make-bst A1 false false)
                        (make-bst Z1 false false)))             

(check-expect (db-bst (time-insert insert (list))) false)

  

(define (time-insert fn low)
  (local [(define DB (make-db widget-quantity < = false))]
    (generate-tree fn low DB)))









  

(define (time-find tree)
  (local [(define DB (make-db get-number < = tree))
          (define (findoperation widget)
            (find (get-number widget) DB))]
    (map findoperation low-f)))
  


(begin
  (time (time-insert insert! low-i))
  "^ run time for insert!")
(begin
  (time (time-insert insert low-i))
  "^ run time for insert")
(begin
  (time (time-find BST-low-f))
  "^ run time for find in a non-AVL BST")
(begin
  (time (time-find BST-AVL-f))
  "^ run time for find in an AVL BST") 