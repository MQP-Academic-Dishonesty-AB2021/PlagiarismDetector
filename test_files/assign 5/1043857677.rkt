

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3-1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


(require 2htdp/image)



(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define L1 (make-widget "LL23" 6 10))
(define B1 (make-widget "B" 17 12)) 
(define C1 (make-widget "C12" 21 44))

(define-struct bst (widget left right))





(define BST1 false)
(define BST2 (make-bst Z1 false false))
(define BST3 (make-bst W1 false BST2))
(define BST4 (make-bst D1 false false))
(define BST5 (make-bst L1 BST4 BST3))


(define BST6 (make-bst L1
                       (make-bst D1
                                 (make-bst W1 false false)
                                 false)
                       (make-bst Z1 false false))) 
(define BST7 (make-bst L1
                       (make-bst D1 false false)
                       (make-bst W1 false
                                    (make-bst Z1 false false)))) 
(define BST8 (make-bst Z1
                       (make-bst L1
                                 (make-bst A1
                                           (make-bst W1 false false)
                                           false)
                                 (make-bst B1 false false))
                       (make-bst C1 false false))) 


(define AVL1 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst W1 false false))) 
(define AVL2 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst W1 false
                                    (make-bst Z1 false false)))) 
(define AVL3 (make-bst D1
                       false
                       (make-bst W1 false false))) 
(define NOT-AVL1 (make-bst L1
                           (make-bst D1
                                     (make-bst A1 false false)
                                     false)
                           false)) 




(define bsta (make-bst 1 false false))
(define bstb (make-bst 2 bsta false))
(define bstc (make-bst 3 bstb false))
(define bstd (make-bst 2
                       bsta
                       (make-bst 3 false false)))

(define bste (make-bst 2 false
                       (make-bst 3 false false)))
(define bstf (make-bst 4 bste false))
(define bstg (make-bst 4
                       (make-bst 3
                                 (make-bst 2 false false)
                                 false)
                       false))
(define bsth (make-bst 3
                       (make-bst 2 false false)
                       (make-bst 4 false false)))
(define bsti (make-bst 7 false
                         (make-bst 12 false
                                      (make-bst 19 false false))))
(define bstj (make-bst 7 false
                         (make-bst 12
                                   (make-bst 8 false false) false)))
(define bstk (make-bst 11
                       (make-bst 4 false false)
                       (make-bst 13 false
                                    (make-bst 17
                                              (make-bst 14 false false)
                                              (make-bst 21 false false)))))

(define-struct db (field lt? eq? bst))












(define DB-quantity (make-db widget-quantity < = false))
(define DB-name (make-db widget-name string<? string=? false))

(define db1 (make-db widget-name string<? equal? BST7)) 
(define db2 (make-db widget-quantity < = BST6))
(define db3 (make-db widget-quantity < = false))

(define db4 (make-db widget-name string<? equal? AVL1))
(define db5 (make-db widget-name string<? equal? AVL2))
(define db6 (make-db widget-name string<? equal? NOT-AVL1))
(define db7 (make-db widget-name string<? equal? AVL3))
(define db8 (make-db widget-price < = BST8))






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
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))









(check-expect (db-bst (insert! W1 DB-name)) (make-bst W1 false false)) 
(check-expect (db-bst (insert! W1 DB-quantity)) (make-bst W1 false false)) 
(check-expect (db-bst (insert! A1 db2))
              (make-bst L1
                        (make-bst D1
                                  (make-bst W1 false
                                               (make-bst A1 false false))
                                  false)
                        (make-bst Z1 false false))) 
(check-expect (db-bst (insert! C1 db1))
              (make-bst L1
                        (make-bst D1 (make-bst C1 false false)
                                     false)
                        (make-bst W1 false
                                     (make-bst Z1 false false)))) 

(define (insert! widget db)
                
                
  (local [(define (insert-ins widget bst previous left?) 
                                                         
            
            (cond [(false? bst)                          
                   (if left?
                       
                       
                       (set-bst-left! previous (make-bst widget false false))
                       (set-bst-right! previous (make-bst widget false false)))]
                  
                  [else
                   (if ((db-lt? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst)))
                       (insert-ins widget (bst-left bst) bst true)
                       (insert-ins widget (bst-right bst) bst false))]))]

    
    (if (false? (db-bst db)) 
        
        (begin (set-db-bst! db (make-bst widget false false))
               db)
        
        (begin (insert-ins widget (db-bst db) false false)
               db))))









(check-expect (db-bst (insert-avl Z1 db4))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1 false
                                     (make-bst Z1 false false))))

(check-expect (db-bst (insert-avl C1 db5))
              (make-bst D1
                        (make-bst A1 false
                                     (make-bst C1 false false))
                        (make-bst W1 false
                                     (make-bst Z1 false false))))

(check-expect (db-bst (insert-avl Z1 db6))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst L1 false
                                     (make-bst Z1 false false))))

(check-expect (db-bst (insert-avl Z1 db7))
              (make-bst W1
                        (make-bst D1 false false)
                        (make-bst Z1 false false)))

(check-expect (db-bst (insert-avl D1 db8))
              (make-bst L1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  (make-bst D1 false false))
                        (make-bst Z1
                                  (make-bst B1 false false)
                                  (make-bst C1 false false))))


(define (insert-avl widget db)
         
  (local [(define (update-db-bst db bst)
            (make-db (db-field db)
                     (db-lt? db)
                     (db-eq? db)
                     bst))
          
          (define (insert-ins widget bst) 
            (cond [(false? bst) (make-bst widget false false)] 
                  [else
                   (if ((db-lt? db) ((db-field db) widget)
                       ((db-field db) (bst-widget bst))) 
                       
                       (balance (make-bst (bst-widget bst)
                                 (insert-ins widget (balance (bst-left bst)))
                                 (bst-right bst)))
                       
                       (balance (make-bst (bst-widget bst)
                                 (bst-left bst)
                                 (insert-ins widget (balance (bst-right bst))))))]))]
    
    
    (update-db-bst db (insert-ins widget (balance (db-bst db))))))







(check-expect (balance BST1) BST1) 
(check-expect (balance BST2) BST2) 
(check-expect (balance AVL1) AVL1) 
(check-expect (balance AVL2) AVL2) 
(check-expect (balance NOT-AVL1) (make-bst D1
                                           (make-bst A1 false false)
                                           (make-bst L1 false false))) 
(check-expect (balance bstk)
              (make-bst 13
                        (make-bst 11 (make-bst 4 false false) false)
                        (make-bst 17 (make-bst 14 false false)
                                  (make-bst 21 false false)))) 

(define (balance bst)
  (local
    [(define h-diff (height-diff bst)) 
     
     (define (left-left bst)
       (local
         [(define new-root (bst-left bst))] 
         (make-bst
          (bst-widget new-root)
          (bst-left new-root)
          (make-bst
           (bst-widget bst)
           (bst-right (bst-left bst))
           (bst-right bst)))))

     
     (define (left-right bst)
       (local
         [(define new-bst (make-bst (bst-widget bst)
                                    (make-bst
                                     (bst-widget (bst-right (bst-left bst)))
                                     (make-bst
                                      (bst-widget (bst-left bst))
                                      (bst-left (bst-left bst))
                                      false)
                                     (bst-right (bst-right (bst-left bst))))
                                    (bst-right bst)))]
         (left-left new-bst)))

     
     (define (right-right bst)
       (local
         [(define new-root (bst-right bst))] 
         (make-bst
          (bst-widget new-root)
          (make-bst
           (bst-widget bst)
           (bst-left bst)
           (bst-left (bst-right bst)))
          (bst-right new-root))))

     
     (define (right-left bst)
       (local
         [(define new-bst (make-bst (bst-widget bst)
                                    (bst-left bst)
                                    (make-bst
                                     (bst-widget (bst-left (bst-right bst)))
                                     (bst-left (bst-left (bst-right bst)))
                                     (make-bst
                                      (bst-widget (bst-right bst))
                                      false
                                      (bst-right (bst-right bst))))))]
         (right-right new-bst)))]

    
    (cond
      [(< (abs h-diff) 2) bst]
      [(> h-diff 1)
       (if (< (height-diff (bst-left bst)) 0)
           (left-right bst)
           (left-left bst))]
      [else
       (if (> (height-diff (bst-right bst)) 0)
           (right-left bst)
           (right-right bst))])))









(define (find key db)
  (local
    [(define (find key bst) 
       (cond [(false? bst) false]
             
             [((db-lt? db) key ((db-field db) (bst-widget bst)))
              (find key (bst-left bst))]
             
             [((db-eq? db) key ((db-field db) (bst-widget bst)))
              (bst-widget bst)]
             
             [else
              (find key (bst-right bst))]))]
    
    
    (find key (db-bst db))))

(define (insert widget db)
         
  (local [(define (update-db-bst db bst)
            (make-db (db-field db) (db-lt? db) (db-eq? db) bst))
          
          (define (insert-ins widget bst) 
            (cond [(false? bst) (make-bst widget false false)] 
                  [else
                   (if ((db-lt? db) ((db-field db) widget)
                       ((db-field db) (bst-widget bst))) 
                       
                       (make-bst (bst-widget bst)
                                 (insert-ins widget (bst-left bst))
                                 (bst-right bst))
                       
                       (make-bst (bst-widget bst)
                                 (bst-left bst)
                                 (insert-ins widget (bst-right bst))))]))]
    
    
    (update-db-bst db (insert-ins widget (db-bst db)))))








(define (create-db fn low db)
  (local
    [(define (create-bst low db)
       (cond
         [(empty? low) db]
         [else
          (create-bst (rest low) (fn (first low) db))]))]
    (create-bst low db)))











 
(define (time-insert)
  (local [(define random-list-250k (random-widgets 250000 250000)) 
          (define test-db (make-db widget-quantity < = false))]    

   (begin (time (create-db insert random-list-250k test-db))
          (time (create-db insert! random-list-250k test-db))
          "Test Complete")))















(define (time-find)
  (local [(define random-list-10k (random-widgets 10000 10000)) 
          (define test-db (make-db widget-quantity < = false)) 
          (define random-list-10k-keys (map (db-field test-db) random-list-10k)) 

          
          (define test-db-bst (create-db insert! random-list-10k test-db))
          
          
          (define test-db-avl (create-db insert-avl random-list-10k test-db))]

   (begin (time (foldr (λ (key dummy) (find key test-db-bst))
                       (first random-list-10k-keys)
                       (rest random-list-10k-keys)))
          (time (foldr (λ (key dummy) (find key test-db-avl))
                       (first random-list-10k-keys)
                       (rest random-list-10k-keys)))
          "Test Complete")))