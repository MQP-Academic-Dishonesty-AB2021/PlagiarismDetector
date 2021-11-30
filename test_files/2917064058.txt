

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3-1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


(require 2htdp/image)

(define-struct widget (name quantity price))


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 6 9))
(define F1 (make-widget "F1" 0 4))

(define-struct bst (widget left right))



(define BST1 (make-bst W1 false
                       (make-bst A1 false
                                 (make-bst Z1 false false))))
(define BST2 (make-bst D1 false false))
(define BST3 (make-bst A1 false (make-bst D1 false false)))
(define BST4 (make-bst Z1 (make-bst A1 false false) false))
(define BST5 (make-bst D1 (make-bst F1
                                    (make-bst W1 false false)
                                    false)
                       (make-bst Z1 false false)))
(define BST6 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst F1 false
                                 (make-bst W1 false false))))
(define BST7 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst E1 false
                                 (make-bst W1 false false))))
(define BST8 (make-bst D1
                       (make-bst F1
                                 (make-bst A1 false false)
                                 false)
                       (make-bst Z1 false false)))


(define-struct db (field lt? eq? bst))






(define DB-quantity-false (make-db widget-quantity < = false))
(define DB-quantity (make-db widget-quantity < = BST1))
(define DB-name (make-db widget-name string<? string=? BST4))
(define DB-price (make-db widget-price < = BST5))
(define DB-name2 (make-db widget-name string<? string=? BST6))
(define DB-name3 (make-db widget-name string<? string=? BST7))
(define DB-price2 (make-db widget-price < = BST8))








(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))




(define (build-tree low)
  (foldr insert-name false low))



(check-expect (db-bst (insert! D1 DB-quantity))          
              (make-bst W1 false (make-bst A1 false
                                           (make-bst Z1 (make-bst D1 false false)
                                                     false))))
(check-expect (db-bst (insert! A1 DB-quantity-false))    
              (make-bst A1 false false))
(check-expect (db-bst (insert! D1 DB-name))              
              (make-bst Z1 (make-bst A1 false
                                     (make-bst D1 false false))
                        false)) 



(define (insert! key db)
  (local
    [(define BST-DB (db-bst db))  

     (define lt (db-lt? db))      
     (define field (db-field db)) 

     
     (define (smaller? key bst0) 
       (lt (field key) (field (bst-widget bst0))))

     
     (define (set-bst bst direction) 
       (cond [(false? bst) (make-bst key false false)]
             [(zero? direction)
              (begin (set-bst-left! bst (make-bst key false false)) BST-DB)]
             [else
              (begin (set-bst-right! bst (make-bst key false false)) BST-DB)]))

     
     (define (create-db bst)      
       (make-db (db-field db)
                (db-lt? db)
                (db-eq? db)
                bst))
     
     
     
     (define (insert-inner key bst direction bst-before)
       (cond [(false? bst)                 
              (set-bst bst-before direction)]
             [(smaller? key bst)           
              (insert-inner key (bst-left bst) 0 bst)]         
             [else                         
              (insert-inner key (bst-right bst) 1 bst)]))]

    (create-db (insert-inner key BST-DB 0 BST-DB))))





(check-expect (db-bst (insert-avl A1 DB-price))              
              (make-bst D1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  (make-bst F1 false false))
                        (make-bst Z1 false false)))
(check-expect (db-bst (insert-avl Z1 DB-name2))              
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1
                                  (make-bst F1 false false)
                                  (make-bst Z1 false false))))
(check-expect (db-bst (insert-avl F1 DB-name3))              
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst F1
                                  (make-bst E1 false false)
                                  (make-bst W1 false false))))
(check-expect (db-bst (insert-avl W1 DB-price2))              
              (make-bst D1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  (make-bst F1 false false))
                        (make-bst Z1 false false)))


(define (insert-avl key db)
  (local
    [(define BST-DB (db-bst db))  

     (define lt (db-lt? db))      
     (define field (db-field db)) 

     (define (smaller? key bst0)  
       (lt (field key) (field (bst-widget bst0))))

     (define (create-db bst)      
       (make-db (db-field db)
                (db-lt? db)
                (db-eq? db)
                bst))
     
     
     (define (insert-inner key bst)
       (cond [(false? bst)                 
              (make-bst key false false)]
             [(smaller? key bst)           
              (make-bst (bst-widget bst)
                        (balance (insert-inner key (bst-left bst)))
                        (bst-right bst))]
             [else                         
              (make-bst (bst-widget bst)
                        (bst-left bst)
                        (balance (insert-inner key (bst-right bst))))]))]

    (create-db (insert-inner key BST-DB))))


(define AVL1 (make-bst Z1 (make-bst W1 (make-bst A1 false false) false)
                       false))
(define AVL2 (make-bst A1 false                                        
                       (make-bst W1 false (make-bst Z1 false false)))) 
(define AVL3 (make-bst A1 false                                        
                       (make-bst Z1 (make-bst W1 false false) false))) 
(define AVL4 (make-bst Z1 (make-bst A1 false                           
                                    (make-bst D1 false false))
                       false))
(define AVL5 (make-bst W1 (make-bst A1 false false)                    
                       (make-bst Z1 false false)))



(check-expect (balance AVL1) (make-bst W1 (make-bst A1 false false) 
                                       (make-bst Z1 false false)))
(check-expect (balance AVL2) (make-bst W1 (make-bst A1 false false) 
                                       (make-bst Z1 false false)))
(check-expect (balance AVL3) (make-bst W1 (make-bst A1 false false) 
                                       (make-bst Z1 false false)))
(check-expect (balance AVL4) (make-bst D1 (make-bst A1 false false) 
                                       (make-bst Z1 false false)))
(check-expect (balance AVL5) AVL5)                                  
(check-expect (balance (make-bst A1 false (make-bst D1 false (make-bst Z1 false false))))
              (make-bst D1 (make-bst A1 false false) (make-bst Z1 false false)))
                                                                    



(define (balance bst)
  (local
    
    [(define (right-left bst)
       (left (make-bst (bst-widget bst)
                       false
                       (make-bst (bst-widget (bst-left (bst-right bst)))
                                 false
                                 (make-bst (bst-widget (bst-right bst)) false false)))))
     
     (define (left bst)
       (make-bst (bst-widget (bst-right bst))
                 (make-bst (bst-widget bst) false false)
                 (bst-right (bst-right bst))))

     
     (define (left-right bst)
       (right (make-bst (bst-widget bst)
                        (make-bst (bst-widget (bst-right (bst-left bst)))
                                  (make-bst (bst-widget (bst-left bst)) false false)
                                  false)
                        false)))

     
     (define (right bst)
       (make-bst (bst-widget (bst-left bst))
                 (bst-left (bst-left bst))
                 (make-bst (bst-widget bst) false false)))]
      
    (cond [(> (height-diff bst) 1)                 
           (if (false? (bst-left (bst-left bst)))
               (left-right bst)
               (right bst))]
          [(< (height-diff bst) -1)                
           (if (false? (bst-right (bst-right bst)))
               (right-left bst)
               (left bst))]
          [else bst])))                            




(define (time-insert start)
  (local
    [(define test-list (random-widgets 250000 1000))
     (define (build-insert-name low)
       (local
         [(define BST false)]
         (foldr insert-name BST low)))
     (define (build-insert! low)
       (local
         [(define BST false)
          (define db (make-db widget-name string<? string=? BST))]
         (foldr insert! db low)))]
    (begin
      (time (build-insert-name test-list))
      (time (build-insert! test-list))
      "done")))




(define (time-find start)
  (local
    [(define test-list (random-widgets 10000 1000))

     (define (build-insert! low)
       (local
         [(define BST false)
          (define db (make-db widget-name string<=? string=? BST))]
         (foldr insert! db low)))

     (define (build-insert-avl low)
       (local
         [(define BST false)
          (define db (make-db widget-name string<=? string=? BST))]
         (foldr insert-avl db low)))

     (define dbBST (build-insert! test-list))

     (define dbAVL (build-insert-avl test-list))

     (define (find-bst low db)
       (map (λ (x) (find x db)) (map widget-name low)))]
    
    (begin
      (time (find-bst test-list dbBST))
      (time (find-bst test-list dbAVL))
      "done")))









(define (smaller? key bst)
  (string<? key (widget-name (bst-widget bst))))





(define (same? key bst)
  (string=? key (widget-name (bst-widget bst))))




(define (insert-name widget bst)
  (cond [(false? bst)                                 
         (make-bst widget false false)]
        [(smaller? (widget-name widget) bst)          
         (make-bst (bst-widget bst)
                   (insert-name widget (bst-left bst))
                   (bst-right bst))]
        [else                                         
         (make-bst (bst-widget bst)
                   (bst-left bst)
                   (insert-name widget (bst-right bst)))]))





(define (find key db)
  (local
    [(define BST-DB (db-bst db))  

     (define lt (db-lt? db))      
     (define eq (db-eq? db))      
     (define field (db-field db)) 

     (define (smaller? key bst0)  
       (lt key (field (bst-widget bst0))))
     
     (define (same? key bst0)     
       (eq key (field (bst-widget bst0))))
    
     (define (find-inner key bst)
       (cond [(false? bst) false]
             [(same? key bst)
              (bst-widget bst)]
             [(smaller? key bst)
              (find-inner key (bst-left bst))]
             [else
              (find-inner key (bst-right bst))]))]

    (find-inner key BST-DB)))





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





(define (render b)
  (local
    [
     (define TEXT-SIZE 20)    
     (define TEXT-COLOR1 "pink")
     (define TEXT-COLOR2 "orange")
     (define TAB 8)
     
     
     (define (blanks n)
       (list->string (build-list n (λ(x) #\ ))))
     
     
     (define (get-color d)
       (if (= (modulo d 2) 0)
           TEXT-COLOR1
           TEXT-COLOR2))
     
     
     (define (to-text side w d)
       (text  (string-append (blanks (* d TAB)) side (widget-name w))
              TEXT-SIZE
              (get-color d)))
     
     
     (define (render-helper b d img side)
       (if (false? b)
           img
           (above/align "left"
                        (to-text side (bst-widget b) d)
                        (render-helper (bst-left b) (+ d 1) img "L: ")
                        (render-helper (bst-right b) (+ d 1) img "R: "))))]
    (render-helper b 0 empty-image "T: ")))
