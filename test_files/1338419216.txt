

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))








(define-struct widget (name quantity price))






(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define E5 (make-widget "E5" 7 20))
(define G7 (make-widget "G7" 4 0))


(define-struct bst (widget left right))




(define BST-NAME-MAIN (make-bst A1
                                false
                                (make-bst W1
                                          (make-bst D1 false false)
                                          (make-bst Z1 false false))))
(define BST-QUANTITY-MAIN (make-bst A1
                                    (make-bst W1 false false)
                                    (make-bst Z1
                                              (make-bst D1 false false)
                                              false)))
(define BST-PRICE-MAIN (make-bst A1
                                 (make-bst W1 false false)
                                 (make-bst Z1
                                           (make-bst D1 false false)
                                           false)))


(define BST-NAME-UNBALANCED-1
  (make-bst D1
            false
            (make-bst E5
                      false
                      (make-bst G7 false false))))
(define BST-NAME-BALANCED-1
  (make-bst E5
            (make-bst D1 false false)
            (make-bst G7 false false)))


(define BST-NAME-UNBALANCED-2
  (make-bst D1
            (make-bst A1 false false)
            (make-bst W1
                      (make-bst G7
                                (make-bst E5 false false)
                                false)
                      (make-bst Z1 false false))))
(define BST-NAME-BALANCED-2
  (make-bst G7
            (make-bst D1
                      (make-bst A1 false false)
                      (make-bst E5 false false))
            (make-bst W1
                      false
                      (make-bst Z1 false false))))


(define BST-PRICE-UNBALANCED
  (make-bst Z1
            (make-bst D1
                      (make-bst A1
                                (make-bst W1 false false)
                                false)
                      false)
            (make-bst E5 false false)))
(define BST-PRICE-BALANCED
  (make-bst D1
            (make-bst A1
                      (make-bst W1 false false)
                      false)
            (make-bst Z1
                      false
                      (make-bst E5 false false))))


(define BST-QUANTITY-UNBALANCED
  (make-bst E5
            (make-bst A1
                      (make-bst W1 false false)
                      (make-bst G7
                                false
                                (make-bst D1 false false)))
            (make-bst Z1 false false)))
(define BST-QUANTITY-BALANCED
  (make-bst G7
            (make-bst A1
                      (make-bst W1 false false)
                      false)
            (make-bst E5
                      (make-bst D1 false false)
                      (make-bst Z1 false false))))


(define-struct db (field lt? eq? bst))










(define DB-NAME-FALSE (make-db widget-name string<? string=? false))
(define DB-NAME (make-db widget-name string<? string=? BST-NAME-MAIN))

(define DB-QUANTITY-FALSE (make-db widget-quantity < = false))
(define DB-QUANTITY (make-db widget-quantity < = BST-QUANTITY-MAIN))

(define DB-PRICE-FALSE (make-db widget-price < = false))
(define DB-PRICE (make-db widget-price < = BST-PRICE-MAIN))












(check-expect (insert-left? DB-PRICE E5 (db-bst DB-PRICE)) false)


(check-expect (insert-left? DB-NAME E5 (make-bst Z1 false false)) true)

(define (insert-left? db wid b)
  ((db-lt? db) ((db-field db) wid) ((db-field db) (bst-widget b))))






(check-expect (db-bst (insert! E5 DB-NAME))
              (make-bst A1
                        false
                        (make-bst W1
                                  (make-bst D1
                                            false
                                            (make-bst E5 false false))
                                  (make-bst Z1 false false))))


(check-expect (db-bst (insert! E5 DB-NAME-FALSE))
              (make-bst E5 false false))


(check-expect (db-bst (insert! E5 DB-QUANTITY))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            (make-bst E5 false false))
                                  false)))


(check-expect (db-bst (insert! E5 DB-QUANTITY-FALSE))
              (make-bst E5 false false))


(check-expect (db-bst (insert! E5 DB-PRICE))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1 false false)
                                  (make-bst E5 false false))))


(check-expect (db-bst (insert! E5 DB-PRICE-FALSE))
              (make-bst E5 false false))



(check-expect (db-bst (insert! G7 DB-PRICE))
              (make-bst A1
                        (make-bst W1
                                  (make-bst G7 false false)
                                  false)
                        (make-bst Z1
                                  (make-bst D1 false false)
                                  (make-bst E5 false false))))



(check-expect (db-bst (insert! G7 DB-PRICE-FALSE))
              (make-bst E5
                        (make-bst G7 false false)
                        false))

(define (insert! wid db)
  (local [(define bst (db-bst db))
          
          (define (insert! b parent)
            (cond
              [(false? b)
               (begin
                 (if (insert-left? db wid parent)
                     (set-bst-left! parent (make-bst wid false false))
                     (set-bst-right! parent (make-bst wid false false)))
                 bst)]
              [(insert-left? db wid b) (insert! (bst-left b) b)] 
              [else
               (insert! (bst-right b) b)]))]
    
    (if (false? bst)
        (begin
          (set-db-bst! db (make-bst wid false false))
          db)
        (make-db (db-field db)
                 (db-lt? db)
                 (db-eq? db)
                 (insert! bst false)))))







(check-expect 
 (begin
   (set! BST-NAME-MAIN (make-bst A1
                                 false
                                 (make-bst W1
                                           (make-bst D1 false false)
                                           (make-bst Z1 false false))))
   (set! BST-QUANTITY-MAIN (make-bst A1
                                     (make-bst W1 false false)
                                     (make-bst Z1
                                               (make-bst D1 false false)
                                               false)))
   (set! BST-PRICE-MAIN (make-bst A1
                                  (make-bst W1 false false)
                                  (make-bst Z1
                                            (make-bst D1 false false)
                                            false)))
   
   (set-db-bst! DB-NAME-FALSE false)
   (set-db-bst! DB-NAME BST-NAME-MAIN)
   
   (set-db-bst! DB-QUANTITY-FALSE false)
   (set-db-bst! DB-QUANTITY BST-QUANTITY-MAIN)
   
   (set-db-bst! DB-PRICE-FALSE false)
   (set-db-bst! DB-PRICE BST-PRICE-MAIN)
   
   true)
 true)









(check-expect (balance BST-NAME-UNBALANCED-1) BST-NAME-BALANCED-1)


(check-expect (balance BST-PRICE-UNBALANCED) BST-PRICE-BALANCED)


(check-expect (balance BST-QUANTITY-UNBALANCED) BST-QUANTITY-BALANCED)


(check-expect (balance BST-NAME-UNBALANCED-2) BST-NAME-BALANCED-2)


(check-expect (balance BST-PRICE-MAIN) BST-PRICE-MAIN)

(define (balance b)
  (if (<= (abs (height-diff b)) 1)
      b
      (local [(define right-heavy? (< (height-diff b) -1))
              
              (define child-right-heavy?
                (if right-heavy?
                    (negative? (height-diff (bst-right b)))
                    (negative? (height-diff (bst-left b)))))

              
              
              
              (define (rotate-right b)
                (make-bst (bst-widget (bst-left b))
                          (bst-left (bst-left b))
                          (make-bst (bst-widget b)
                                    (bst-right (bst-left b))
                                    (bst-right b))))

              
              
              
              (define (rotate-left b)
                (make-bst (bst-widget (bst-right b)) 
                          (make-bst (bst-widget b)
                                    (bst-left b)
                                    (bst-left (bst-right b)))
                          (bst-right (bst-right b))))]
        
        (cond
          
          
          [(and right-heavy? child-right-heavy?)
           (rotate-left b)]

          
          [(and (not right-heavy?) (not child-right-heavy?)) 
           (rotate-right b)]
    
          
          [(and (not right-heavy?) child-right-heavy?) 
           (rotate-right
            (make-bst (bst-widget b)
                      (rotate-left (bst-left b))
                      (bst-right b)))]
    
          
          [(and right-heavy? (not child-right-heavy?))
           (rotate-left
            (make-bst (bst-widget b)
                      (bst-left b)
                      (rotate-right (bst-right b))))]))))








(check-expect
 (db-bst (insert-avl Z1
                     (make-db widget-name string<? string=?
                              (make-bst E5
                                        (make-bst A1 false false)
                                        (make-bst G7
                                                  false
                                                  (make-bst W1 false false))))))
 (make-bst E5
           (make-bst A1 false false)
           (make-bst W1
                     (make-bst G7 false false)
                     (make-bst Z1 false false))))                            


(check-expect
 (db-bst (insert-avl G7 (make-db widget-price < = BST-PRICE-BALANCED)))
 (make-bst D1
           (make-bst W1
                     (make-bst G7 false false)
                     (make-bst A1 false false))
           (make-bst Z1
                     false
                     (make-bst E5 false false))))


(check-expect (db-bst (insert-avl E5 DB-QUANTITY))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst E5
                                  (make-bst D1 false false)
                                  (make-bst Z1 false false))))


(check-expect (db-bst (insert-avl Z1 (make-db widget-price < =
                                              (make-bst D1
                                                        false
                                                        (make-bst E5 false false)))))
              (make-bst Z1
                        (make-bst D1 false false)
                        (make-bst E5 false false)))


(check-expect
 (db-bst (insert-avl A1 (make-db widget-price < =
                                 (make-bst D1
                                           (make-bst W1 false false)
                                           (make-bst Z1
                                                     false
                                                     (make-bst E5 false false)))))) 
 (make-bst D1
           (make-bst W1
                     false
                     (make-bst A1 false false)) 
           (make-bst Z1
                     false
                     (make-bst E5 false false))))
              

(check-expect (db-bst (insert-avl E5 DB-QUANTITY-FALSE))
              (make-bst E5 false false))

(define (insert-avl wid db)
  (local [(define (insert-avl b)
            (cond
              [(false? b) (make-bst wid false false)]
              [(insert-left? db wid b)
               (make-bst (bst-widget b)
                         (balance (insert-avl (bst-left b)))
                         (bst-right b))]
              [else
               (make-bst (bst-widget b)
                         (bst-left b)
                         (balance (insert-avl (bst-right b))))]))]
    
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (balance (insert-avl (db-bst db))))))






(check-expect (height BST-QUANTITY-MAIN) 3)


(check-expect (height (make-bst W1 false false)) 1)


(check-expect (height (db-bst DB-PRICE-FALSE)) 0)

(define (height b)
  (local [(define (height-helper b d)
            (cond
              [(false? b) d]
              [else
               (max d
                    (height-helper (bst-left b) (add1 d))
                    (height-helper (bst-right b) (add1 d)))]))]
    
    (height-helper b 0)))










(check-expect (height-diff BST-QUANTITY-MAIN)
              (- (height (bst-left BST-QUANTITY-MAIN))
                 (height (bst-right BST-QUANTITY-MAIN))))


(check-expect (height-diff (make-bst W1 false false)) 0)


(check-expect (height-diff (db-bst DB-PRICE-FALSE)) 0)

(define (height-diff b)
  (if (false? b)
      0
      (- (height (bst-left b))
         (height (bst-right b)))))







(define DB-QUANTITY-DUMMY (make-db widget-quantity < = false))







(define (random-widgets num max)
  (build-list num
              (λ (dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))








(define (random-widgets-string num slen nmax)
  (local [(define (random-string len)
            (list->string
             (build-list len (λ (dummy) (integer->char (+ 97 (random 26)))))))]
    
    (build-list num
                (λ(dummy)
                  (make-widget
                   (random-string slen) 
                   (random nmax)
                   (random nmax))))))







(check-expect (db-bst (build-bst insert-avl (list A1 D1 W1 E5)
                                 DB-QUANTITY-DUMMY))
              (make-bst D1
                        (make-bst W1
                                  false
                                  (make-bst A1 false false))
                        (make-bst E5 false false)))


(check-expect (db-bst (build-bst insert-avl (list E5) DB-QUANTITY-DUMMY))
              (make-bst E5 false false))


(check-expect (db-bst (build-bst insert-avl empty DB-QUANTITY-DUMMY)) false)

(define (build-bst insert-fn low db)
  (foldr (lambda (wid db) (insert-fn wid db)) db low))







(define (time-insert)
  (local [(define low (random-widgets 250000 1000))

          
          
          
          
          
          (define (insert wid db)
            (local [(define (insert b)
                      (cond
                        [(false? b) (make-bst wid false false)]
                        [(insert-left? db wid b) (make-bst (bst-widget b)
                                                           (insert (bst-left b))
                                                           (bst-right b))]
                        [else
                         (make-bst (bst-widget b)
                                   (bst-left b)
                                   (insert (bst-right b)))]))]
    
              (make-db (db-field db)
                       (db-lt? db)
                       (db-eq? db)
                       (insert (db-bst db)))))]  
   
    (begin
      (printf "Timing for Non-Mutation Insertion: \n")
      (time (build-bst insert low DB-QUANTITY-DUMMY))
      (set-db-bst! DB-QUANTITY-DUMMY false)
      (printf "\n")
      (printf "Timing for Mutation Insertion: \n")
      (time (build-bst insert! low DB-QUANTITY-DUMMY))
      (set-db-bst! DB-QUANTITY-DUMMY false)
      "done"))) 






(define (time-find)
  (local [(define low (random-widgets-string 10000 10000 10000)) 
     
          (define DB-QUANTITY-AVL (build-bst insert-avl low DB-QUANTITY-DUMMY))
     
          (define DB-QUANTITY-BST (build-bst insert! low DB-QUANTITY-DUMMY))
          
          
          
          
          
          
          (define (compare? comparison-fn? db param b)
            ((comparison-fn? db) param ((db-field db) (bst-widget b)))) 
          
          
          
          
          (define (find param db)
            (local [(define (find b)
                      (cond
                        [(false? b) false]
                        [(compare? db-eq? db param b) (bst-widget b)]
                        [(compare? db-lt? db param b) (find (bst-left b))]
                        [else
                         (find (bst-right b))]))]
    
              (find (db-bst db))))

          
          
          (define (do-find low db)
            (local [(define (do-find acc)
                      (cond
                        [(= acc 0) (void)]
                        [else (begin
                                (map (lambda (wid)
                                       (find (widget-quantity wid) db)) low)
                                (do-find (sub1 acc)))]))]
         
              (do-find 10)))]
   
    (begin
      (printf "Timing for Finding in a BST: \n")
      (time (do-find low DB-QUANTITY-BST))
      (printf "\n")
      (printf "Timing for Finding in an AVL: \n")
      (time (do-find low DB-QUANTITY-AVL))
      (set-db-bst! DB-QUANTITY-DUMMY false)
      "done")))