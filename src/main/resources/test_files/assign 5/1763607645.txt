

#reader(lib "htdp-advanced-reader.ss" "lang")((modname VenatArjun_AdlerBernhardt_Assignment5Part3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

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





(define (random-widgets-string num slen nmax)
  (local
    [(define (random-string len)
       (list->string (build-list len (λ(dummy)
                                       (integer->char (+ 97 (random 26)))))))]
    (build-list num
                (λ(dummy)
                  (make-widget
                   (random-string slen) 
                   (random nmax)
                   (random nmax))))))

(define (build-tree low)
  (foldr insert (make-db widget-quantity < = false) low))





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





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define B1 (make-widget "B1" 5 2))
(define V1 (make-widget "V1" 7 4))
(define I1 (make-widget "I1" 2 5))

(define BST1name (make-bst D1 (make-bst A1 false false)
                           (make-bst Z1 (make-bst W1 false false) false)))
(define BST2name (make-bst W1 (make-bst A1 false
                                        (make-bst D1 false false))
                           (make-bst Z1 false false)))
(define BST1price (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
                            (make-bst Z1 false false)))
(define BST2price (make-bst A1 (make-bst W1 false false)
                            (make-bst Z1 (make-bst D1 false false) false)))
(define BST1quantity (make-bst A1 (make-bst W1 false false)
                               (make-bst Z1
                                         (make-bst D1 false false)
                                         false)))
(define BST2quantity (make-bst Z1 (make-bst A1
                                            (make-bst W1 false false)
                                            (make-bst D1 false false))
                               false
                               ))

(define DB-quantity1 (make-db widget-quantity < = BST1quantity))
(define DB-quantity2 (make-db widget-quantity < = BST2quantity))
(define DB-name1 (make-db widget-name string<? string=? BST1name))
(define DB-name2 (make-db widget-name string<? string=? BST2name))
(define DB-price1 (make-db widget-price < = BST1price))
(define DB-price2 (make-db widget-price < = BST2price))






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




(check-expect (db-bst (insert V1 DB-quantity2))
              (make-bst Z1 (make-bst A1
                                     (make-bst W1 false false)
                                     (make-bst D1 false (make-bst V1 false false)))
                        false
                        ))

(check-expect (db-bst (insert B1 DB-name2))
              (make-bst W1 (make-bst A1 false
                                     (make-bst D1 (make-bst B1 false false) false))
                        (make-bst Z1 false false)))

(check-expect (db-bst (insert I1 DB-name2))
              (make-bst W1 (make-bst A1 false
                                     (make-bst D1 false (make-bst I1 false false)))
                        (make-bst Z1 false false)))

(check-expect (db-bst (insert B1 DB-price1))
              (make-bst D1 (make-bst A1 (make-bst W1 false (make-bst B1 false false)) false)
                        (make-bst Z1 false false)))


                                  

(define (insert w db)
  (local [(define (smaller? k b)
            ((db-lt? db) k ((db-field db) (bst-widget b))))

          
          

          

          
          (define (get-bst v b)
            (if (false? b)
                (make-bst v false false)
                (if (smaller? ((db-field db) v) b)
                    (make-bst (bst-widget b) (get-bst v (bst-left b)) (bst-right b))
                    (make-bst (bst-widget b) (bst-left b) (get-bst v (bst-right b))))
                ))]
    
    (make-db (db-field db) (db-lt? db) (db-eq? db) (get-bst w (db-bst db)))
    ))






(check-expect (db-bst (insert! V1 DB-quantity2))
              (make-bst Z1 (make-bst A1
                                     (make-bst W1 false false)
                                     (make-bst D1 false (make-bst V1 false false)))
                        false
                        ))

(check-expect (db-bst (insert! B1 DB-name1))
              (make-bst D1 (make-bst A1 false (make-bst B1 false false))
                        (make-bst Z1 (make-bst W1 false false) false)))

(check-expect (db-bst (insert! I1 DB-name2))
              (make-bst W1 (make-bst A1 false
                                     (make-bst D1 false (make-bst I1 false false)))
                        (make-bst Z1 false false)))

(check-expect (db-bst (insert! B1 DB-price1))
              (make-bst D1 (make-bst A1 (make-bst W1 false (make-bst B1 false false)) false)
                        (make-bst Z1 false false)))


                                  

(define (insert! w db)
  (local [(define (smaller? k b)
            ((db-lt? db) k ((db-field db) (bst-widget b))))

          
          

          

          
          (define (get-bst v b)
            (if (false? b)
                (make-bst v false false)
                (if (smaller? ((db-field db) v) b)
                    (if (false? (bst-left b))
                        (begin (set-bst-left! b (make-bst v false false)) b)
                        (begin (get-bst v (bst-left b)) b))
                    (if (false? (bst-right b))
                        (begin (set-bst-right! b (make-bst v false false)) b)    
                        (begin (get-bst v (bst-right b)) b))
                    )))]
    
    (make-db (db-field db) (db-lt? db) (db-eq? db) (get-bst w (db-bst db)))
    ))






(check-expect (right (make-bst 10 false (make-bst 20 false (make-bst 30 false false))))
              (make-bst 20 (make-bst 10 false false) (make-bst 30 false false))) 
(check-expect (right (make-bst 10 false (make-bst 30 (make-bst 20 false false) false)))
              (make-bst 30 (make-bst 10 false (make-bst 20 false false)) false)) 
(check-expect (right (make-bst 10 false
                               (make-bst 20 false
                                         (make-bst 30 false
                                                   (make-bst 40 false
                                                             (make-bst 50 false false
                                                                       ))))))
              (make-bst 20 (make-bst 10 false false)
                        (make-bst 30 false
                                  (make-bst 40 false
                                            (make-bst 50 false false))))
              

              )
              


(define (right bst)
  (local
    [(define old bst)
     (define new (bst-right bst))]
    (make-bst (bst-widget new) (make-bst (bst-widget old) (bst-left old) (bst-left new))
              (bst-right new))))





(check-expect (left (make-bst 30 (make-bst 20 (make-bst 10 false false) false) false))
              (make-bst 20 (make-bst 10 false false) (make-bst 30 false false)))

(check-expect (left (make-bst 30 (make-bst 10 false (make-bst 20 false false)) false))
              (make-bst 10 false (make-bst 30 (make-bst 20 false false) false))
              )
(check-expect (left (make-bst 50 
                              (make-bst 40 
                                        (make-bst 30 
                                                  (make-bst 20 
                                                            (make-bst 10 false false)
                                                            false) false) false) false))
              
              (make-bst 40 (make-bst 30 (make-bst 20 (make-bst 10 false false) false) false)
                        (make-bst 50 false false)))



(define (left bst)
  (local [(define old bst)
          (define new (bst-left bst))]
    (make-bst (bst-widget new) (bst-left new) (make-bst (bst-widget old) (bst-right new)
                                                        (bst-right old)))))





(check-expect (balance (make-bst 10 false (make-bst 20 false (make-bst 30 false false))))
              (make-bst 20 (make-bst 10 false false) (make-bst 30 false false)))
(check-expect (balance (make-bst 50 (make-bst 10 false (make-bst 40 false false)) false))
              (make-bst 40 (make-bst 10 false false) (make-bst 50 false false)))
(check-expect (balance (make-bst 10 false (make-bst 30 (make-bst 20 false false) false)))
              (make-bst 20 (make-bst 10 false false) (make-bst 30 false false)))
(check-expect (balance (make-bst 50 false (make-bst 60 (make-bst 55 false
                                                                 (make-bst 57 false false)) false)))
              (make-bst 55 (make-bst 50 false false) (make-bst 60 (make-bst 57 false false) false)))


(define (balance bst)
  (local
    [(define (balance bst flipped?)
       (cond
         [(false? bst) false]
         [flipped? (cond [(> (height-diff bst) 1) 
                          (balance (left (make-bst (bst-widget bst)
                                                   (right (bst-left bst))
                                                   (bst-right bst))) false)]
                         [(< (height-diff bst) -1) 
                          (balance (right (make-bst (bst-widget bst)
                                                    (bst-left bst)
                                                    (left (bst-right bst)))) false)]
                         [else (make-bst (bst-widget bst)
                                         (balance (bst-left bst) false)
                                         (balance (bst-right bst) false))])]
         [(> (height-diff bst) 1) 
          (balance (left bst) true)
          ] 
         [(< (height-diff bst) -1) 
          (balance (right bst) true)
          ]
         [else
          (make-bst (bst-widget bst) (balance (bst-left bst) false)
                    (balance (bst-right bst) false))] 
         ))]
    (balance bst false)))
                  





(check-expect (db-bst (insert-avl V1 DB-quantity1))
              (make-bst
               D1
               (make-bst A1 (make-bst W1 false false) false)
               (make-bst Z1 (make-bst V1 false false) false)))

(check-expect (db-bst (insert-avl B1 DB-name2))
              (make-bst D1 (make-bst A1 false (make-bst B1 false false))
                        (make-bst W1 (make-bst I1 false false)
                                  (make-bst Z1 false false))))

(check-expect (db-bst (insert B1 DB-price2))
              (make-bst A1
                        (make-bst W1 false
                                  (make-bst B1 false false))
                        (make-bst Z1 (make-bst D1 false false) false)))

                                  

(define (insert-avl w db)
  (local [(define (smaller? k b)
            ((db-lt? db) k ((db-field db) (bst-widget b))))

          
          

          

          
          (define (get-bst v b)
            (if (false? b)
                (make-bst v false false)
                (if (smaller? ((db-field db) v) b)
                    (make-bst (bst-widget b) (get-bst v (bst-left b)) (bst-right b))
                    (make-bst (bst-widget b) (bst-left b) (get-bst v (bst-right b))))
                ))]
    
    (make-db (db-field db) (db-lt? db) (db-eq? db) (balance (get-bst w (db-bst db))))
    ))






(check-expect (find "D1" DB-name1) D1) 
(check-expect (find "A1" DB-name2) A1) 
(check-expect (find 51 DB-quantity1) Z1) 
(check-expect (find 9 DB-quantity2) false) 


(define (find x db)
  (local [
          (define (smaller? k b)
            ((db-lt? db) k ((db-field db) (bst-widget b))))

          (define (same? k b)
            ((db-eq? db) k ((db-field db) (bst-widget b))))
          

          (define (get-bst k b)
            (cond
              [(false? b) false]
              [(smaller? k b) (get-bst k (bst-left b))]
              [(same? k b) (bst-widget b)]
              [else
               (get-bst k (bst-right b))]
              ))
          ]

    (get-bst x (db-bst db))
    )
  )



(define (build-tree! low)
  (foldr insert! (make-db widget-quantity < = false) low))

(define (build-avl-tree low)
  (foldr insert-avl (make-db widget-quantity < = false) low))

 (define 250K (random-widgets 250000 9999))
"time to make by recreation"
    (define dummy (time (build-tree 250K)))
"time to make by mutation"
    (define dummy1 (time (build-tree! 250K)))



    (define 10K (random-widgets 10000 9999))

    (define 10kBST (build-tree 10K))


    (define 10kAVL (build-avl-tree 10K))


    (define (find-all db low)
      (if (empty? low) 0
      (begin (find (widget-quantity (first low)) db) (find-all db (rest low)) 0)))

    "Time to find in BST"
    (begin (time (find-all 10kBST (append 10K 10K 10K 10K 10K 10K 10K 10K 10K 10K))) "done")
    "Time to find in AVL"
    (begin (time (find-all 10kAVL (append 10K 10K 10K 10K 10K 10K 10K 10K 10K 10K))) "done")
    