

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Assignment 5 Part 3 (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))





(require 2htdp/image)






(define-struct widget (name quantity price))







(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 6 4))


(define A2 (make-widget "A2" 0 0))
(define B2 (make-widget "B2" 0 0))
(define C2 (make-widget "C2" 0 0))
(define D2 (make-widget "D2" 0 0))
(define E2 (make-widget "E2" 0 0)) 
(define F2 (make-widget "F2" 0 0))
(define G2 (make-widget "G2" 0 0))
(define H2 (make-widget "H2" 0 0))
(define I2 (make-widget "I2" 0 0))








(define-struct bst (widget left right))







(define BST0 (make-bst W1 false false))
(define BST-by-name (make-bst W1
                              (make-bst A1 false false)
                              (make-bst Z1 false false)))
(define BST-by-price (make-bst D1
                               (make-bst A1 false false)
                               (make-bst Z1 false false)))

(define BST-by-quantity (make-bst A1
                                  (make-bst W1 false false)
                                  (make-bst Z1
                                            (make-bst D1 false false)
                                            false)))

(define BST1 (make-bst A1
                       (make-bst W1 false false)
                       (make-bst Z1
                                 (make-bst D1 false
                                           (make-bst E1 false false))
                                 false)))


(define BST2 (make-bst Z1
                       (make-bst W1
                                 (make-bst E1
                                           (make-bst D1
                                                     (make-bst A1 false false)
                                                     false)
                                           false)
                                 false)
                       false))



(define BST00 (make-bst W1 false false))

(define BST3 (make-bst E2
                       (make-bst C2 false
                                 (make-bst D2 false false)
                                 )
                       (make-bst F2 false
                                 (make-bst H2 false false)
                                 )
                       ))

(define BST4 (make-bst A1
                       (make-bst W1 false false)
                       (make-bst Z1
                                 (make-bst D1 false false)
                                 false)))









(define-struct db (field lt? eq? bst))












(define DB0 (make-db widget-name string<? string=? false)) 
(define DB1 (make-db widget-name string<? string=? BST0)) 
(define DB2 (make-db widget-name string<? string=? BST-by-name)) 
(define DB3 (make-db widget-price < = BST-by-price)) 
(define DB4 (make-db widget-quantity < = BST-by-quantity)) 


(define DB5 (make-db widget-name string<? string=? false)) 
(define DB6 (make-db widget-name string<? string=? BST00)) 
(define DB7 (make-db widget-quantity < = BST4)) 
(define DB8 (make-db widget-name string<? string=? BST3)) 
(define DB9 (make-db widget-name string<? string=? BST2)) 

















(check-expect (db-bst (insert! Z1 DB0))
              (make-bst Z1 false false)) 
 
(check-expect (db-bst (insert! A1 DB1))
              (make-bst W1
                        (make-bst A1 false false)
                        false)) 
 
(check-expect (db-bst (insert! D1 DB2))
              (make-bst W1
                        (make-bst A1 false
                                  (make-bst D1 false false))
                        (make-bst Z1 false false))) 
 
(check-expect (db-bst (insert! W1 DB3))
              (make-bst D1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  false)
                        (make-bst Z1 false false))) 
 
(check-expect (db-bst (insert! E1 DB4))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1 false
                                            (make-bst E1 false false))
                                  false))) 
               
 
 
 
(define (insert! key db)
  
  
  
  (local [ (define (insert!-bst bst parent path-function!) 
             (cond
               [(false? bst)
                (begin (path-function!
                        parent (make-bst key false false))
                       db)]
               
               [(smaller? db ((db-field db) key) bst)
                (insert!-bst (bst-left bst) bst set-bst-left!)]
               
               [else
                (insert!-bst (bst-right bst) bst set-bst-right!)]))]
    
    (if (false? (db-bst db))
        (begin (set-db-bst! db (make-bst key false false))
               db)
        (insert!-bst (db-bst db) (db-bst db) set-bst-left!))))
 
 














(check-expect (db-bst (insert-avl Z1 DB5))
              (make-bst Z1 false false)) 
                                         

(check-expect (db-bst (insert-avl A2 DB9))
              (make-bst E1
               (make-bst A2
                         (make-bst A1 false false)
                         (make-bst D1 false false))
               (make-bst Z1
                         (make-bst W1 false false)
                         false)))

(check-expect (db-bst (insert-avl A1 DB6))
              (make-bst W1
                        (make-bst A1 false false)
                        false)) 

(check-expect (db-bst (insert-avl Z1 DB6))
              (make-bst W1
                        false
                        (make-bst Z1 false false))) 


(check-expect (db-bst (insert-avl E1 DB7))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst E1
                                  (make-bst D1 false false)
                                  (make-bst Z1 false false)))) 

(check-expect (db-bst (insert-avl G2 DB8))
              (make-bst E2
                        (make-bst C2 false
                                  (make-bst D2 false false)
                                  )
                        (make-bst G2
                                  (make-bst F2 false false)
                                  (make-bst H2 false false)
                                  )
                        )) 

              
              



(define (insert-avl key db)
  (local [ (define (insert-avl-bst bst) 
             (cond
               [(false? bst) (make-bst key false false)]
               [(smaller? db ((db-field db) key) bst)
                (balance (make-bst
                          (bst-widget bst)
                          (insert-avl-bst (bst-left bst))
                          (bst-right bst)))]
               [else
                (balance (make-bst
                          (bst-widget bst)
                          (bst-left bst)
                          (insert-avl-bst (bst-right bst))))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-avl-bst (db-bst db)))))












(check-expect (balance BST0) BST0) 


(check-expect (balance BST1)
              (make-bst D1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  false)
                        (make-bst Z1
                                  (make-bst E1 false false)
                                  false)
                        )
              ) 
                                   

(check-expect (balance (make-bst D2 false
                                 (make-bst E2 false
                                           (make-bst F2 false false)
                                           )
                                 ))
              (make-bst E2
                        (make-bst D2 false false)
                        (make-bst F2 false false))) 

(check-expect (balance (make-bst E2
                                 (make-bst D2
                                           (make-bst B2 false
                                                     (make-bst C2 false false)
                                                     )
                                           false)
                                 false))
              (make-bst D2
                        (make-bst B2 false
                                  (make-bst C2 false false)
                                  )
                        (make-bst E2 false false))) 

(check-expect (balance (make-bst E2 false
                                 (make-bst G2
                                           (make-bst F2 false false)
                                           false)
                                 ))
              (make-bst F2
                        (make-bst E2 false false)
                        (make-bst G2 false false))) 
                                 


(define (balance bst)
  (local [
          (define (balanced? bst)
            (<= (abs (height-diff bst)) 1))

          (define (right-heavy? bst)
            (< (height-diff bst) 0))

          (define (left-heavy? bst)
            (> (height-diff bst) 0))
          
          ]

    (cond [(balanced? bst) bst]
          [(right-heavy? bst)
           (if (left-heavy? (bst-right bst))
               (rotate-right-left bst)
               (rotate-right bst))]
          [else
           (if (right-heavy? (bst-left bst))
               (rotate-left-right bst)
               (rotate-left bst))])))






(check-expect (rotate-left (make-bst E2
                                     (make-bst D2 false false)
                                     false))
              (make-bst D2 false
                        (make-bst E2 false false)
                        )) 

(check-expect (rotate-left (make-bst A2
                                     (make-bst B2
                                               (make-bst C2 false
                                                         (make-bst D2 false false)
                                                         )
                                               (make-bst E2 false false)
                                               )
                                     (make-bst F2 false false)))
              (make-bst B2
                        (make-bst C2 false
                                  (make-bst D2 false false)
                                  )
                        (make-bst A2
                                  (make-bst E2 false false)
                                  (make-bst F2 false false)
                                  ))) 

(check-expect (rotate-left (make-bst E2
                                     (make-bst C2
                                               (make-bst B2 false false)
                                               (make-bst D2 false false)
                                               )
                                     false))
              (make-bst C2
                        (make-bst B2 false false)
                        (make-bst E2
                                  (make-bst D2 false false)
                                  false))) 

(define (rotate-left bst)
  (make-bst
   (bst-widget (bst-left bst))
   (bst-left (bst-left bst))
   (make-bst
    (bst-widget bst)
    (bst-right (bst-left bst))
    (bst-right bst))))










(check-expect (rotate-right (make-bst A2 false
                                      (make-bst B2 false false)
                                      ))
              (make-bst B2
                        (make-bst A2 false false)
                        false)) 

(check-expect (rotate-right (make-bst B2
                                      (make-bst A2 false false)
                                      (make-bst D2
                                                (make-bst C2 false false)
                                                (make-bst F2
                                                          (make-bst E2 false false)
                                                          false)
                                                )
                                      ))
              (make-bst D2
                        (make-bst B2
                                  (make-bst A2 false false)
                                  (make-bst C2 false false)
                                  )
                        (make-bst F2
                                  (make-bst E2 false false)
                                  false)
                        )) 

(check-expect (rotate-right (make-bst E2 false
                                      (make-bst G2
                                                (make-bst F2 false false)
                                                (make-bst H2 false false)
                                                )
                                      ))
              (make-bst G2
                        (make-bst E2 false
                                  (make-bst F2 false false)
                                  )
                        (make-bst H2 false false))) 

(define (rotate-right bst) 
  (make-bst
   (bst-widget (bst-right bst))
   (make-bst
    (bst-widget bst)
    (bst-left bst)
    (bst-left (bst-right bst)))
   (bst-right (bst-right bst))))







(check-expect (rotate-left-right (make-bst A2
                                           (make-bst B2 false
                                                     (make-bst C2 false false)
                                                     )
                                           false))
              (make-bst C2
                        (make-bst B2 false false)
                        (make-bst A2 false false))) 

(check-expect (rotate-left-right (make-bst E2
                                           (make-bst A2 false
                                                     (make-bst B2 false
                                                               (make-bst C2 false false)
                                                               )
                                                     )
                                           false))
              (make-bst B2
                        (make-bst A2 false false)
                        (make-bst E2
                                  (make-bst C2 false false)
                                  false))) 

(check-expect (rotate-left-right (make-bst E2
                                           (make-bst A2 false
                                                     (make-bst C2
                                                               (make-bst B2 false false)
                                                               (make-bst D2 false false)
                                                               ))
                                           false))
              (make-bst C2
                        (make-bst A2 false
                                  (make-bst B2 false false)
                                  )
                        (make-bst E2
                                  (make-bst D2 false false)
                                  false)))
                                                                                      

(define (rotate-left-right bst)
  (rotate-left
   (make-bst
    (bst-widget bst)
    (rotate-right (bst-left bst))
    (bst-right bst))))










(check-expect (rotate-right-left (make-bst A2 false
                                           (make-bst B2
                                                     (make-bst C2 false false)
                                                     false)
                                           ))
              (make-bst C2
                        (make-bst A2 false false)
                        (make-bst B2 false false))) 

(check-expect (rotate-right-left (make-bst E2 false
                                           (make-bst I2
                                                     (make-bst H2
                                                               (make-bst G2 false false)
                                                               false)
                                                     false)))
              (make-bst H2
                        (make-bst E2 false
                                  (make-bst G2 false false)
                                  )
                        (make-bst I2 false false))) 

(check-expect (rotate-right-left (make-bst E2 false
                                           (make-bst I2
                                                     (make-bst G2
                                                               (make-bst F2 false false)
                                                               (make-bst H2 false false)
                                                               )
                                                     false)))
              (make-bst G2
                        (make-bst E2 false
                                  (make-bst F2 false false)
                                  )
                        (make-bst I2
                                  (make-bst H2 false false)
                                  false))) 
                                                               


(define (rotate-right-left bst)
  (rotate-right
   (make-bst
    (bst-widget bst)
    (bst-left bst)
    (rotate-left (bst-right bst)))))



















            
(define (insert key db)
  (local [(define (insert-bst bst) 
            (cond
              [(false? bst) (make-bst key false false)]
              [(same? db ((db-field db) key) bst) bst]
              [(smaller? db ((db-field db) key) bst)
               (make-bst
                (bst-widget bst)
                (insert-bst (bst-left bst))
                (bst-right bst))]
              [else
               (make-bst
                (bst-widget bst)
                (bst-left bst)
                (insert-bst (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-bst (db-bst db)))))









(define (same? db key bst)
  (cond [(false? bst) false]
        [else
         ((db-eq? db) key ((db-field db) (bst-widget bst)))]))












(define (smaller? db key bst)
  (cond [(false? bst) false]
        [else
         ((db-lt? db) key ((db-field db) (bst-widget bst)))]))












(define (time-insert)
  (local [(define ex-DB (make-db widget-name string<? string=? false))
          (define low-1 (random-widgets 250000 999999999))
          
          (define (normal-insert db low)
            (cond [(empty? low) false]
                  [else
                   (normal-insert (insert (first low) db)
                                  (rest low))]))

                   

          (define (fast-insert db low)
            (cond [(empty? low) false]
                  [else
                   (fast-insert (insert! (first low) db)
                                (rest low))]))]
    
    (begin (time (normal-insert ex-DB low-1))
           (time (fast-insert ex-DB low-1))
           "Done")))






(define (time-find)
  (local [(define ex-db (make-db widget-name string<? string=? false))
          (define low (random-widgets 10000 9999999))
          
          (define (bst-insert db low)
            (cond [(empty? low) db]
                  [else
                   (bst-insert (insert (first low) db)
                               (rest low))]))

          (define (avl-insert db low)
            (cond [(empty? low) db]
                  [else
                   (avl-insert (insert-avl (first low) db)
                               (rest low))]))
            
          
          (define db-normal (bst-insert ex-db low))
          (define db-avl (avl-insert ex-db low))


          
          
          
          
          (define (find key db)
            (local [
                    (define (find-bst bst)
                      (cond [(false? bst) false]
                            [(same? db key bst)
                             (bst-widget bst)]
                            [(smaller? db key bst)
                             (find-bst (bst-left bst))]
                            [else
                             (find-bst (bst-right bst))]))]
              (find-bst (db-bst db))))

          (define (find-all db low)
            (cond [(empty? low) false]
                  [else
                   (begin (find ((db-field db) (first low)) db)
                          (find-all db (rest low)))]))

          ]

    (begin (time (find-all db-normal low))
           (time (find-all db-avl low))
           "Done")

    ))



          






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