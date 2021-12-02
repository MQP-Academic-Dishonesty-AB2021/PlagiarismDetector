

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))




(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 12 12))
(define F1 (make-widget "F1" 12 22))
(define B1 (make-widget "B1" 10 3))
(define J1 (make-widget "J1" 4 12))
(define T1 (make-widget "T1" 8 7))
(define E1 (make-widget "E1" 4 30))

(define-struct bst (widget left right))




 

(define BST0 false)
(define BST1 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst W1 false (make-bst Z1 false false)))) 
(define BST2 (make-bst W1
                       (make-bst D1 false false)
                       (make-bst Z1 false (make-bst Z1 false false)))) 
(define BST3 (make-bst A1                                       
                       (make-bst W1 false false)
                       (make-bst Z1 (make-bst D1 false false) false))) 
(define BST4 (make-bst D1
                       (make-bst C1 (make-bst B1 false false) false)
                       false)) 
(define BST5 (make-bst A1
                       false
                       (make-bst B1 false (make-bst D1 false false)))) 
(define BST6 (make-bst A1
                       false
                       (make-bst F1 (make-bst D1 false false) false))) 
(define BST7 (make-bst C1
                       (make-bst A1 false false)
                       false))
(define BST8 (make-bst C1
                       (make-bst A1 false (make-bst B1 false false))
                       false)) 




(define-struct db (field lt? eq? bst))





(define DB-name (make-db widget-name string<? string=? BST1))  
(define DB-name2 (make-db widget-name string<? string=? BST4)) 
(define DB-name3 (make-db widget-name string<? string=? BST5)) 
(define DB-name4 (make-db widget-name string<? string=? BST6)) 
(define DB-name5 (make-db widget-name string<? string=? BST7)) 

(define DB-quantity (make-db widget-quantity < = BST3))
(define DB-price (make-db widget-price < = false))









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





(check-expect (db-bst (insert! B1 DB-name))   
              (db-bst (make-db
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name)
                       (make-bst D1
                                 (make-bst A1
                                           false
                                           (make-bst B1 false false))
                                 (make-bst W1
                                           false
                                           (make-bst Z1 false false))))))  

(check-expect (db-bst (insert! B1 DB-quantity))
              (db-bst (make-db
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name)
                       (make-bst A1
                                 (make-bst W1 false false)
                                 (make-bst Z1
                                           (make-bst D1
                                                     false
                                                     (make-bst B1 false false))
                                           false)))))  

(check-expect (db-bst (insert! B1 DB-price))
              (db-bst (make-db
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name)  
                       (make-bst B1 false false)))) 

(check-expect (db-bst (insert! E1 DB-quantity)) 
              (db-bst (make-db
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name) 
                       (make-bst A1
                                 (make-bst W1 false false)
                                 (make-bst Z1
                                           (make-bst D1
                                                     (make-bst E1 false false)
                                                     (make-bst B1 false false))
                                           false))))) 

(define (insert! widget db)
  (local
    [(define (insert-widget widget bst parent LoR)
       (cond
         [(false? bst)
          (cond [(string=? "left" LoR) (begin
                                         (set-bst-left!
                                          parent
                                          (make-bst widget false false))
                                         (db-bst db))]
                [(string=? "right" LoR) (begin
                                          (set-bst-right!
                                           parent
                                           (make-bst widget false false))
                                          (db-bst db))]
                [else
                 (make-bst widget false false)])]
         [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
          (insert-widget widget (bst-left bst) bst "left")]
         [else 
          (insert-widget widget (bst-right bst) bst "right")]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-widget widget (db-bst db) (db-bst db) ""))))







(check-expect (db-bst (insert-avl T1 DB-name))
              (db-bst (make-db                          
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name)
                       (make-bst D1
                                 (make-bst A1
                                           false
                                           (make-bst B1 false false))
                                 (make-bst W1
                                           (make-bst T1 false false)
                                           (make-bst Z1 false false)))))) 

(check-expect (db-bst (insert-avl B1 DB-name5))
              (db-bst (make-db                          
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name)
                       (make-bst B1
                                 (make-bst A1 false false)
                                 (make-bst C1 false false)))))  

(check-expect (db-bst (insert-avl A1 DB-name2))
              (db-bst (make-db                          
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name)
                       (make-bst C1
                                 (make-bst B1
                                           (make-bst A1 false false)
                                           false)
                                 (make-bst D1 false false)))))  

(check-expect (db-bst (insert-avl F1 DB-name3))
              (db-bst (make-db                         
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name)
                       (make-bst B1
                                 (make-bst A1 false false)
                                 (make-bst D1
                                           false
                                           (make-bst F1 false false)))))) 

(check-expect (db-bst (insert-avl B1 DB-name4))
              (db-bst (make-db                       
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name)
                       (make-bst D1
                                 (make-bst A1
                                           false
                                           (make-bst B1 false false))
                                 (make-bst F1 false false))))) 

(check-expect (db-bst (insert-avl B1 DB-price))
              (db-bst (make-db                        
                       (db-field DB-name)
                       (db-lt? DB-name)
                       (db-eq? DB-name)  
                       (make-bst B1 false false))))

(define (insert-avl widget db)

  (local

    [(define (insert-widget widget bst)
       (cond
         [(false? bst) (make-bst widget false false)]
         [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
          (make-bst (bst-widget bst)
                    (balance (insert-widget widget (bst-left bst)))
                    (bst-right bst))]
         [else
          (make-bst (bst-widget bst)
                    (bst-left bst)
                    (balance (insert-widget widget (bst-right bst))))]))]

    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (balance (insert-widget widget (balance (db-bst db)))))))







(check-expect (balance BST0) false) 
(check-expect (balance BST4) (make-bst C1       
                                       (make-bst B1 false false)
                                       (make-bst D1 false false))) 
(check-expect (balance BST5) (make-bst B1       
                                       (make-bst A1 false false)
                                       (make-bst D1 false false))) 
(check-expect (balance BST6) (make-bst D1       
                                       (make-bst A1 false false)
                                       (make-bst F1 false false))) 
(check-expect (balance BST8) (make-bst B1       
                                       (make-bst A1 false false)
                                       (make-bst C1 false false))) 

(define (balance bst)
  (local [(define (leftleft bst)
            (balance (make-bst (bst-widget (bst-left bst))
                               (bst-left (bst-left bst))
                               (make-bst (bst-widget bst) false false))))
          (define (leftright bst)
            (balance (make-bst (bst-widget bst)
                               (make-bst (bst-widget
                                          (bst-right (bst-left bst)))
                                         (make-bst (bst-widget (bst-left bst))
                                                   false
                                                   false)
                                         false)
                               false)))
          (define (rightright bst)
            (balance (make-bst (bst-widget (bst-right bst))
                               (make-bst (bst-widget bst)
                                         false
                                         (bst-left (bst-right bst)))
                               (bst-right (bst-right bst)))))
          (define (rightleft bst)
            (balance (make-bst (bst-widget bst)
                               false
                               (make-bst (bst-widget
                                          (bst-left (bst-right bst)))
                                         false
                                         (make-bst (bst-widget
                                                    (bst-right bst))
                                                   false
                                                   false)))))]
    (cond
      [(<= (abs (height-diff bst)) 1) bst]
      [(>= (height-diff bst) 2)
       (cond
         [(>= (height-diff (bst-left bst)) 1) (leftleft bst)]
         [else (leftright bst)])]
      [(<= (height-diff bst) -2)
       (cond
         [(<= (height-diff (bst-right bst)) -1) (rightright bst)]
         [else (rightleft bst)])])))








(define (insert widget db)
  (local
    [(define (insert-widget widget bst)
       (cond
         [(false? bst) (make-bst widget false false)]
         [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
          (make-bst (bst-widget bst)
                    (insert-widget widget (bst-left bst))
                    (bst-right bst))]
         [else
          (make-bst (bst-widget bst)
                    (bst-left bst)
                    (insert-widget widget (bst-right bst)))]))]
  
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-widget widget (db-bst db)))))


(define (find val db)
  (cond
    [(false? (db-bst db)) false]
    [((db-eq? db) val ((db-field db) (bst-widget (db-bst db))))
     (bst-widget (db-bst db))]
    [((db-lt? db) val ((db-field db) (bst-widget (db-bst db))))
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
                (bst-right
                 (db-bst db))))]))






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





(define (listofwidget->bst insertfunction low db)
  (cond [(empty? low) db]
        [else (listofwidget->bst
               insertfunction
               (rest low)
               (insertfunction (first low) db))]))





(define (time-insert numberofwidgets max)
  (local [(define lorw (random-widgets numberofwidgets max))]
    (begin
      (time (listofwidget->bst
             insert!
             lorw
             (make-db widget-name string<? string=? false)))
      (time (listofwidget->bst
             insert
             lorw
             (make-db widget-name string<? string=? false)))
      "done")))




(define (time-find)
  (local [(define lorw (random-widgets 10000 999999999))
          (define BST (listofwidget->bst
                       insert!
                       lorw
                       (make-db widget-quantity < = false)))
          (define AVL (listofwidget->bst
                       insert-avl
                       lorw
                       (make-db widget-quantity < = false)))
          
          (define (findbst elm)
            (find (widget-quantity elm) BST))
          (define (findavl elm)
            (find (widget-quantity elm) AVL))]
    (begin
      (time (map findbst lorw))
      (time (map findavl lorw))
      "done")))


