

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3-1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))



(require 2htdp/image)

(define-struct bst (widget left right))




(define-struct widget (name quantity price))





(define-struct db (field lt? eq? bst))

(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 92 87))
(define C1 (make-widget "C1" 10 15))

(define DB1 (make-db widget-name string<? string=? (make-bst W1 false false)))
(define DB2 (make-db widget-name string<? string=? false))
(define DB3 (make-db widget-quantity < = (make-bst D1 (make-bst A1 false false)
                                                   (make-bst Z1 false false))))
(define DB4 (make-db widget-price < = (make-bst Z1 (make-bst D1
                                                             (make-bst A1
                                                                       (make-bst W1 false false)
                                                                       false) false) false)))
(define DB7 (make-db widget-name string<? string=? (make-bst B1 (make-bst A1 false false)
                                                             (make-bst D1 false
                                                                       (make-bst W1
                                                                                 false false)))))

(define DB8 (make-db widget-name string<? string=? (make-bst D1 (make-bst C1
                                                                          (make-bst B1 false false)
                                                                          false)
                                                             (make-bst W1 false false))))
(define DB9 (make-db widget-name string<? string=? (make-bst B1 (make-bst A1 false false)
                                                             (make-bst D1 false
                                                                       (make-bst Z1
                                                                                 false false)))))

(define DB10 (make-db widget-name string<? string=? (make-bst W1 (make-bst D1
                                                                           (make-bst B1
                                                                                     false false)
                                                                           false)
                                                              (make-bst Z1 false false))))






(check-expect (find "D1" DB1) false) 
(check-expect (find "W1" DB1) W1) 
(check-expect (find 3 DB3) false) 
(check-expect (find 3 DB4) A1) 
(check-expect (find "W1" DB2) false) 

(define (find x db)
  (local [(define (find-bst bst)
            (cond [(false? bst) false]
                  [((db-eq? db) x ((db-field db) (bst-widget bst))) (bst-widget bst)] 
                  [((db-lt? db) x ((db-field db) (bst-widget bst))) (find-bst (bst-left bst))]
                  [else (find-bst (bst-right bst))]))]
    (find-bst (db-bst db))))




(check-expect (db-bst (insert W1 DB2)) (make-bst W1 false false))

(check-expect (db-bst (insert A1 DB1)) (make-bst W1 (make-bst A1 false false) false))

(check-expect (db-bst (insert W1 DB3)) (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
                                                 (make-bst Z1 false false)))

(check-expect (db-bst (insert Z1 DB1)) (make-bst W1 false (make-bst Z1 false false)))

(define (insert widget db)
  (local [(define (insert-bst bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
                   (make-bst (bst-widget bst) (insert-bst (bst-left bst)) (bst-right bst))]
                  [else (make-bst (bst-widget bst) (bst-left bst) (insert-bst
                                                                   (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-bst (db-bst db)))))




(check-expect (db-bst (mutate-copy insert! W1 DB2)) (make-bst W1 false false))

(check-expect (db-bst (mutate-copy insert! A1 DB1)) (make-bst W1 (make-bst A1 false false) false))

(check-expect (db-bst (mutate-copy insert! W1 DB3))
              (make-bst D1 (make-bst A1 (make-bst W1 false false)
                                     false)
                        (make-bst Z1 false false)))

(check-expect (db-bst (mutate-copy insert! Z1 DB1)) (make-bst W1 false (make-bst Z1 false false)))


(check-expect (db-bst (local [(define this-copy (copy DB1))]
                        (begin (insert! A1 this-copy)
                               this-copy)))
              (make-bst W1 (make-bst A1 false false) false))

(define (insert! widget db)
  
  (local [(define (left-child? bst)
            ((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst))))
          
          
          
          
          
          (define (insert-bst! bst parent side-setter!)
            (cond [(false? bst) (side-setter! parent (make-bst widget false false))]
                  [(left-child? bst)
                   (insert-bst! (bst-left bst) bst set-bst-left!)]
                  [else (insert-bst! (bst-right bst) bst set-bst-right!)]))]
    (insert-bst! (db-bst db) db set-db-bst!)))

(check-expect (db-bst (insert-avl Z1 DB7)) (make-bst B1 (make-bst A1 false false)
                                                     (make-bst W1
                                                               (make-bst D1 false false)
                                                               (make-bst Z1 false false))))   


(check-expect (db-bst (insert-avl A1 DB8)) (make-bst D1 (make-bst B1 (make-bst A1 false false)
                                                                  (make-bst C1 false false))
                                                     (make-bst W1 false false)))

(check-expect (db-bst (insert-avl W1 DB9)) (make-bst B1 (make-bst A1 false false)
                                                     (make-bst W1 (make-bst D1 false false)
                                                               (make-bst Z1 false false))))

(check-expect (db-bst (insert-avl C1 DB10))
              (make-bst W1 (make-bst C1 (make-bst B1 false false) (make-bst D1 false false))
                        (make-bst Z1 false false)))


(check-expect (db-bst (insert-avl W1 DB2)) (make-bst W1 false false))

(check-expect (db-bst (insert-avl A1 DB1)) (make-bst W1 (make-bst A1 false false) false))

(check-expect (db-bst (insert-avl W1 DB3)) (make-bst D1 (make-bst A1
                                                                  (make-bst W1 false false) false)
                                                     (make-bst Z1 false false)))

(check-expect (db-bst (insert-avl Z1 DB1)) (make-bst W1 false (make-bst Z1 false false)))





(define (insert-avl widget db)
  (local [(define (insert-bst bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
                   (make-bst (bst-widget bst)
                             (balance (insert-bst (bst-left bst))) (bst-right bst))]
                  [else (make-bst (bst-widget bst) (bst-left bst)
                                  (balance (insert-bst (bst-right bst))))]))
          (define (balance bst)
            (cond
              [(> -1 (height-diff bst))
               (cond [(>= -1 (height-diff (bst-right bst)))
                      (rotate-right bst)]
                     [(<= 1 (height-diff (bst-right bst)))
                      (rotate-rleft bst)]                      
                     [else (bst-right bst)])]
              [(< 1 (height-diff bst))
               (cond [(>= -1 (height-diff (bst-left bst)))
                      (rotate-lright  bst)]
                     [(<= 1 (height-diff (bst-left bst)))
                      (rotate-left bst)]
                                                             
                     [else (bst-left bst)])
               ]
              [else bst]))]
         
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-bst (db-bst db)))))



(define BST1 (make-bst B1 false (make-bst C1 false (make-bst D1 false false))))
(define BST2 (make-bst W1 (make-bst D1 (make-bst A1 false false) false)false))
(define BST3 (make-bst D1 false (make-bst W1 false (make-bst Z1 false false))))
(define BST4 (make-bst D1 (make-bst B1 (make-bst A1 false false) false) false))
(define BST5 (make-bst A1 false (make-bst W1 (make-bst D1 false false)false)))
(define BST6 (make-bst W1 (make-bst B1 false (make-bst D1 false false))false))
(define BST7 (make-bst B1 false (make-bst W1 (make-bst D1 false false) false)))
(define BST8 (make-bst Z1 (make-bst B1 false (make-bst D1 false false)) false))

(check-expect (rotate-right BST3) (make-bst W1 (make-bst D1 false false)
                                            (make-bst Z1 false false)))
(check-expect (rotate-right BST1) (make-bst C1 (make-bst B1 false false)
                                            (make-bst D1 false false)))



(define (rotate-right bst)
  (make-bst (bst-widget(bst-right bst)) (make-bst (bst-widget bst) false false)
            (make-bst (bst-widget (bst-right (bst-right bst))) false false)))

(check-expect (rotate-left BST2) (make-bst D1 (make-bst A1 false false)
                                           (make-bst W1 false false)))
(check-expect (rotate-left BST4) (make-bst B1 (make-bst A1 false false)
                                           (make-bst D1 false false)))



(define (rotate-left bst)
  (make-bst (bst-widget(bst-left bst))
            (make-bst (bst-widget (bst-left (bst-left bst))) false false)
            (make-bst (bst-widget bst) false false)))

(check-expect (rotate-lright BST6) (make-bst D1 (make-bst B1 false false)
                                             (make-bst W1 false false)))
(check-expect (rotate-lright BST8) (make-bst D1 (make-bst B1 false false)
                                             (make-bst Z1 false false)))



(define (rotate-lright bst)
  (rotate-left (make-bst (bst-widget bst)
                         (make-bst (bst-widget (bst-right (bst-left bst)))
                                   (make-bst (bst-widget(bst-left bst)) false false) false)
                         false)))

(check-expect (rotate-rleft BST5) (make-bst D1 (make-bst A1 false false)
                                            (make-bst W1 false false)))
(check-expect (rotate-rleft BST7) (make-bst D1 (make-bst B1 false false)
                                            (make-bst W1 false false)))



(define (rotate-rleft bst)
  (rotate-right (make-bst (bst-widget bst) false
                          (make-bst (bst-widget (bst-left (bst-right bst)))
                                    false (make-bst (bst-widget(bst-right bst)) false false)))))







(define (set-db-bst-params! bst db)
  (set-db-bst! db bst))





(check-expect (db-bst (mutate-copy set-db-bst-params! false DB1)) false)

(check-expect (begin (mutate-copy set-db-bst-params! false DB1)
                     (db-bst DB1))
              (make-bst W1 false false))

(check-expect (db-bst (mutate-copy set-db-bst-params! false DB3)) false)

(check-expect (begin (mutate-copy set-db-bst-params! false DB3)
                     (db-bst DB3))
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))

(define (mutate-copy fn-mutator! val db)
  (local [(define this-copy (copy db))]
    (begin (fn-mutator! val this-copy)
           this-copy)))







(check-expect (begin (set-db-bst! (copy DB1) false)
                     (db-bst DB1)) (make-bst W1 false false))

(check-expect (db-bst (copy DB2)) (db-bst DB2))

(check-expect (db-bst (copy DB3)) (db-bst DB3))

(check-expect (begin (set-db-bst! (copy DB4) false)
                     (db-bst DB4)) (make-bst Z1 (make-bst D1
                                                          (make-bst A1
                                                                    (make-bst W1 false false)
                                                                    false) false) false))


(check-expect (db-bst (local [(define this-copy (copy DB4))]
                        (begin (set-bst-left! (bst-left (db-bst this-copy)) false)
                               DB4)))
              (make-bst Z1 (make-bst D1
                                     (make-bst A1
                                               (make-bst W1 false false)
                                               false) false) false))
              
            
(define (copy db)
  (local [(define (copy-bst bst)
            (cond [(false? bst) false]
                  [else
                   (make-bst (bst-widget bst)
                             (copy-bst (bst-left bst))
                             (copy-bst (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (copy-bst (db-bst db)))))




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







(define (time-insert)
  (local [(define list (random-widgets 250000 10000000))
          (define (loop-insert low db)
            (cond [(empty? low) db]
                  [else (loop-insert (rest low) (insert (first low) db))]))
          (define db! (make-db widget-quantity < = false))
          (define (loop-insert! low)
            (cond [(empty? low) (void)]
                  [else (begin (insert! (first low) db!)
                               (loop-insert! (rest low)))]))]
    (begin (time (loop-insert list (make-db widget-quantity < = false)))
           (time (loop-insert! list))
           "done!")))

 
 
 
 
 









(define (time-find)
  (local [(define list (random-widgets 10000 500000))
          (define bst (foldr insert (make-db widget-quantity < = false) list))
          (define avl (foldr insert-avl (make-db widget-quantity < = false) list))
          (define (loop-find low db)
            (cond [(empty? low)  (void)]
                  [else (begin (find ((db-field db) (first low)) db)
                               (loop-find (rest low) db))]))]
    (begin (time (loop-find list bst))
           (time (loop-find list avl))
           "done!")))

 
 
 
 
 
 
 
 
 
 