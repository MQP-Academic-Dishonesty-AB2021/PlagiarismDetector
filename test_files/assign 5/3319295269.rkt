

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


(require 2htdp/image)



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




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define R1 (make-widget "R1" 100000 2.5))


(define B1 (make-widget "B1" 12 2))
(define F1 (make-widget "F1" 12 4))
(define X1 (make-widget "X1" 7 6))

(define BSTA1 (make-bst A1 false false))
(define BSTD1 (make-bst D1 false false))
(define BSTZ1 (make-bst Z1 false false))
(define BSTX1 (make-bst X1 false false))
(define BSTB1 (make-bst B1 BSTA1 BSTD1))
(define BSTW1 (make-bst W1 false BSTZ1))
(define BSTF1 (make-bst F1 BSTB1 BSTW1))

(define L1 (make-widget "L1" 2 13))
(define M1 (make-widget "M1" 3 14))
(define N1 (make-widget "N1" 1 15))
(define O1 (make-widget "O1" 4 17))
(define P1 (make-widget "P1" 3 18))

(define BSTpri (make-bst L1                                 
                         (make-bst F1                               
                                   (make-bst B1
                                             (make-bst W1 false false)
                                             (make-bst A1 false false))
                                   (make-bst X1
                                             (make-bst D1 false false)
                                             false))
                         (make-bst M1                               
                                   false
                                   (make-bst O1
                                             (make-bst N1 false
                                                       (make-bst Z1 false false))
                                             (make-bst P1 false false))
                                   )))

(define BSTpri1 (make-bst L1                                
                          (make-bst F1                               
                                    (make-bst B1
                                              (make-bst W1 false false)
                                              (make-bst A1 false false))
                                    (make-bst X1
                                              false
                                              false))
                          (make-bst M1                               
                                    false
                                    (make-bst O1
                                              (make-bst N1 false
                                                        (make-bst Z1 false false))
                                              (make-bst P1 false false))
                                    )))
                                                          
(define BSTL1 (make-bst L1 false false))
(define BSTP1 (make-bst P1 false false))
(define BSTM1 (make-bst M1 BSTL1 false))
(define BSTO1 (make-bst O1 false BSTP1))
(define BSTpN1 (make-bst N1 BSTM1 BSTO1))

(define BSTL1w (make-bst L1 false false))
(define BSTP1w (make-bst P1 false false))
(define BSTM1w (make-bst M1 false false))
(define BSTO1w (make-bst O1 false BSTP1w))
(define BSTpN1w (make-bst N1 BSTM1w BSTO1w))


(define DB-quantity (make-db widget-quantity < = false))

(define DB-name (make-db widget-name string<? string=? BSTF1))
(define DB-name2 (make-db widget-name string<? string=? BSTpN1w))
(define DB-name3 (make-db widget-name string<? string=? BSTpN1))
(define DB-price (make-db widget-price < = BSTpri))
(define DB-price1 (make-db widget-price < = BSTpri1))

(define DB-empty (make-db widget-name string<? string=? false))
(define DB-emptyfill (make-db widget-name string<? string=? (make-bst D1 false false)))
(define DB-empty2 (make-db widget-name string<? string=? false))


  




(check-expect (db-bst (insert! L1 DB-name2)) (db-bst DB-name3))           
(check-expect (db-bst (insert! D1 DB-price1)) (db-bst DB-price))          
(check-expect (db-bst (insert! D1 DB-empty)) (db-bst DB-emptyfill))       


(define (insert! wid DB)
  (local
    [(define (insert bst)
       (cond
         [(false? bst) (set-db-bst! DB (make-bst wid false false))]               
         [((db-lt? DB) ((db-field DB) wid) ((db-field DB) (bst-widget bst)))      
          (if (false? (bst-left bst))
              (set-bst-left! bst (make-bst wid false false))
              (insert (bst-left bst)))]
         [else
          (if (false? (bst-right bst))
              (set-bst-right! bst (make-bst wid false false))                     
              (insert (bst-right bst)))]
         )
       )]
    (begin (insert (db-bst DB))
           DB)))







(define C1 (make-widget "C1" 1 0))
(define RR (make-bst A1 false (make-bst B1 false (make-bst C1 false false))))


(check-expect (rotate-right RR) (make-bst B1 (make-bst A1 false false) (make-bst C1 false false)))


(define (rotate-right bst)
   (local
     [(define Right (bst-right bst))
     (define Left  (bst-left bst))
     (define RightL (bst-left (bst-right bst)))]
     (make-bst (bst-widget Right)
               (make-bst (bst-widget bst)
                         Left
                         RightL)
               (bst-right Right))))




(define LL  (make-bst C1 (make-bst B1 (make-bst A1 false false) false) false))
(define LL2 (make-bst X1 (make-bst A1 (make-bst B1 (make-bst W1 false false) false) false)
                         (make-bst Z1 false false)))

(check-expect (rotate-left LL)  (make-bst B1 (make-bst A1 false false) (make-bst C1 false false)))
(check-expect (rotate-left LL2) (make-bst A1 (make-bst B1 (make-bst W1 false false) false)
                                             (make-bst X1 false (make-bst Z1 false false))))

(define (rotate-left bst)
  (local
    [(define Right (bst-right bst))
     (define Left  (bst-left bst))
     (define LeftR (bst-right (bst-left bst)))]
    (make-bst (bst-widget Left)
              (bst-left Left)
              (make-bst (bst-widget bst)
                        LeftR
                        Right))))
                        




(define LR (make-bst C1 (make-bst A1 false (make-bst B1 false false)) false))

(check-expect (setup-left LR) (make-bst C1 (make-bst B1 (make-bst A1 false false) false) false))

(define (setup-left bst)
   (local
     [(define Right (bst-right bst))
      (define Left  (bst-left bst))
      (define LeftR (bst-right (bst-left bst)))]
     (make-bst (bst-widget bst)
               (make-bst (bst-widget LeftR) 
                         (make-bst (bst-widget Left) (bst-left Left) (bst-left LeftR))
                         (bst-right LeftR))
                Right)))




(define RL (make-bst A1 false (make-bst C1 (make-bst B1 false false) false)))

(check-expect (setup-right RL) (make-bst A1 false (make-bst B1 false (make-bst C1 false false))))

(define (setup-right bst)
  (local
    [(define Right (bst-right bst))
     (define Left  (bst-left bst))
     (define RightL (bst-left (bst-right bst)))]
    (make-bst (bst-widget  bst)
              Left
              (make-bst (bst-widget RightL)                        
                        (bst-left RightL)
                         (make-bst (bst-widget Right) (bst-right RightL) (bst-right Right))))))
                        
                                   








(define PriAVL   (make-bst D1 (make-bst A1 (make-bst B1 false false) false)
                           (make-bst X1 false false)))
(define PriAVL2 (make-bst D1
                          (make-bst A1 (make-bst B1 false false) false)
                          (make-bst X1 false (make-bst Z1 false false))))

 
(define PriAVLLL (make-bst D1 (make-bst B1 (make-bst W1 false false) (make-bst A1 false false))
                           (make-bst X1 false false)))
(define PriAVLLR (make-bst D1 (make-bst R1 (make-bst B1 false false)
                                        (make-bst A1 false false))
                           (make-bst X1 false false)))

(define PriAVLRR (make-bst D1
                           (make-bst A1 (make-bst B1 false false) false)
                           (make-bst Z1 (make-bst X1 false false) (make-bst O1 false false))))
(define PriAVLRL (make-bst D1
                           (make-bst A1 (make-bst B1 false false) false)
                           (make-bst N1 (make-bst X1 false false) (make-bst Z1 false false))))
(define DB-price5 (make-db widget-price < = PriAVL)) 
(define DB-price6 (make-db widget-price < = PriAVL2))


(check-expect (db-bst (insert-avl W1 DB-price5))   PriAVLLL)                   
(check-expect (db-bst (insert-avl R1 DB-price5))   PriAVLLR)                   
(check-expect (db-bst (insert-avl O1 DB-price6))   PriAVLRR)                   
(check-expect (db-bst (insert-avl N1 DB-price6))   PriAVLRL)                   
(check-expect (db-bst (insert-avl D1 DB-empty2)) (db-bst DB-emptyfill))        


(define (insert-avl wid DB)
  (local
    [(define (insert-name wid bst)
       (cond
         [(false? bst) (make-bst wid false false)]
         [((db-lt? DB) ((db-field DB) wid) ((db-field DB) (bst-widget bst)))
          (make-bst
           (bst-widget bst)
           (balance (insert-name wid (bst-left bst)))
           (bst-right bst))]
         [else (make-bst (bst-widget bst) (bst-left bst) (balance (insert-name wid
                                                                               (bst-right bst))))]
         )
       )]
    (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (balance (insert-name wid (db-bst DB))))))






(check-expect (balanced? BSTL1) true)               
(check-expect (balanced? BSTpri) true)             
(check-expect (balanced? LL) false)                 
(check-expect (balanced? BSTM1) true)               
(check-expect (balanced? BSTpN1) true)               



 


(define (balanced? bst)
  (if (> (abs (height-diff bst)) 1)
      false
      true))
    




(check-expect (balance BSTL1) BSTL1)   
(check-expect (balance BSTM1) BSTM1)   
(check-expect (balance LL) (make-bst B1 (make-bst A1 false false) (make-bst C1 false false)))    

(define (balance bst)
  (cond
    [(balanced? bst) bst]
    [else (balancer bst)]))





(check-expect (balancer LL) (make-bst B1 (make-bst A1 false false) (make-bst C1 false false)))
(check-expect (balancer RL) (make-bst B1 (make-bst A1 false false) (make-bst C1 false false)))
(check-expect (balancer LR) (make-bst B1 (make-bst A1 false false) (make-bst C1 false false)))
(check-expect (balancer RR) (make-bst B1 (make-bst A1 false false) (make-bst C1 false false)))

(define (balancer bst)
  (if (>  (height-diff bst) 0)
      (if (> (height-diff (bst-left bst)) 0)
          (rotate-left bst)
          (rotate-left (setup-left bst)))
      (if (< (height-diff (bst-right bst)) 0)
          (rotate-right bst)
          (rotate-right (setup-right bst)))))











(define DB-emptytest (make-db  widget-name string<? string=? false))
(define DB-emptytest1 (make-db widget-name string<? string=? false))
(define DB-emptytest2 (make-db widget-name string<? string=? false))
(define DB-emptytest3 (make-db widget-name string<? string=? false))
(define DB-emptytest4 (make-db widget-name string<? string=? false))   
(define DB-emptytest5 (make-db widget-name string<? string=? false))


(define (insert wid DB)
  (local
    [(define (insert-name wid bst)
       (cond
         [(false? bst) (make-bst wid false false)]
         [((db-lt? DB) ((db-field DB) wid) ((db-field DB) (bst-widget bst)))
          (make-bst
           (bst-widget bst)
           (insert-name wid (bst-left bst))
           (bst-right bst))]
         [else (make-bst (bst-widget bst) (bst-left bst) (insert-name wid (bst-right bst)))]
         )
       )]
    (make-db (db-field DB) (db-lt? DB) (db-eq? DB) (insert-name wid (db-bst DB)))))

  

(define (build-tree DB low) 
  (foldr (λ (wid DB) (insert wid DB)) DB-emptytest1 low))

(define (build-tree! DB low)
  (foldr (λ (wid DB) (insert! wid DB)) DB-emptytest3 low))

(define (build-treeavl DB low)
  (foldr insert-avl DB-emptytest5 low))





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


(define testlow
  (random-widgets 250000 999999999))








(define (timed-test)
  (begin
    (set-db-bst! DB-emptytest2 false)
    (time (build-tree DB-emptytest2 testlow))
    (set-db-bst! DB-emptytest2 false)
    (time (build-tree! DB-emptytest2 testlow))
    "done"))
(define testlow2
  (random-widgets 10000 999999999))


(define BSTCheck (build-tree DB-emptytest3 testlow2))
(define AVLCheck (build-treeavl DB-emptytest5 testlow2))




(check-expect (findtot testlow2 BSTCheck 0) 10000)    
(check-expect (findtot testlow2 AVLCheck 0) 10000)    
                                                      
                                                      
(define (findtot low DB counter)
  (local
    [(define (find X BST)
       (cond
         [((db-eq? DB) ((db-field DB) X) ((db-field DB) (bst-widget BST)))
          (findtot (rest low) DB (add1 counter))]
         [((db-lt? DB) ((db-field DB) X) ((db-field DB) (bst-widget BST)))
          (find X (bst-left BST))]
         [else
          (find X (bst-right BST))]))]
    (if (empty? low)
        counter
        (find (first low) (db-bst DB))
        )))





(define (time-test)
  (begin
    (time (findtot testlow2 BSTCheck 0))
    (time (findtot testlow2 AVLCheck 0))
    "done"
    ))





