

#reader(lib "htdp-advanced-reader.ss" "lang")((modname assignment5part3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))





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
(define C1 (make-widget "C1" 8 4))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 5 5))

(define BST0 (make-bst W1 false false))
(define BST1 (make-bst D1 false (make-bst Z1 false false)))
(define BST2 (make-bst W1 (make-bst A1 false (make-bst D1 false false))
                       (make-bst Z1 false false)))
(define BST3 false) 

(define DB-quantity (make-db widget-quantity < = false))
(define DB-quantity2 (make-db widget-quantity < = BST0))
(define DB-name2 (make-db widget-name string<? string=? BST2))
(define DB-name1 (make-db widget-name string<? string=? BST1))
(define DB-name (make-db widget-name string<? string=? BST0))
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




(define (b-val field b) (field (bst-widget b)))










(define (insert! widg dtb)
  (local [
          
          
          
          
          
          
          (define (insert-widget! b parent smaller-than-parent? field smaller?)
            (cond
              [(false? b)
               (if smaller-than-parent?
                   (set-bst-left! parent (make-bst widg false false))
                   (set-bst-right! parent (make-bst widg false false)))
                   ]
              [else
               (local
                 
                 [(define smt (smaller? (field widg) (b-val field b)))]
                 (insert-widget!
                  (if smt (bst-left b)
                          (bst-right b))
                  b 
                  smt 
                  field 
                  smaller?)) 
               ]))]
    (begin 
            
            (if (false? (db-bst dtb)) (set-db-bst! dtb (make-bst widg false false))
                (insert-widget! (db-bst dtb)
                                false 
                                false 
                                (db-field dtb)
                                (db-lt? dtb)))
            
             dtb))) 




(define BST0-C (make-bst W1 false false))
(define BST2-C (make-bst W1 (make-bst A1 false (make-bst D1 false false))
                         (make-bst Z1 false false)))
(define DB-quantity-C (make-db widget-quantity < = false))
(define DB-quantity2-C (make-db widget-quantity < = BST0-C))
(define DB-name2-C (make-db widget-name string<? string=? BST2-C))

(insert! W1 DB-quantity-C)
(check-expect  (db-bst DB-quantity-C) 
              (make-bst W1 false false))

(insert! C1 DB-name2-C)
(check-expect  (db-bst DB-name2-C)
              (make-bst W1 (make-bst A1 false
                        (make-bst D1 (make-bst C1 false false) false))
                        (make-bst Z1 false false)))

(insert! Z1 DB-quantity2-C)
(check-expect (db-bst DB-quantity2-C) 
              (make-bst W1 false (make-bst Z1 false false)))








(define (do-left-rotation tree)
  (if (< (height-diff (bst-left tree)) 0)
      
      (do-left-rotation
       (make-bst (bst-widget tree)
                 
                 (make-bst (bst-widget (bst-right (bst-left tree)))
                           
                           (make-bst (bst-widget (bst-left tree)) false false)
                           false)
                 false))
      
      
      (make-bst (bst-widget (bst-left tree))
                
                (bst-left (bst-left tree))
                
                (make-bst (bst-widget tree) false false))))




(define (do-right-rotation tree)
  (if (> (height-diff (bst-right tree)) 0)
      
      (do-right-rotation
       (make-bst (bst-widget tree)
                 false
                 
                 (make-bst (bst-widget (bst-left (bst-right tree)))
                           
                           false
                           (make-bst (bst-widget (bst-right tree)) false false)
                           )))
      
      
      (make-bst (bst-widget (bst-right tree))
                
                (make-bst (bst-widget tree) false false)
                
                (bst-right (bst-right tree))
                )))







(check-expect (balance (make-bst C1 false (make-bst Z1 false false)))
              (make-bst C1 false (make-bst Z1 false false))) 

(check-expect (balance
               (make-bst W1
                         (make-bst A1 false (make-bst C1 false (make-bst D1 false false)))
                         (make-bst Z1 false false)))
              (make-bst W1
                        (make-bst C1 (make-bst A1 false false) (make-bst D1 false false))
                        (make-bst Z1 false false)))


(define (balance tree)
  (local [(define hd (height-diff tree))]
    (cond
          [(false? tree) false]
          
          
          [(and (= 2 hd) (false? (bst-right tree))) (do-left-rotation tree)]
          
          
          [(and (= -2 hd) (false? (bst-left tree))) (do-right-rotation tree)]
          [else
           
           (make-bst (bst-widget tree) (balance (bst-left tree)) (balance (bst-right tree)))]
          ))
)





(define (avl-insert widg dtb)
  (local [
          (define (insert-widget b field smaller?)
            (cond
              [(false? b)
               (make-bst widg false false)]
              [(smaller? (field widg) (b-val field b))
               (make-bst (bst-widget b)
                         (insert-widget (bst-left b) field smaller?) (bst-right b))]
              [else
               (make-bst (bst-widget b)
                         (bst-left b) (insert-widget (bst-right b) field smaller?))]))]
    
    (make-db (db-field dtb) (db-lt? dtb) (db-eq? dtb)
             
             (balance (insert-widget (db-bst dtb) (db-field dtb) (db-lt? dtb)))))
)

(check-expect (db-bst (avl-insert Z1
                        (make-db widget-quantity < =
                           (make-bst D1 false (make-bst C1 false false))))) 
              (make-bst C1 (make-bst D1 false false) (make-bst Z1 false false))
              )
(check-expect (db-bst (avl-insert W1
                         (make-db widget-name string<? string=?
                           (make-bst Z1 (make-bst A1 false false) false))))
              (make-bst W1 (make-bst A1 false false) (make-bst Z1 false false)))
(check-expect (db-bst (avl-insert Z1
                         (make-db widget-quantity < =
                           (make-bst C1 (make-bst D1 false false) false))))
              
              (make-bst C1 (make-bst D1 false false) (make-bst Z1 false false)))
(check-expect (db-bst (avl-insert D1
                         (make-db widget-price < =
                            (make-bst C1 false (make-bst Z1 false false)))))
              
              (make-bst D1 (make-bst C1 false false) (make-bst Z1 false false)))
(check-expect (db-bst (avl-insert Z1
                         (make-db widget-name string<? string=? (make-bst C1 false false))))
              
              (make-bst C1 false (make-bst Z1 false false)))
(check-expect (db-bst (avl-insert Z1
                         (make-db widget-name string<? string=? false)))
              
              (make-bst Z1 false false))
(check-expect (db-bst (avl-insert A1 (make-db widget-name string<? string=?
                        (make-bst W1
                          (make-bst E1
                            (make-bst D1 false false) false)
                          (make-bst Z1 false false)))))
              
              (make-bst W1 (make-bst D1 (make-bst A1 false false)
                                     (make-bst E1 false false)) (make-bst Z1 false false)))






(define (make-avl-tree dtb low)
    (foldl avl-insert (make-db (db-field dtb) (db-lt? dtb) (db-eq? dtb) false) low))

































(check-expect (db-bst (make-avl-tree (make-db widget-quantity < = false) 
                                     (list D1 C1 Z1)))
              (make-bst C1 (make-bst D1 false false) (make-bst Z1 false false))
              )
(check-expect (db-bst (make-avl-tree (make-db widget-name string<? string=? false)
                                     
                                     (list Z1 A1 W1)))
              (make-bst W1 (make-bst A1 false false) (make-bst Z1 false false)))
(check-expect (db-bst (make-avl-tree (make-db widget-quantity < = false) 
                                     (list D1 C1 Z1)))
              (make-bst C1 (make-bst D1 false false) (make-bst Z1 false false)))
(check-expect (db-bst (make-avl-tree (make-db widget-price < = false) 
                                     (list C1 Z1 D1)))
              (make-bst D1 (make-bst C1 false false) (make-bst Z1 false false)))
(check-expect (db-bst (make-avl-tree (make-db widget-name string<? string=? false)
                                     
                                     (list C1 Z1)))
              (make-bst C1 false (make-bst Z1 false false)))
(check-expect (balance false) false) 
(check-expect (db-bst (make-avl-tree (make-db widget-name string<? string=? false) 
                                     (list W1 A1 C1 E1 D1)))
              
              (make-bst C1 (make-bst A1 false false)
                        (make-bst E1 (make-bst D1 false false) (make-bst W1 false false))))
(check-expect (db-bst (make-avl-tree (make-db widget-name string<? string=? false) 
                                     (list W1 E1 Z1 D1 A1)))
              
              (make-bst W1 (make-bst D1 (make-bst A1 false false)
                                     (make-bst E1 false false)) (make-bst Z1 false false)))


(define (insert widg dtb)
  (local [
          (define (insert-widget b field smaller?)
            (cond
              [(false? b)
               (make-bst widg false false)]
              [(smaller? (field widg) (b-val field b))
               (make-bst (bst-widget b)
                         (insert-widget (bst-left b) field smaller?) (bst-right b))]
              [else
               (make-bst (bst-widget b)
                         (bst-left b) (insert-widget (bst-right b) field smaller?))]))]
    (make-db (db-field dtb) (db-lt? dtb) (db-eq? dtb)
             (insert-widget (db-bst dtb) (db-field dtb) (db-lt? dtb))))
)




(define (make-bst-tree--insert dtb low)
  (foldr insert (make-db (db-field dtb) (db-lt? dtb) (db-eq? dtb) false) low))




(define (make-bst-tree--insert! dtb low)
  
  (if (empty? low)
      dtb 
      (begin
        (insert! (first low) dtb)
        (make-bst-tree--insert! dtb (rest low))
        )))
      

(define DB-test (make-db widget-name string<? string=? false)) 

(check-expect (db-bst (make-bst-tree--insert! DB-test (list A1 D1 E1 W1)))
              
              (make-bst A1 false (make-bst D1 false (make-bst E1 false
                                                              (make-bst W1 false false)))))
(check-expect (db-bst DB-test)
              
              (make-bst A1 false (make-bst D1 false (make-bst E1 false
                                                              (make-bst W1 false false)))))



(define (time-insert dtb)
  (local [(define widgets (random-widgets 250000 99999999))]
  (begin (time (make-bst-tree--insert dtb widgets))
         (time (make-bst-tree--insert! dtb widgets))
         
         "done"))
)









(define (find val dtb)
  (local [
          (define (find-widget k b field smaller? same?)
            (cond
              [(false? b) false]
              [(same? k (b-val field b)) (bst-widget b)]
              [(smaller? k (b-val field b)) (find-widget k (bst-left b) field smaller? same?)]
              [else
               (find-widget k (bst-right b) field smaller? same?)]))]
    (find-widget val (db-bst dtb) (db-field dtb) (db-lt? dtb) (db-eq? dtb)))
)


(define (find-all-items dtb items)
  (map (λ (dummy)
         (map (λ (item) (find ((db-field dtb) item) dtb)) items))
         (build-list 10 identity))) 



(define (time-find dtb)
  (local [(define widgets (random-widgets 10000 99999999))
          (define BST-db (make-bst-tree--insert dtb widgets))
          (define AVL-db (make-avl-tree dtb widgets))
          ]
    (begin
      (time (find-all-items BST-db widgets))
      (time (find-all-items AVL-db widgets))
      "done")))




