

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))



(require 2htdp/image)



(define-struct widget (name quantity price))




(define W1 (make-widget "W1" 1 1))
(define Y1 (make-widget "Y1" 60 2))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 2 9))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))




(define BST1 (make-bst D1 (make-bst A1 false false)
                       (make-bst Z1 false false)))
(define BST (make-bst D1 (make-bst A1 false false)
                      (make-bst Z1 false false)))
(define BST2 (make-bst D1 (make-bst A1 false false)
                       (make-bst Z1 (make-bst W1 false false) false)))

(define-struct db (field lt? eq? bst))




(define DB-quantity (make-db widget-quantity < = BST1))
(define DB-name (make-db widget-name string<? string=? BST1))
(define DB-n (make-db widget-name string<? string=? BST))






(check-expect                               
 (db-bst (insert! D1 DB-name)) BST1)

(check-expect                               
 (db-bst (insert! B1 DB-name))
 (make-bst D1 (make-bst A1 false (make-bst B1 false false))
           (make-bst Z1 false false)))

(check-expect
 (db-bst (insert! W1 DB-name))               
 (make-bst D1 (make-bst A1 false (make-bst B1 false false))
           (make-bst Z1 (make-bst W1 false false) false)))




(define (insert! x db)
  (local [
          (define (bst-insert! x db bst)
            (cond
              [(false? bst) (make-bst x false false)] 
    
              [((db-eq? db) ((db-field db) x)
                            ((db-field db) (bst-widget bst)))
               bst]  
   

              [((db-lt? db) ((db-field db) x)
                            ((db-field db) (bst-widget bst))
                            )
               (make-bst
                (bst-widget bst)
                (bst-insert! x db (bst-left bst))
                (bst-right bst))]
    
              [else 
               (make-bst
                (bst-widget bst)
                (bst-left bst)
                (bst-insert! x db (bst-right bst)))]))]
    (begin
      (set-db-bst! db (bst-insert! x db (db-bst db)))
      db) 
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









(check-expect                               
 (db-bst (insert-avl Y1 DB-quantity))
 (make-bst D1 (make-bst A1 false false)
           (make-bst Z1 false (make-bst Y1 false false))))

(check-expect                               
 (db-bst (insert-avl W1 DB-quantity))
 (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
           (make-bst Z1 false false)))

(check-expect                               
 (db-bst (insert-avl D1 DB-n)) BST)

(check-expect                               
 (db-bst (insert-avl B1 DB-n))
 (make-bst D1 (make-bst A1 false (make-bst B1 false false))
           (make-bst Z1 false false)))


(check-expect (db-bst (insert-avl W1 DB-n))  
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 (make-bst W1 false false) false )))


(define (insert-avl x db)
  (local [
          (define (insert x db bst)
            (cond
              [(false? bst) (make-bst x false false)] 
    
              [((db-eq? db) ((db-field db) x)
                            ((db-field db) (bst-widget bst)))
               bst]  
   

              [((db-lt? db) ((db-field db) x)
                            ((db-field db) (bst-widget bst))
                            )
               (make-bst
                (bst-widget bst)
                (balance (insert x db (bst-left bst)))
                (bst-right bst))]
    
              [else 
               (make-bst
                (bst-widget bst)
                (bst-left bst)
                (balance (insert x db (bst-right bst))))]))]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db) 
     (insert x db (db-bst db)))))





(check-expect (balance (make-bst A1
                                 (make-bst D1 (make-bst Z1 false false) false) false))
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))

(check-expect (balance (make-bst A1 false
                                 (make-bst D1 false (make-bst Z1 false false))))
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))

(check-expect (balance (make-bst A1
                                 (make-bst D1 false (make-bst Z1 false false)) false))
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))

(check-expect (balance (make-bst A1 false
                                 (make-bst D1 (make-bst Z1 false false) false)))
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))
(check-expect (balance (make-bst D1
                                 (make-bst A1 false false)
                                 (make-bst Z1 false false)))
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))      
                       
                       
(define (balance bst)
  (cond [(<= (abs(height-diff bst)) 1) bst]
        [(> (height-diff bst) 1)
         (cond [(false? (bst-left (bst-left bst)))(rt bst 3)]
               [else
                (rt bst 1)])]
        [(< (height-diff bst) -1)
         (cond [(false? (bst-right (bst-right bst)))(rt bst 4)]
               [else
                (rt bst 2)])]
        [else (error "How did we get here?")]))





(check-expect (rt (make-bst A1 (make-bst D1 (make-bst Z1 false false) false) false) 1)
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))
                        
(check-expect (rt (make-bst A1 false (make-bst D1 false (make-bst Z1 false false))) 2)
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))
                        
(check-expect (rt (make-bst A1 (make-bst D1 false (make-bst Z1 false false)) false) 3)
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))

(check-expect (rt (make-bst A1 false (make-bst D1 (make-bst Z1 false false) false)) 4)
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 false false)))
                        
(define (rt bst val)
  (cond
    [(equal? 1 val)
     (local [(define 1st bst)
             (define 2nd (bst-left bst))
             (define 3rd (bst-left 2nd))]
       (make-bst (bst-widget 2nd)
                 (make-bst (bst-widget 1st) false false)
                 (make-bst (bst-widget 3rd) false false)))]
    [(equal? 2 val)
     (local [(define 1st bst)
             (define 2nd (bst-right bst))
             (define 3rd (bst-right 2nd))]
       (make-bst (bst-widget 2nd)
                 (make-bst (bst-widget 1st) false false)
                 (make-bst (bst-widget 3rd) false false)))]
    [(equal? 3 val)
     (local [(define 1st bst)
             (define 2nd (bst-left bst))
             (define 3rd (bst-right 2nd))]
       (rt (make-bst (bst-widget 1st)
                     (make-bst (bst-widget 2nd) 
                               (make-bst
                                (bst-widget 3rd) false false)
                               false)false) 1))]
    [(equal? 4 val)
     (local [(define 1st bst)
             (define 2nd (bst-right bst))
             (define 3rd (bst-left 2nd))]
       (rt (make-bst (bst-widget 1st) false
                     (make-bst (bst-widget 2nd) false
                               (make-bst (bst-widget 3rd) false false))) 2))]
    (else (error "how" ))))
        
  








(check-expect
 (find "D1" DB-n) D1)  
(check-expect
 (find "D1" DB-n) (make-widget "D1" 5 5)) 
(check-expect
 (find "W1" DB-n) false) 
(check-expect
 (find "Z1" DB-n) Z1) 

(define (find x db)
  (local [(define (fn-for-wid x db)
            (cond
              [(false? (db-bst db)) false]
              
              [((db-eq? db) x ((db-field db) (bst-widget (db-bst db))))
               (bst-widget (db-bst db))]
              
              [((db-lt? db) x ((db-field db) (bst-widget (db-bst db))))
               (fn-for-wid x (make-db (db-field db)
                                      (db-lt? db) (db-eq? db)
                                      (bst-left (db-bst db))))]
              
              [(not ((db-lt? db) x ((db-field db)(bst-widget (db-bst db)))))
               (fn-for-wid x (make-db (db-field db)
                                      (db-lt? db) (db-eq? db)
                                      (bst-right (db-bst db))))]
              
              [else (error "How did we get here?")]))]

    (fn-for-wid x db)))







(check-expect                               
 (db-bst (insert D1 DB-n)) BST1)

(check-expect                               
 (db-bst (insert B1 DB-n))
 (make-bst D1 (make-bst A1 false (make-bst B1 false false))
           (make-bst Z1 false false)))


(check-expect (db-bst (insert W1 DB-n))  
              (make-bst D1 (make-bst A1 false false)
                        (make-bst Z1 (make-bst W1 false false) false )))

(define (insert x db)
  (local [
          (define (bst-insert x db bst)
            (cond
              [(false? bst) (make-bst x false false)] 
    
              [((db-eq? db) ((db-field db) x)
                            ((db-field db) (bst-widget bst)))
               bst]  
   

              [((db-lt? db) ((db-field db) x)
                            ((db-field db) (bst-widget bst))
                            )
               (make-bst
                (bst-widget bst)
                (bst-insert x db (bst-left bst))
                (bst-right bst))]
    
              [else 
               (make-bst
                (bst-widget bst)
                (bst-left bst)
                (bst-insert x db (bst-right bst)))]))]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (bst-insert x db (db-bst db)))
    ))





(check-expect
 (create-bst (list W1 Y1 Z1  A1  B1  D1) insert!)
 (make-bst
  (make-widget "W1" 1 1)
  (make-bst (make-widget "A1" 2 3) #false
            (make-bst (make-widget "B1" 2 9) #false
                      (make-bst (make-widget "D1" 5 5) #false #false)))
  (make-bst (make-widget "Y1" 60 2) #false
            (make-bst (make-widget "Z1" 51 16) #false #false))))

(define (create-bst low fn? )
  (local [(define (fn-for-wid low fn? db)
            (cond [(empty? low) (db-bst db)]
                  [else
                   (fn-for-wid (rest low) fn? (fn? (first low) db))]))]
    (fn-for-wid low fn? (make-db widget-name string<? string=?
                                 (make-bst W1 false false)))))





(define (find-all low fn? bst)
  (local
    [(define (fn-for-wid low fn? db found)
       (cond [(empty? low) found]
             [else
              (fn-for-wid
               (rest low) fn? db
               (append (list (fn? (widget-name (first low)) db)) found))]))]
    (fn-for-wid low fn? (make-db widget-name string<? string=? bst) (list "" ))))









(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))

(define 250k (random-widgets 250000 100))

(begin (time (create-bst 250k insert)) "Insert done")
(begin (time (create-bst 250k insert!)) "Insert! done")





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
(define 10k  (random-widgets-string 10000 10 100))
(define cbst (create-bst  10k insert!))
(define cavl (create-bst  10k insert-avl))

(begin (time (find-all 10k find cbst)) "Find BST done")
(begin (time (find-all 10k find cavl)) "Find AVL Tree done ") 
  




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

