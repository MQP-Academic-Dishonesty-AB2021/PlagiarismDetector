

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))




(require 2htdp/image)

(define-struct widget (name quantity price))


(define W1 (make-widget "W1" 1 1))
(define W1.1 (make-widget "W1" 0 .5))
(define F1 (make-widget "F1" 19 13))
(define E1 (make-widget "E1" 19 13))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define M1 (make-widget "M1" 50 50))

(define-struct bst (widget left right))



(define W1-NODE (make-bst W1 false false))
(define W1-BST (make-bst W1 false (make-bst W1.1 false false)))
(define W1-BST-FLIP (make-bst W1.1 false (make-bst W1 false false)))
(define Z1-NODE (make-bst Z1 false false))
(define ALPHABET (make-bst W1 
                           (make-bst A1 false (make-bst D1 false false))
                           (make-bst Z1 false false)))
(define QUANTABET (make-bst A1 
                            (make-bst W1 false false)
                            (make-bst D1 false (make-bst Z1 false false))))
(define LEFT-LEFT-TOP (make-bst W1 
                                (make-bst F1
                                          (make-bst D1
                                                    (make-bst A1 false false) false) false)
                                (make-bst Z1 false false)))
(define LEFT-RIGHT-TOP (make-bst W1 
                                 (make-bst F1
                                           (make-bst D1 false
                                                     (make-bst E1 false false)) false)
                                 (make-bst Z1 false false)))
(define RIGHT-RIGHT-TOP (make-bst W1 
                                  (make-bst D1 false
                                            (make-bst E1 false
                                                      (make-bst F1 false false)))
                                  (make-bst Z1 false false)))
(define RIGHT-LEFT-TOP (make-bst W1 
                                 (make-bst D1 false
                                           (make-bst F1
                                                     (make-bst E1 false false) false))
                                 (make-bst Z1 false false)))

(define LEFT-LEFT-INSERT-A (make-bst W1 
                                     (make-bst F1
                                               (make-bst D1 false false) false)
                                     (make-bst Z1 false false)))
(define LEFT-RIGHT-INSERT-E (make-bst W1 
                                      (make-bst F1
                                                (make-bst D1 false false) false)
                                      (make-bst Z1 false false)))
(define RIGHT-RIGHT-INSERT-F (make-bst W1 
                                       (make-bst D1
                                                 false (make-bst E1 false false))
                                       (make-bst Z1 false false)))
(define RIGHT-LEFT-INSERT-E (make-bst W1 
                                      (make-bst D1
                                                false (make-bst F1 false false))
                                      (make-bst Z1 false false)))








(define-struct db (field lt? eq? bst))
(define DB-NAME (make-db widget-name string<? string=? false))
(define DB-NAME-2 (make-db widget-name string<? string=? ALPHABET))
(define DB-NAME-W0 (make-db widget-name string<? string=? W1-NODE))
(define DB-NAME-W1 (make-db widget-name string<? string=? W1-BST))
(define DB-NAME-W1-FLIP (make-db widget-name string<? string=? W1-BST-FLIP))
(define DB-QUANTITY (make-db widget-quantity < = false))
(define DB-QUANTITY-2 (make-db widget-quantity < = QUANTABET))
(define DB-NAME-LL (make-db widget-name string<? string=? LEFT-LEFT-INSERT-A))
(define DB-NAME-LR (make-db widget-name string<? string=? LEFT-RIGHT-INSERT-E))
(define DB-NAME-RR (make-db widget-name string<? string=? RIGHT-RIGHT-INSERT-F))
(define DB-NAME-RL (make-db widget-name string<? string=? RIGHT-LEFT-INSERT-E))







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






 
(check-expect (find "" DB-NAME) false) 
(check-expect (find "" DB-NAME-2) false) 
(check-expect (find "W1" DB-NAME-2) W1) 
(check-expect (find 51 DB-QUANTITY-2) Z1) 
(check-expect (find "W1" DB-NAME-W1) W1) 
(check-expect (find "W1" DB-NAME-W1-FLIP) W1.1) 
(check-expect (find 0 DB-QUANTITY-2) false) 

  (define (find x db) 
    (local [
            
            
            (define (left? x db)
              ((db-lt? db) x ((db-field db) (bst-widget (db-bst db)))))
            
            
            
            (define (dbequal? x db)
              ((db-eq? db) ((db-field db) (bst-widget (db-bst db))) x))]
      (cond [(false? (db-bst db)) false]
            [(dbequal? x db) (bst-widget (db-bst db))]
            [(left? x db) (find x (make-db
                                   (db-field db)
                                   (db-lt? db)
                                   (db-eq? db)
                                   (bst-left (db-bst db))))]
            [else 
             (find x (make-db 
                      (db-field db)
                      (db-lt? db)
                      (db-eq? db)
                      (bst-right (db-bst db))))])))

  

  
  
  
  (define (left? widget db bst) 
    ((db-lt? db) ((db-field db) widget)
                 ((db-field db) (bst-widget bst))))

  (check-expect (left? W1.1 DB-QUANTITY-2 QUANTABET) true)
  (check-expect (left? Z1 DB-NAME-W0 W1-NODE) false)
  (check-expect (left? W1.1 DB-NAME-W0 W1-NODE) false)
  

  (define (insert widget db)
    (local [(define (insert-abstract bst)
              (cond [(false? bst) (make-bst widget false false)]
                    
                    [(left? widget db bst)
                     
                     (make-bst 
                      (bst-widget bst)
                      (insert-abstract (bst-left bst))
                      (bst-right bst))]
                    [else 
                     
                     (make-bst 
                      (bst-widget bst)
                      (bst-left bst)
                      (insert-abstract (bst-right bst)))]))]
      (make-db (db-field db)
               (db-lt? db)
               (db-eq? db)
               (insert-abstract (db-bst db)))))

  
               
  
  
  
  

  (check-expect (db-bst (insert! W1.1 DB-NAME-W0)) W1-BST)
  
  (check-expect (db-bst (insert! W1 DB-NAME))
                (make-bst W1 false false)) 
  (check-expect (db-bst (insert! (make-widget "" 0 0) DB-NAME-2)) 
                (make-bst W1 
                          (make-bst A1 
                                    (make-bst (make-widget "" 0 0) false false)
                                    (make-bst D1 false false))
                          (make-bst Z1 false false)))
  

  (define (insert! widget db)
    (local [(define (insert!-in bst prior assign!)
              (cond
                [(false? bst)
                 (begin (assign! prior
                                 (make-bst widget false false))
                        db)]
                [(left? widget db bst)
                 (insert!-in
                  (bst-left bst)
                  bst
                  set-bst-left!)]
                [else 
                 (insert!-in
                  (bst-right bst)
                  bst
                  set-bst-right!)]))]
                
      (insert!-in (db-bst db) db set-db-bst!)))

  

  
  
  
  (define (insert-avl widget db)
    (local [(define (insert-avl-abstract bst)
              (cond [(false? bst)
                     (make-bst widget false false)]
                    
                    [((db-lt? db) ((db-field db) widget)
                                  ((db-field db) (bst-widget bst)))
                     
                     (make-bst 
                      (bst-widget bst)
                      (balance (insert-avl-abstract (bst-left bst)))
                      (bst-right bst))]
                    [else 
                     
                     (make-bst 
                      (bst-widget bst)
                      (bst-left bst)
                      (balance (insert-avl-abstract (bst-right bst))))]))]
      (make-db (db-field db)
               (db-lt? db)
               (db-eq? db)
               (balance (insert-avl-abstract (db-bst db))))))

  (check-expect (db-bst (insert-avl A1
                                    (make-db widget-name string<? string=? false)))
                (make-bst A1 false false))
  (check-expect (db-bst  (insert-avl A1 DB-NAME-LL)) 
                (make-bst W1
                          (make-bst D1
                                    (make-bst A1 false false) (make-bst F1 false false))
                          (make-bst Z1 false false)))
  (check-expect (db-bst (insert-avl E1 DB-NAME-LR)) 
                (make-bst W1
                          (make-bst E1
                                    (make-bst D1 false false) (make-bst F1 false false))
                          (make-bst Z1 false false)))
  (check-expect (db-bst (insert-avl F1 DB-NAME-RR)) 
                (make-bst W1
                          (make-bst E1
                                    (make-bst D1 false false) (make-bst F1 false false))
                          (make-bst Z1 false false)))
  (check-expect (db-bst (insert-avl E1 DB-NAME-RL)) 
                (make-bst W1
                          (make-bst E1
                                    (make-bst D1 false false) (make-bst F1 false false))
                          (make-bst Z1 false false)))
                        

  
  
  
  (define (balance bst) 
    (local [(define (left-rotate bst)
              (make-bst 
               (bst-widget (bst-right bst))
               (make-bst
                        (bst-widget bst)
                        (bst-left bst)
                        (bst-left (bst-right bst)))
               (bst-right (bst-right bst))))
            
            (define (right-rotate bst)
              (make-bst 
               	(bst-widget (bst-left bst))
                           (bst-left (bst-left bst))
                           (make-bst 
                            (bst-widget bst)
                            (bst-right (bst-left bst))
                            (bst-right bst))))
            
            (define (left-left bst)
              (right-rotate bst))
               
            (define (left-right bst)
               (right-rotate
                  (make-bst (bst-widget bst)
                            (left-rotate (bst-left bst))
                            (bst-right bst))))
            
            (define (right-right bst)
              (left-rotate bst))
               
            (define (right-left bst)
              (left-rotate 
               (make-bst
                (bst-widget bst)
                (bst-left bst)
                (right-rotate (bst-right bst)))))]
     
      (cond [(= (height-diff bst) 2)
             (cond [(= (height-diff (bst-left bst)) 1)
                    (left-left bst)]
                   [(= (height-diff (bst-left bst)) -1)
                    (left-right bst)]
                   [else 
                    (error "Left - Some assumption the programmer made
                                  about what will be passed into balance was
                                  not fulfilled.")])]
            [(= (height-diff bst) -2)
             (cond [(= (height-diff (bst-right bst)) 1)
                    (right-left bst)]
                   [(= (height-diff (bst-right bst)) -1)
                    (right-right bst)]
                   [else 
                    (error "Right - Some assumption the programmer made
                                  about what will be passed into balance was
                                  not fulfilled.")])]
            [(> (abs (height-diff bst)) 2)
             (error "A tree was passed to balance that was not the most
                           fundamental problem.")]
            [else 
             bst])))

  (check-expect (balance ALPHABET) ALPHABET)        
  (check-expect (balance (bst-left LEFT-LEFT-TOP))  
                (make-bst D1 (make-bst A1 false false) (make-bst F1 false false))) 
  (check-expect (balance (bst-left LEFT-RIGHT-TOP)) 
                (make-bst E1 (make-bst D1 false false) (make-bst F1 false false)))
  (check-expect (balance (bst-left RIGHT-RIGHT-TOP))
                (make-bst E1 (make-bst D1 false false) (make-bst F1 false false)))
  (check-expect (balance (bst-left RIGHT-LEFT-TOP)) 
                (make-bst E1 (make-bst D1 false false) (make-bst F1 false false)))

  
  
  
  
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

  

  
  (define tests (random-widgets 250000 9999999))
  (define empty-db (make-db widget-name string<? string=? false))
  
  (define (time-insert)
    (begin (set-db-bst! empty-db false)
           (time (foldl insert empty-db tests))
           "done"))

  (define (time-insert!)
    (begin (set-db-bst! empty-db false)
           (time (foldl insert! empty-db tests))
           "done"))


 
  (define tests2 (random-widgets 10000 99999))
  (define test-db (foldl insert empty-db tests2))

  (define (time-find--bst)
    (begin (time (map (λ (x) (find (widget-name x) test-db)) tests2))
           "done"))

  (define test-avl-db (foldl insert-avl empty-db tests2))

  (define (time-find--avl)
    (begin (time (map (λ (x) (find (widget-name x) test-avl-db)) tests2))
           "done"))