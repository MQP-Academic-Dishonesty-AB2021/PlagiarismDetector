

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


(define-struct widget (name quantity price))




(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 1 1010101))
(define C1 (make-widget "C1" 10 50))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 9 20))
(define V1 (make-widget "V1" 100 100))
(define W1 (make-widget "W1" 1 1))
(define X1 (make-widget "X1" 200 70))
(define Y1 (make-widget "Y1" 52 60))
(define Z1 (make-widget "Z1" 51 16))
(define free-stuff (make-widget "free stuff" 0 0))

(define-struct bst (widget left right))






(define EMPTY false)

(define NO-CHILDREN (make-bst D1 false false))

(define BST1 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst Z1
                                 (make-bst W1 false false)
                                 false)))

(define BST2 (make-bst D1
                       (make-bst A1 false false)
                       (make-bst Z1 false false)))

(define LEFT (make-bst Z1
                       (make-bst W1
                                 (make-bst D1
                                           (make-bst A1 false false)
                                           false)
                                 false)
                       false))

(define RIGHT (make-bst A1
                        false
                        (make-bst D1
                                  false
                                  (make-bst W1
                                            false
                                            (make-bst Z1 false false)))))


(define BST1Q (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1 false false)
                                  false)))

(define RIGHTQ (make-bst W1
                         false
                         (make-bst A1
                                   false
                                   (make-bst D1
                                             false
                                             (make-bst Z1 false false)))))


(define BST1P (make-bst A1
                        (make-bst W1 false false)
                        (make-bst V1
                                  (make-bst Y1 false false)
                                  false)))

(define LEFTP (make-bst V1
                        (make-bst Y1
                                  (make-bst D1
                                            (make-bst W1 false false)
                                            false)
                                  false)
                        false))


(define-struct db (field lt? eq? bst))


(define DB1 (make-db widget-name string<? string=? false)) 
(define DB2 (make-db widget-quantity < = BST1Q)) 
(define DB3 (make-db widget-price < = BST1P)) 
(define DB4 (make-db widget-name string<? string=? BST1)) 













(define BAVL1 DB1)
(define BAVL2 (make-db widget-name string<? string=? NO-CHILDREN))
(define BAVL3 (make-db widget-quantity
                       <
                       =
                       (make-bst A1
                                 (make-bst W1 false false)
                                 (make-bst D1
                                           false
                                           (make-bst X1 false false)))))


(define LLAVL1 (make-db widget-name
                        string<?
                        string=?
                        (make-bst X1
                                  (make-bst D1
                                            (make-bst A1 false false)
                                            false)
                                  false)))

(define LLAVL1B (make-db widget-name
                         string<?
                         string=?
                         (make-bst D1
                                   (make-bst A1 false false)
                                   (make-bst X1 false false))))

(define LLAVL2 (make-db widget-quantity
                        <
                        =
                        (make-bst V1
                                  (make-bst D1
                                            (make-bst A1
                                                      (make-bst W1 false false)
                                                      false)
                                            false)
                                  (make-bst X1 false false))))

(define LLAVL2B (make-db widget-quantity
                         <
                         =
                         (make-bst V1
                                   (make-bst A1
                                             (make-bst W1 false false)
                                             (make-bst D1 false false))
                                   (make-bst X1 false false))))

(define LLAVL3 (make-db widget-price
                        <
                        =
                        (make-bst Y1
                                  (make-bst Z1
                                            (make-bst D1
                                                      (make-bst A1 false false)
                                                      false)
                                            false)
                                  (make-bst X1
                                            false
                                            (make-bst V1 false false)))))

(define LLAVL3B (make-db widget-price
                         <
                         =
                         (make-bst Y1
                                   (make-bst D1
                                             (make-bst A1 false false)
                                             (make-bst Z1 false false))
                                   (make-bst X1
                                             false
                                             (make-bst V1 false false)))))


(define LRAVL1 (make-db widget-quantity
                        <
                        =
                        (make-bst V1
                                  (make-bst D1
                                            false
                                            (make-bst Z1 false false))
                                  false)))

(define LRAVL1B (make-db widget-quantity
                         <
                         =
                         (make-bst Z1
                                   (make-bst D1 false false)
                                   (make-bst V1 false false))))

(define LRAVL2 (make-db widget-price
                        <
                        =
                        (make-bst Y1
                                  (make-bst Z1
                                            (make-bst W1
                                                      false
                                                      (make-bst A1 false false))
                                            false)
                                  (make-bst V1 false false))))

(define LRAVL2B (make-db widget-price
                         <
                         =
                         (make-bst Y1
                                   (make-bst A1
                                             (make-bst W1 false false)
                                             (make-bst Z1 false false))
                                   (make-bst V1 false false))))


(define LRAVL3 (make-db widget-name
                        string<?
                        string=?
                        (make-bst Y1
                                  (make-bst A1
                                            false
                                            (make-bst W1
                                                      (make-bst D1 false false)
                                                      (make-bst X1 false false)))
                                  (make-bst Z1 false false))))

(define LRAVL3B (make-db widget-name
                         string<?
                         string=?
                         (make-bst W1
                                   (make-bst D1
                                             false
                                             (make-bst A1 false false))
                                   (make-bst Y1
                                             (make-bst X1 false false)
                                             (make-bst Z1 false false)))))


(define RLAVL1 (make-db widget-quantity
                        <
                        =
                        (make-bst D1
                                  false
                                  (make-bst X1
                                            (make-bst Z1 false false)
                                            false))))

(define RLAVL1B (make-db widget-quantity
                         <
                         +
                         (make-bst Z1
                                   (make-bst D1 false false)
                                   (make-bst X1 false false))))

(define RLAVL2 (make-db widget-name
                        string<?
                        string=?
                        (make-bst D1
                                  (make-bst A1 false false)
                                  (make-bst W1
                                            false
                                            (make-bst Y1
                                                      (make-bst X1 false false)
                                                      false)))))

(define RLAVL2B (make-db widget-name
                         string<?
                         string=?
                         (make-bst D1
                                   (make-bst A1 false false)
                                   (make-bst X1
                                             (make-bst W1 false false)
                                             (make-bst Y1 false false)))))


(define RLAVL3 (make-db widget-price
                        <
                        =
                        (make-bst A1
                                  (make-bst W1 false false)
                                  (make-bst V1
                                            (make-bst Y1
                                                      (make-bst D1 false false)
                                                      (make-bst X1 false false))
                                            false))))

(define RLAVL3B (make-db widget-price
                         <
                         =
                         (make-bst Y1
                                   (make-bst A1
                                             false
                                             (make-bst W1 false false))
                                   (make-bst X1
                                             (make-bst D1 false false)
                                             (make-bst V1 false false)))))


(define RRAVL1 (make-db widget-price
                        <
                        =
                        (make-bst A1
                                  false
                                  (make-bst Y1
                                            false
                                            (make-bst V1 false false)))))

(define RRAVL1B (make-db widget-price
                         <
                         =
                         (make-bst Y1
                                   (make-bst A1 false false)
                                   (make-bst V1 false false))))

(define RRAVL2 (make-db widget-name
                        string<?
                        string=?
                        (make-bst D1
                                  (make-bst A1 false false)
                                  (make-bst X1
                                            false
                                            (make-bst Y1
                                                      false
                                                      (make-bst Z1 false false))))))

(define RRAVL2B (make-db widget-name
                         string<?
                         string=?
                         (make-bst D1
                                   (make-bst A1 false false)
                                   (make-bst Y1
                                             (make-bst X1 false false)
                                             (make-bst Z1 false false)))))


(define RRAVL3 (make-db widget-quantity
                        string<?
                        string=?
                        (make-bst Z1
                                  (make-bst D1
                                            (make-bst W1 false false)
                                            false)
                                  (make-bst Y1
                                            false
                                            (make-bst V1
                                                      false
                                                      (make-bst X1 false false))))))

(define RRAVL3B (make-db widget-quantity
                         string<?
                         string=?
                         (make-bst Z1
                                   (make-bst D1
                                             (make-bst W1 false false)
                                             false)
                                   (make-bst V1
                                             (make-bst Y1 false false)
                                             (make-bst X1 false false)))))








(define (random-widgets num max)
  (build-list num
              (λ (dummy)
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











(check-expect (db-bst 
               (insert! D1 (make-db widget-name string<? string=? EMPTY)))
              (make-bst D1 false false))

(check-expect (db-bst 
               (insert! A1 (make-db widget-name
                                    string<?
                                    string=?
                                    (make-bst D1 false false)))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert! W1 (make-db widget-name
                                    string<?
                                    string=?
                                    (make-bst D1 false false))))
              (make-bst D1 false (make-bst W1 false false)))

(check-expect (db-bst 
               (insert! V1 (make-db widget-name
                                    string<?
                                    string=?
                                    (make-bst D1
                                              (make-bst A1 false false)
                                              (make-bst Z1
                                                        (make-bst W1 false false)
                                                        false)))))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Z1
                                  (make-bst W1
                                            (make-bst V1 false false)
                                            false)
                                  false)))

(check-expect (db-bst 
               (insert! Y1 (make-db widget-name
                                    string<?
                                    string=?
                                    (make-bst D1
                                              (make-bst A1 false false)
                                              (make-bst Z1
                                                        (make-bst W1 false false)
                                                        false)))))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Z1
                                  (make-bst W1
                                            false
                                            (make-bst Y1 false false))
                                  false)))


(check-expect (db-bst 
               (insert! D1 (make-db widget-quantity < = EMPTY)))
              (make-bst D1 false false))

(check-expect (db-bst 
               (insert! A1 (make-db widget-quantity < = (make-bst D1 false false)))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert! Z1 (make-db widget-quantity < = (make-bst D1 false false))))
              (make-bst D1 false (make-bst Z1 false false)))

(check-expect (db-bst 
               (insert! free-stuff (make-db widget-quantity
                                            <
                                            =
                                            (make-bst A1
                                                      (make-bst W1 false false)
                                                      (make-bst Z1
                                                                (make-bst D1 false false)
                                                                false)))))
              (make-bst A1
                        (make-bst W1
                                  (make-bst free-stuff false false)
                                  false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            false)
                                  false)))

(check-expect (db-bst 
               (insert! V1
                        (make-db widget-quantity
                                 <
                                 =
                                 (make-bst A1
                                           (make-bst W1 false false)
                                           (make-bst Z1
                                                     (make-bst D1 false false)
                                                     false)))))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            false)
                                  (make-bst V1 false false))))


(check-expect (db-bst 
               (insert! D1 (make-db widget-price < = EMPTY)))
              (make-bst D1 false false))

(check-expect (db-bst 
               (insert! A1 (make-db widget-price < = (make-bst D1 false false)))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert! Y1 (make-db widget-price < = (make-bst D1 false false))))
              (make-bst D1 false (make-bst Y1 false false)))

(check-expect (db-bst 
               (insert! Z1
                        (make-db widget-price
                                 <
                                 =
                                 (make-bst A1
                                           (make-bst W1 false false)
                                           (make-bst V1
                                                     (make-bst Y1 false false)
                                                     false)))))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst V1
                                  (make-bst Y1
                                            (make-bst Z1 false false)
                                            false)
                                  false)))

(check-expect (db-bst 
               (insert! X1 (make-db widget-price
                                    <
                                    =
                                    (make-bst A1
                                              (make-bst W1 false false)
                                              (make-bst V1
                                                        (make-bst Y1 false false)
                                                        false)))))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst V1
                                  (make-bst Y1
                                            false
                                            (make-bst X1 false false))
                                  false)))


(define (insert! widget db)
  (local [(define new-widget (make-bst widget false false))]
    (local [(define (make-new-bst bst parent pos)
              (begin
                (cond
                  [(false? bst) (cond [(= pos 0) (set-db-bst! db (make-bst widget false false))]
                                      [(= pos -1) (set-bst-left! parent new-widget)]
                                      [(= pos 1) (set-bst-right! parent new-widget)])]
                  
                  [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
                   (make-new-bst (bst-left bst) bst -1)]
                  
                  [else (make-new-bst (bst-right bst) bst 1)])
                (db-bst db)))]

      (make-db (db-field db)
               (db-lt? db)
               (db-eq? db)
               (make-new-bst (db-bst db) false 0)))))









(check-expect (db-bst 
               (insert-avl D1 (make-db widget-name string<? string=? EMPTY))) NO-CHILDREN)

(check-expect (db-bst 
               (insert-avl A1 (make-db widget-name string<? string=? NO-CHILDREN))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert-avl W1 (make-db widget-name string<? string=? NO-CHILDREN)))
              (make-bst D1 false (make-bst W1 false false)))

(check-expect (db-bst 
               (insert-avl V1 (make-db widget-name string<? string=? BST1)))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1
                                  (make-bst V1 false false)
                                  (make-bst Z1 false false))))

(check-expect (db-bst 
               (insert-avl Y1 (make-db widget-name string<? string=? BST1)))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Y1
                                  (make-bst W1 false false)
                                  (make-bst Z1 false false))))


(check-expect (db-bst 
               (insert-avl D1 (make-db widget-quantity < = EMPTY))) NO-CHILDREN)

(check-expect (db-bst 
               (insert-avl A1 (make-db widget-quantity < = NO-CHILDREN))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert-avl Z1 (make-db widget-quantity < = NO-CHILDREN)))
              (make-bst D1 false (make-bst Z1 false false)))

(check-expect (db-bst 
               (insert-avl free-stuff (make-db widget-quantity < = BST1Q)))
              (make-bst A1
                        (make-bst W1
                                  (make-bst free-stuff false false)
                                  false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            false)
                                  false)))

(check-expect (db-bst 
               (insert-avl V1 (make-db widget-quantity < = BST1Q)))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            false)
                                  (make-bst V1 false false))))


(check-expect (db-bst 
               (insert-avl D1 (make-db widget-price < = EMPTY))) NO-CHILDREN)

(check-expect (db-bst 
               (insert-avl A1 (make-db widget-price < = NO-CHILDREN))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert-avl Y1 (make-db widget-price < = NO-CHILDREN)))
              (make-bst D1 false (make-bst Y1 false false)))

(check-expect (db-bst 
               (insert-avl Z1 (make-db widget-price < = BST1P)))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Y1
                                  (make-bst Z1 false false)
                                  (make-bst V1 false false))))

(check-expect (db-bst 
               (insert-avl X1 (make-db widget-price < = BST1P)))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst X1
                                  (make-bst Y1 false false)
                                  (make-bst V1 false false))))


(define (insert-avl widget db)
  (local [(define (make-new-bst bst)
            
            (cond [(false? bst) (make-bst widget false false)] 
                  [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
                   (make-bst (bst-widget bst) (make-new-bst (bst-left bst)) (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst) (bst-left bst) (make-new-bst (bst-right bst)))]))]

    
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (balance (make-new-bst (db-bst db))))))














(check-expect (balance (db-bst BAVL1)) (db-bst BAVL1)) 


(check-expect (balance (db-bst BAVL2)) (db-bst BAVL2))
(check-expect (balance (db-bst BAVL3)) (db-bst BAVL3))


(check-expect (balance (db-bst LLAVL1)) (db-bst LLAVL1B))
(check-expect (balance (db-bst LLAVL2)) (db-bst LLAVL2B))
(check-expect (balance (db-bst LLAVL3)) (db-bst LLAVL3B))


(check-expect (balance (db-bst LRAVL1)) (db-bst LRAVL1B))
(check-expect (balance (db-bst LRAVL2)) (db-bst LRAVL2B))
(check-expect (balance (db-bst LRAVL3)) (db-bst LRAVL3B))


(check-expect (balance (db-bst RLAVL1)) (db-bst RLAVL1B))
(check-expect (balance (db-bst RLAVL2)) (db-bst RLAVL2B))
(check-expect (balance (db-bst RLAVL3)) (db-bst RLAVL3B))


(check-expect (balance (db-bst RRAVL1)) (db-bst RRAVL1B))
(check-expect (balance (db-bst RRAVL2)) (db-bst RRAVL2B))
(check-expect (balance (db-bst RRAVL3)) (db-bst RRAVL3B))


(define (balance bst)
  
  (local [(define (rotate-left-left parent)
            (local [(define unbalanced (bst-left parent))]
              (make-bst (bst-widget unbalanced) 
                        (bst-left unbalanced)
                        (make-bst (bst-widget parent) 
                                  (bst-right unbalanced)
                                  (bst-right parent)))))

          (define (rotate-right-right parent)
            (local [(define unbalanced (bst-right parent))]
              (make-bst (bst-widget unbalanced) 
                        (make-bst (bst-widget parent) 
                                  (bst-left unbalanced)
                                  (bst-left parent))
                        (bst-right unbalanced))))
          
          (define (rotate-left-right bst)
            (rotate-left-left (make-bst (bst-widget bst)
                                        (rotate-right-right (bst-left bst))
                                        (bst-right bst))))

          (define (rotate-right-left bst)
            (rotate-right-right (make-bst (bst-widget bst)
                                          (bst-left bst)
                                          (rotate-left-left (bst-right bst)))))

          
          (define (balance-single-node bst)
            (cond

              
              [(>= (height-diff bst) 2)
               (if (>= (height-diff (bst-left bst)) 1)
                   (rotate-left-left bst)
                   (rotate-left-right bst))]

              
              [(<= (height-diff bst) -2)
               (if (<= (height-diff (bst-right bst)) -1)
                   (rotate-right-right bst)
                   (rotate-right-left bst))]

              
              [else bst]))]

    
    (cond [(false? bst) false]
          [else
           (balance-single-node
            (make-bst (bst-widget bst)
                      (balance (balance-single-node (bst-left bst)))
                      (balance (balance-single-node (bst-right bst)))))])))




(define (find key db)
  (local [(define bst (db-bst db))
          (define field (db-field db))]
    
    (cond [(false? bst) false]
          [else (cond [((db-eq? db) key (field (bst-widget bst))) (bst-widget bst)]
                      [((db-lt? db) key (field (bst-widget bst)))
                       (find key (make-db field
                                          (db-lt? db)
                                          (db-eq? db)
                                          (bst-left bst)))]
                      [else (find key (make-db field
                                               (db-lt? db)
                                               (db-eq? db)
                                               (bst-right bst)))])])))


(define (insert widget db)
  (local [(define (make-new-bst bst)
            (cond 
              [(false? bst) (make-bst widget false false)] 
              [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
               (make-bst (bst-widget bst) (make-new-bst (bst-left bst)) (bst-right bst))]
              [else
               (make-bst (bst-widget bst) (bst-left bst) (make-new-bst (bst-right bst)))]))]
    
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (make-new-bst (db-bst db)))))






(define EMPTY-DB (make-db widget-price < = false))









(define (time-insert)
  (local [(define (test db fn)
            (local [(define low (random-widgets 250000 999999))] 
              (local [(define (test-in db low)
                        (cond [(empty? low) db] 
                              [else 
                               (fn (first low) (test-in db (rest low)))]))]
                (test-in db low))))

          
          (define (test-insert)
            (begin (time (test EMPTY-DB insert))
                   (time (test EMPTY-DB insert))
                   (time (test EMPTY-DB insert))
                   (time (test EMPTY-DB insert))
                   (time (test EMPTY-DB insert))
                   "done"))

          
          
          (define (test-insert!)
            (begin (time (test (make-db widget-price < = false) insert!))
                   (time (test (make-db widget-price < = false) insert!))
                   (time (test (make-db widget-price < = false) insert!))
                   (time (test (make-db widget-price < = false) insert!))
                   (time (test (make-db widget-price < = false) insert!))
                   "done"))]
    
    (begin (test-insert)
           (test-insert!)
           "Done with insert and insert!")))








(define (time-find)
  (local [(define LOW (random-widgets 10000 999999))

          
          
          
          (define (mega-insert fn)
            (local [(define db (make-db widget-price < = false))
                    (define (inner db LOW)
                      (cond [(empty? LOW) db] 
                            [else (fn (first LOW) (inner db (rest LOW)))]))]
              (inner db LOW)))

          (define LOW-BST (mega-insert insert))
          (define LOW-AVL (mega-insert insert-avl))

          
          (define (test-find db low)
            (cond [(empty? low) db]
                  [else (begin
                          (find ((db-field db) (first low)) db)
                          (test-find db (rest low)))]))

          
          (define (test-insert-bst-vs-avl)
            (begin 
              (time (test-find LOW-BST LOW))
              (time (test-find LOW-AVL LOW))
              "Done"))]

    
    (begin
      (test-insert-bst-vs-avl)
      (test-insert-bst-vs-avl)
      (test-insert-bst-vs-avl)
      (test-insert-bst-vs-avl)
      (test-insert-bst-vs-avl)
      "Done with find in bst vs avl")))



















(check-expect (balance! (db-bst BAVL1)) (db-bst BAVL1)) 


(check-expect (balance! (db-bst BAVL2)) (db-bst BAVL2))
(check-expect (balance! (db-bst BAVL3)) (db-bst BAVL3))


(check-expect (balance! (db-bst LLAVL1)) (db-bst LLAVL1B))
(check-expect (balance! (db-bst LLAVL2)) (db-bst LLAVL2B))


(check-expect (balance! (db-bst LRAVL1)) (db-bst LRAVL1B))
(check-expect (balance! (db-bst LRAVL2)) (db-bst LRAVL2B))


(check-expect (balance! (db-bst RLAVL1)) (db-bst RLAVL1B))
(check-expect (balance! (db-bst RLAVL2)) (db-bst RLAVL2B))


(check-expect (balance! (db-bst RRAVL1)) (db-bst RRAVL1B))
(check-expect (balance! (db-bst RRAVL2)) (db-bst RRAVL2B))


(define (balance! bst)
  
  (local [(define (rotate-left-left! root)
            (local [(define old-root (make-bst (bst-widget root)
                                               (bst-left root)
                                               (bst-right root))) 
                    (define root-left-right (bst-right (bst-left old-root)))]
              (begin
                (set-bst-widget! root (bst-widget (bst-left root)))
                (set-bst-left! root (bst-left (bst-left old-root)))
                (set-bst-right! root old-root)
                (set-bst-left! (bst-right root)  root-left-right)
                root)))

          (define (rotate-right-right! root)
            (local [(define old-root (make-bst (bst-widget root)
                                               (bst-left root)
                                               (bst-right root))) 
                    (define root-right-left (bst-left (bst-right old-root)))]
              (begin
                (set-bst-widget! root (bst-widget (bst-right root)))
                (set-bst-right! root (bst-right (bst-right old-root)))
                (set-bst-left! root old-root)
                (set-bst-right! (bst-left root)  root-right-left)
                root)))
          
          (define (rotate-left-right! bst)
            (rotate-left-left! (begin (set-bst-left! bst (rotate-right-right! (bst-left bst)))
                                      bst)))

          (define (rotate-right-left! bst)
            (rotate-right-right! (begin set-bst-right! (rotate-left-left! (bst-right bst))
                                        bst)))

          
          (define (balance-singular-node! bst)
            (cond [(>= (height-diff bst) 2)
                   (if (>= (height-diff (bst-left bst)) 1)
                       (rotate-left-left! bst)
                       (rotate-left-right! bst))]
          
                  [(<= (height-diff bst) -2)
                   (if (<= (height-diff (bst-right bst)) -1)
                       (rotate-right-right! bst)
                       (rotate-right-left! bst))]
          
                  [else bst]))]
    
    
    (cond [(false? bst) false]
          [else
           (balance-singular-node!
            (begin (set-bst-widget! bst (bst-widget bst))
                   (set-bst-left! bst
                                  (balance (balance-singular-node! (bst-left bst))))
                   (set-bst-right! bst
                                   (balance (balance-singular-node! (bst-right bst))))
                   bst))])))








(check-expect (db-bst 
               (insert-avl! D1 (make-db widget-name string<? string=? EMPTY)))
              (make-bst D1 false false))

(check-expect (db-bst 
               (insert-avl! A1 (make-db widget-name string<? string=? (make-bst D1 false false))))
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert-avl! W1 (make-db widget-name string<? string=? (make-bst D1 false false))))
              (make-bst D1 false (make-bst W1 false false)))

(check-expect (db-bst 
               (insert-avl! V1 (make-db widget-name
                                        string<?
                                        string=?
                                        (make-bst D1
                                                  (make-bst A1 false false)
                                                  (make-bst Z1
                                                            (make-bst W1 false false)
                                                            false)))))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1
                                  (make-bst V1 false false)
                                  (make-bst Z1 false false))))

(check-expect (db-bst 
               (insert-avl! Y1 (make-db widget-name
                                        string<?
                                        string=?
                                        (make-bst D1
                                                  (make-bst A1 false false)
                                                  (make-bst Z1
                                                            (make-bst W1 false false)
                                                            false)))))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst Y1
                                  (make-bst W1 false false)
                                  (make-bst Z1 false false))))


(check-expect (db-bst 
               (insert-avl! D1 (make-db widget-quantity < = EMPTY))) (make-bst D1 false false))

(check-expect (db-bst 
               (insert-avl! A1 (make-db widget-quantity < = (make-bst D1 false false)))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert-avl! Z1 (make-db widget-quantity < = (make-bst D1 false false))))
              (make-bst D1 false (make-bst Z1 false false)))

(check-expect (db-bst 
               (insert-avl! free-stuff (make-db widget-quantity
                                                <
                                                =
                                                (make-bst A1
                                                          (make-bst W1 false false)
                                                          (make-bst Z1
                                                                    (make-bst D1 false false)
                                                                    false)))))
              (make-bst A1
                        (make-bst W1
                                  (make-bst free-stuff false false)
                                  false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            false)
                                  false)))

(check-expect (db-bst 
               (insert-avl! V1 (make-db widget-quantity
                                        <
                                        =
                                        (make-bst A1
                                                  (make-bst W1 false false)
                                                  (make-bst Z1
                                                            (make-bst D1 false false)
                                                            false)))))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Z1
                                  (make-bst D1
                                            false
                                            false)
                                  (make-bst V1 false false))))


(check-expect (db-bst 
               (insert-avl! D1 (make-db widget-price < = EMPTY)))
              (make-bst D1 false false))

(check-expect (db-bst 
               (insert-avl! A1 (make-db widget-price < = (make-bst D1 false false)))) 
              (make-bst D1 (make-bst A1 false false) false))

(check-expect (db-bst 
               (insert-avl! Y1 (make-db widget-price < = (make-bst D1 false false))))
              (make-bst D1 false (make-bst Y1 false false)))

(check-expect (db-bst 
               (insert-avl! Z1 (make-db widget-price
                                        <
                                        =
                                        (make-bst A1
                                                  (make-bst W1 false false)
                                                  (make-bst V1
                                                            (make-bst Y1 false false)
                                                            false)))))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst Y1
                                  (make-bst Z1 false false)
                                  (make-bst V1 false false))))

(check-expect (db-bst 
               (insert-avl! X1 (make-db widget-price
                                        <
                                        =
                                        (make-bst A1
                                                  (make-bst W1 false false)
                                                  (make-bst V1
                                                            (make-bst Y1 false false)
                                                            false)))))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst X1
                                  (make-bst Y1 false false)                                         
                                  (make-bst V1 false false))))


(define (insert-avl! widget db) 
  (make-db (db-field db)
           (db-lt? db)
           (db-eq? db)
           (balance! (db-bst (insert! widget db)))))










































