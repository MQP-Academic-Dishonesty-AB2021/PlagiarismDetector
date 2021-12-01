

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3-1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))





(define-struct bst (widget left right))




(define-struct widget (name quantity price))




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 2 3))



(define BST0 false)


(define BST1 (make-bst
              W1 false false))


(define BST2 (make-bst
              D1
              (make-bst
               A1 false false)
              (make-bst
               Z1 false false)))


(define BST3 (make-bst
              D1
              (make-bst
               A1 false false)
              (make-bst
               Z1
               (make-bst
                W1 false false)
               false)))

(define BST4 (make-bst
              D1
              (make-bst
               A1 false
               (make-bst B1 false false))
              (make-bst
               Z1
               (make-bst
                W1 false false)
               false)))


(define BST5 
  (make-bst
   A1
   (make-bst W1 false false)
   (make-bst Z1 false false)))


(define BST6 (make-bst
              A1
              (make-bst W1 false false)
              (make-bst Z1
                        (make-bst B1 false false)
                        false)))


(define-struct db (field lt? eq? bst))

(define DB-quantity (make-db widget-quantity < = false))

(define DB-name (make-db widget-name string<? string=? false))

(define DB-price (make-db widget-price < = BST5))

(define DB-name-BST3 (make-db widget-name string<? string=? BST3))





(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))





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










(define (insert! value db)
  (local [
          
          (define (insert-inner value bst parent side)
            (cond [(false? bst)
                   (cond
                     
                     
                     [(string=? "first" side)
                      (set-db-bst! db (make-bst value false false))] 
                     [(string=? "left" side)
                      (set-bst-left!
                       parent (make-bst value false false))]
                     [else (set-bst-right! parent (make-bst value false false))])]
                  [else (if ((db-lt? db) ((db-field db) value)
                                         ((db-field db) (bst-widget bst)))
                            (insert-inner value (bst-left bst) bst "left" )
                            (insert-inner value (bst-right bst) bst "right"))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (begin (insert-inner value (db-bst db) false "first")
                    (db-bst db)))))



(check-expect (db-bst(insert! W1 DB-quantity)) BST1)


(check-expect (db-bst (insert! B1 DB-name-BST3)) BST4)


(check-expect (db-bst (insert! B1 DB-price)) BST6)
 










(define list-bst1 (make-bst W1 (make-bst B1 (make-bst A1 false false) false) false))
(define list-bst2 (make-bst W1 (make-bst B1 (make-bst A1 false false)
                                         (make-bst D1 false false)) false))
(define list-bst3 (make-bst B1 (make-bst A1 false false) false))


(check-expect (right-rotation list-bst1)
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(check-expect (right-rotation list-bst2)
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1
                                  (make-bst D1 false false)
                                  false)))



(check-expect (right-rotation list-bst3)
              (make-bst A1 false (make-bst B1 false false)))

(define (right-rotation bst-in)
  (local
    
    [(define new-root (bst-left bst-in))
     (define old-root bst-in)]

    (make-bst (bst-widget new-root)
              (bst-left new-root)
              (make-bst
               (bst-widget old-root)
               (bst-right new-root)
               (bst-right old-root)))))








(define list-bst4 (make-bst A1 false (make-bst B1 false (make-bst W1 false false))))
(define list-bst5 (make-bst A1 false (make-bst D1 (make-bst B1 false false)
                                               (make-bst W1 false false))))
(define list-bst6 (make-bst A1 false (make-bst B1 false false)))


(check-expect (left-rotation list-bst4)
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(check-expect (left-rotation list-bst5)
              (make-bst D1
                        (make-bst A1 false
                                  (make-bst B1 false false))
                        (make-bst W1
                                  false
                                  false)))



(check-expect (left-rotation list-bst6)
              (make-bst B1 (make-bst A1 false false) false))

(define (left-rotation bst-in)
  (local
    
    [(define new-root (bst-right bst-in))
     (define old-root bst-in)]

    (make-bst (bst-widget new-root)
              (make-bst
               (bst-widget old-root)
               (bst-left old-root)
               (bst-left new-root))
              (bst-right new-root))))





(check-expect (avl? list-bst1) false)
(check-expect (avl? list-bst2) false)
(check-expect (avl? list-bst4) false)



(check-expect (avl? (make-bst D1
                              (make-bst A1 false false)
                              (make-bst W1 false false))) true)

(check-expect (avl? (make-bst D1
                              (make-bst A1 false (
                                                  make-bst B1 false false))
                              (make-bst W1 false false))) true)


(check-expect (avl? (make-bst W1 false false)) true)
(check-expect (avl? false) true)


(define (avl? bst-in)
  (<= (abs (height-diff bst-in)) 1))




(check-expect (imbalanced list-bst1)
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(check-expect (imbalanced list-bst2)
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1
                                  (make-bst D1 false false)
                                  false)))


(check-expect (imbalanced list-bst4)
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(check-expect (imbalanced list-bst5)
              (make-bst D1
                        (make-bst A1 false
                                  (make-bst B1 false false))
                        (make-bst W1
                                  false
                                  false)))


(define list-bst7 (make-bst A1
                            false
                            (make-bst W1
                                      (make-bst D1 false false) false)))

(check-expect (imbalanced list-bst7)
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(define list-bst8 (make-bst W1
                            (make-bst A1
                                      false (make-bst D1 false false)) false))

(check-expect (imbalanced list-bst8)
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))




(define (imbalanced bst-in)
  (if (< (height-diff bst-in) 0)
      (if (<= (height-diff (bst-right bst-in)) 0) 
          (left-rotation bst-in)
          (left-rotation
           (make-bst (bst-widget bst-in)
                     false
                     (right-rotation (bst-right bst-in)))))
      (if (>= (height-diff (bst-left bst-in)) 0)
          (right-rotation bst-in)
          (right-rotation
           (make-bst (bst-widget bst-in)
                     (left-rotation (bst-left bst-in))
                     false)))))






(check-expect (balance list-bst1)
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(check-expect (balance list-bst2)
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1
                                  (make-bst D1 false false)
                                  false)))


(check-expect (balance list-bst4)
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(check-expect (balance list-bst5)
              (make-bst D1
                        (make-bst A1 false
                                  (make-bst B1 false false))
                        (make-bst W1
                                  false
                                  false)))


(check-expect (balance list-bst7)
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(check-expect (balance list-bst8)
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))



(check-expect (balance false) false)


(check-expect (balance BST1) BST1)


(check-expect (balance
               (make-bst D1
                         (make-bst A1 false false)
                         (make-bst W1 false false)))
              (make-bst D1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(check-expect (balance (make-bst D1
                                 (make-bst A1 false (make-bst B1 false false))
                                 (make-bst W1 false false)))
              (make-bst D1
                        (make-bst A1 false (make-bst B1 false false))
                        (make-bst W1 false false)))

(define long-tree (make-bst D1
                            (make-bst B1
                                      (make-bst A1 false false) false)
                            (make-bst W1 false false)))



(define (balance bst-in)
  (if (avl? bst-in)
      bst-in
      (imbalanced bst-in)))





(define DB-quantity1 (make-db widget-quantity < = false))
(check-expect (db-bst(insert-avl W1 DB-quantity1)) BST1)


(define DB-name-BST3-1 (make-db widget-name
                                string<?
                                string=? (make-bst
                                          D1
                                          (make-bst
                                           A1 false false)
                                          (make-bst
                                           Z1
                                           (make-bst
                                            W1 false false)
                                           false))))

(check-expect (db-bst (insert-avl B1 DB-name-BST3-1)) BST4)




(define DB-price-1 (make-db widget-price < = 
                            (make-bst
                             A1
                             (make-bst W1 false false)
                             (make-bst Z1 (make-bst
                                           (make-widget "L2" 12 10) false false)
                                       false))))
(check-expect (db-bst (insert-avl B1 DB-price-1))
              (make-bst A1
                        (make-bst W1 false false)
                        (make-bst (make-widget "L2" 12 10)
                                  (make-bst B1 false false)
                                  (make-bst Z1 false false))))


(define DB-name-2 (make-db widget-name
                           string<?
                           string=?
                           (make-bst A1 false (make-bst W1 false false))))
  

(check-expect (db-bst (insert-avl B1 DB-name-2))
              (make-bst B1
                        (make-bst A1 false false)
                        (make-bst W1 false false)))


(define (insert-avl value db)
  (local [
          
          (define (insert-inner value bst)
            (cond [(false? bst) (make-bst value false false)]
                  [else (if ((db-lt? db) ((db-field db) value)
                                         ((db-field db) (bst-widget bst)))
                            (balance (make-bst (bst-widget bst)
                                               (insert-inner value (bst-left bst))
                                               (bst-right bst)))
                            (balance (make-bst (bst-widget bst)
                                               (bst-left bst)
                                               (balance (insert-inner value (bst-right bst))))))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-inner value (db-bst db)))))



(define (find key db)
  (local [
          
          (define (find-inner key bst)
            
            (cond [(false? bst) false]
                  [((db-eq? db) key
                                ((db-field db) (bst-widget bst)))
                   (bst-widget bst)]
                  [else
                   (if ((db-lt? db) key ((db-field db) (bst-widget bst)))
                       (find-inner key (bst-left bst))
                       (find-inner key (bst-right bst)))]))]
    (find-inner key (db-bst db))))



(define (insert value db)
  (local [
          
          (define (insert-inner value bst)
            (cond [(false? bst) (make-bst value false false)]
                  [else (if ((db-lt? db) ((db-field db) value)
                                         ((db-field db) (bst-widget bst)))
                            (make-bst (bst-widget bst)
                                      (insert-inner value (bst-left bst))
                                      (bst-right bst))
                            (make-bst (bst-widget bst)
                                      (bst-left bst)
                                      (insert-inner value (bst-right bst))))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-inner value (db-bst db))))) 


(define 250k (random-widgets 250000 9999))
(define db-test (make-db widget-price < = false))



(define (time-insert)
  (begin 
          
    (time (foldl 
           insert! db-test
           250k))
    (set-db-bst! db-test false)
    (time (foldl 
           insert db-test
           250k))
    (set-db-bst! db-test false)
    "done"))





(define 10k (random-widgets 10000 9999))
(define db-test-2 (make-db widget-price < = false))

(define 10k-bst (foldl 
                 insert db-test-2
                 10k))

(define 10k-avl (foldl 
                 insert-avl db-test-2
                 10k))



(define (time-find)
  (begin (time (map (λ (key) (find (widget-price key) 10k-bst)) 10k))
         (time (map (λ (key) (find (widget-price key) 10k-avl)) 10k))
         "done"))

