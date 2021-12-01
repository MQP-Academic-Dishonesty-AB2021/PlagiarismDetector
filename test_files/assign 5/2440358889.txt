

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

 
(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))






(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define L1 (make-widget "L1" 7 10))
(define E1 (make-widget "E1" 13 2))
(define C1 (make-widget "C1" 2 5))











(define BST1 (make-bst D1
                       (make-bst A1
                                 #f
                                 (make-bst W1 #f #f))
                       (make-bst Z1 #f #f)))










(define BST2 (make-bst A1
                       #f
                       (make-bst Z1
                                 (make-bst D1
                                           #f
                                           (make-bst W1 #f #f))
                                 #f)))
                                 



(define BST3 (make-bst D1 #f #f))







(define BST4 (make-bst D1
                       (make-bst A1 #f #f)
                       (make-bst W1 #f #f)))







(define BST5 (make-bst L1
                       (make-bst A1 #f #f)
                       (make-bst Z1 #f #f)))
                      




(define DB1 (make-db widget-name string<? string=? BST1))
(define DB2 (make-db widget-name string<? string=? BST3))
(define DB3 (make-db widget-name string<? string=? BST4))

(define DB4 (make-db widget-quantity > = BST2))
(define DB5 (make-db widget-quantity > = BST4))
(define DB6 (make-db widget-quantity > = BST5))

(define DB7 (make-db widget-price < = BST1))
(define DB8 (make-db widget-price < = BST3))
(define DB9 (make-db widget-price < = BST5))

(define DB10 (make-db widget-name string<? string=? false))






















































(define (insert! w db)
  (local
    [(define bst (db-bst db))
     (define lt? (db-lt? db))
     (define eq? (db-eq? db))
     (define wid-field (db-field db))]
    (if (false? bst)  
        (make-db lt? eq? wid-field (make-bst w #f #f))
        (local
          [(define (set-bst b side)
             (local
               [(define branch
                  (cond [(string=? "" side) b]
                        [(string=? "l" side) (bst-left b)]
                        [(string=? "r" side) (bst-right b)]))]
               (if (false? branch)
                   (if (string=? "l" side)
                       (set-bst-left! b (make-bst w #f #f))
                       (set-bst-right! b (make-bst w #f #f)))
                   (set-bst branch (if (lt? (wid-field w) (wid-field (bst-widget branch)))
                                       "l"
                                       "r")))))]
          (begin
            (set-bst bst "")
            (make-db lt? eq? wid-field bst))))))
       
   









































                                 












































(define BST10 (make-bst D1
                        (make-bst A1 #f #f)
                        (make-bst L1
                                  #f
                                  (make-bst Z1
                                            (make-bst W1 #f #f)
                                            #f))))

(define BST9 (make-bst D1
                       (make-bst A1 #f #f)
                       (make-bst L1
                                 #f
                                 (make-bst W1
                                           #f
                                           (make-bst Z1 #f #f)))))

(define BST8 (make-bst W1
                       (make-bst L1
                                 (make-bst D1
                                           (make-bst A1 #f #f)
                                           #f)
                                 #f)
                       (make-bst Z1 #f #f)))

(define BST6 (make-bst W1
                       (make-bst L1
                                 (make-bst A1
                                           #f
                                           (make-bst D1 #f #f))
                                 #f)
                       (make-bst Z1 #f #f)))

(define BST7 (make-bst W1
                       (make-bst A1
                                 #f
                                 (make-bst L1
                                           (make-bst D1
                                                     #f
                                                     (make-bst E1 #f #f))
                                           #f))
                       (make-bst Z1 #f #f)))

(define DB11 (make-db widget-name string<? string=? BST8))
(define DB12 (make-db widget-name string<? string=? BST9))
(define DB13 (make-db widget-name string<? string=? BST10))







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










(check-expect (db-bst (insert-avl L1 DB3))
              (make-bst D1
                        (make-bst A1 #f #f)
                        (make-bst W1 (make-bst L1 #f #f) #f)))

(check-expect (db-bst (insert-avl C1 DB11))
              (make-bst D1
                        (make-bst A1
                                  #f
                                  (make-bst C1 #f #f))
                        (make-bst W1
                                  (make-bst L1 #f #f)
                                  (make-bst Z1 #f #f))))

(check-expect (db-bst (insert-avl C1 DB12))
              (make-bst D1
                        (make-bst A1
                                  #f
                                  (make-bst C1 #f #f))
                        (make-bst W1
                                  (make-bst L1 #f #f)
                                  (make-bst Z1 #f #f))))

(check-expect (db-bst (insert-avl C1 DB13))
              (make-bst D1
                        (make-bst A1
                                  #f
                                  (make-bst C1 #f #f))
                        (make-bst W1
                                  (make-bst L1 #f #f)
                                  (make-bst Z1 #f #f))))

(define (insert-avl w db)
  (local
    [(define (insert0 bst)
       (cond [(false? bst) (make-bst w #f #f)]
             [((db-lt? db) ((db-field db) w) ((db-field db) (bst-widget bst)))
              (make-bst (bst-widget bst) (insert0 (bst-left bst)) (bst-right bst))]
             [else
              (make-bst (bst-widget bst) (bst-left bst) (insert0 (bst-right bst)))]))]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (balance (insert0 (balance (db-bst db)))))))










(check-expect (balance BST8)                          
              (make-bst W1
                        (make-bst D1
                                  (make-bst A1 #f #f)
                                  (make-bst L1 #f #f))
                        (make-bst Z1 #f #f)))

(check-expect (balance BST6)                          
              (make-bst W1
                        (make-bst D1
                                  (make-bst A1 #f #f)
                                  (make-bst L1 #f #f))
                        (make-bst Z1 #f #f)))

(check-expect (balance BST9)                          
              (make-bst D1
                        (make-bst A1 #f #f)
                        (make-bst W1
                                  (make-bst L1 #f #f)
                                  (make-bst Z1 #f #f))))

(check-expect (balance BST10)                         
              (make-bst D1
                        (make-bst A1 #f #f)
                        (make-bst W1
                                  (make-bst L1 #f #f)
                                  (make-bst Z1 #f #f))))

(check-expect (balance BST1) BST1)     
(check-expect (balance BST3) BST3)
(check-expect (balance BST4) BST4)
                        

(define (balance bst)
  (if (valid-avl? bst)
      bst
      (local
        [(define v (get-violator bst))]
        (if (positive? (height-diff v))
            (if (positive? (height-diff (bst-left v)))
                (rotate "ll" bst v)
                (rotate "lr" bst v))
            (if (positive? (height-diff (bst-right v)))
                (rotate "rl" bst v)
                (rotate "rr" bst v))))))








(check-expect (valid-avl? BST1) true)
(check-expect (valid-avl? BST2) false)
(check-expect (valid-avl? BST3) true)
(check-expect (valid-avl? BST4) true)
(check-expect (valid-avl? BST5) true)
(check-expect (valid-avl? (bst-right BST2)) false)
(check-expect (valid-avl? (bst-left BST2)) true)

(define (valid-avl? bst)
  (<= (abs (height-diff bst)) 1))













(check-expect (get-violator BST6) (bst-left BST6))
(check-expect (get-violator BST2) (bst-right BST2))
(check-expect (get-violator BST7) (bst-right (bst-left BST7)))

(define (get-violator bst)
  (local
    [(define (violator0 bst prev)
       (cond [(valid-avl? bst) prev]
             [(positive? (height-diff bst)) (violator0 (bst-left bst) bst)]
             [else
              (violator0 (bst-right bst) bst)]))]
    (violator0 bst false)))














(define (rotate type bst v)
  (local
    [(define name (widget-name (bst-widget v)))]
    (local
      [(define (rotate0 bst)    
         (cond
           [(string=? (widget-name (bst-widget bst)) name) (make-rotation v)]
           [(positive? (height-diff bst))
            (make-bst (bst-widget bst) (rotate0 (bst-left bst)) (bst-right bst))]
           [else
            (make-bst (bst-widget bst) (bst-left bst) (rotate0 (bst-right bst)))]))

       (define (make-rotation bst) 
         (cond
           
           
           [(string=? type "ll")
            (local
              [(define v bst)
               (define a (bst-left bst))
               (define b (bst-left a))]
              (make-bst
               (bst-widget a)
               (make-bst (bst-widget b) (bst-left b) (bst-right b))
               (make-bst (bst-widget v) (bst-right a) (bst-right v))))]
           
           
           [(string=? type "lr")
            (local
              [(define v (bst-widget bst))
               (define a (bst-widget (bst-left bst)))
               (define b (bst-widget (bst-right (bst-left bst))))
               (define bst0
                 (make-bst v
                           (make-bst b
                                     (make-bst a #f #f)
                                     #f)
                           #f))]
              (rotate "ll" bst0 (get-violator bst0)))]
           
           
           [(string=? type "rr")
            (local
              [(define v bst)
               (define a (bst-right bst))
               (define b (bst-right a))]
              (make-bst
               (bst-widget a)
               (make-bst (bst-widget v) (bst-left v) (bst-left a))
               (make-bst (bst-widget b) (bst-left b) (bst-right b))))]
           
           
           [(string=? type "rl")
            (local
              [(define v (bst-widget bst))
               (define a (bst-widget (bst-right bst)))
               (define b (bst-widget (bst-left (bst-right bst))))
               (define bst0
                 (make-bst v
                           #f
                           (make-bst b
                                     #f
                                     (make-bst a #f #f))))]
              (rotate "rr" bst0 (get-violator bst0)))]))]
    
      (rotate0 bst))))










(check-expect (db-bst (insert! L1 DB3))
              (make-bst D1
                        (make-bst A1 #f #f)
                        (make-bst W1 (make-bst L1 #f #f) #f)))


(check-expect (db-bst (insert! C1 DB13))
              (make-bst D1
                        (make-bst A1
                                  #f
                                  (make-bst C1 #f #f))
                        (make-bst L1
                                  #f
                                  (make-bst Z1
                                            (make-bst W1 #f #f)
                                            #f))))


(check-expect (db-bst (insert! L1 DB7))
              (make-bst D1
                        (make-bst A1 #f
                                  (make-bst W1 #f #f))
                        (make-bst Z1
                                  (make-bst L1 #f #f)
                                  #f)))


(check-expect (db-bst (insert! Z1 DB5))
              (make-bst D1
                        (make-bst A1
                                  (make-bst Z1 #f #f)
                                  #f)
                        (make-bst W1
                                  (make-bst L1 #f #f)
                                  #f)))

(check-expect (db-bst (insert! A1 DB10)) (make-bst A1 #f #f))



(require 2htdp/image)






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



(define (insert w db)
  (local
    [(define (insert0 bst)
       (cond [(false? bst) (make-bst w #f #f)]
             [((db-lt? db) ((db-field db) w) ((db-field db) (bst-widget bst)))
              (make-bst (bst-widget bst) (insert0 (bst-left bst)) (bst-right bst))]
             [else
              (make-bst (bst-widget bst) (bst-left bst) (insert0 (bst-right bst)))]))]
    (make-db
     (db-field db)
     (db-lt? db)
     (db-eq? db)
     (insert0 (db-bst db)))))



(define (smaller? k b)
  (if (false? b)
      #f
      (string<? k (widget-name (bst-widget b)))))



(define (insert-name w b)
  (cond [(false? b) (make-bst w #f #f)]
        [(smaller? (widget-name w) b)
         (make-bst (bst-widget b) (insert-name w (bst-left b)) (bst-right b))]
        [else
         (make-bst (bst-widget b) (bst-left b) (insert-name w (bst-right b)))]))



(define (find x db)
  (cond
    [(false? (db-bst db)) false]
    [((db-eq? db)
      x
      ((db-field db) (bst-widget (db-bst db))))
     (bst-widget (db-bst db))]
    [((db-lt? db)
      x
      ((db-field db) (bst-widget (db-bst db))))
     (find x (make-db
              (db-field db) (db-lt? db) (db-eq? db) (bst-left (db-bst db))))]
    [else
     (find x (make-db
              (db-field db) (db-lt? db) (db-eq? db) (bst-right (db-bst db))))]))



(define (build-tree low)
  (foldr insert-name false low))














































































