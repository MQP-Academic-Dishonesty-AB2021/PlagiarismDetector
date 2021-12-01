

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))








(define W1 (make-widget "W1" 1 1))
(define W2 (make-widget "Y1" 51 16))
(define W3 (make-widget "B1" 2 3))
(define W4 (make-widget "D1" 5 5))

(define W5 (make-widget "A1" 0 0))
(define W6 (make-widget "Z1" 100 100))

(define W7 (make-widget "X1" 3 3))



(define B0 (make-bst W4 false false))

(define B1
  (make-bst W1
            false
            (make-bst W2 false false)))
(define B2
  (make-bst W1
            (make-bst W3
                      false
                      (make-bst W4 false false))
            (make-bst W2
                      false
                      false)))

(define B3
  (make-bst W1
            (make-bst W3
                      (make-bst W5 false false)
                      (make-bst W4 false false))
            (make-bst W2 false false)))

(define B4
  (make-bst W1
            (make-bst W3
                      false
                      (make-bst W4 false false))
            (make-bst W2
                      false
                      (make-bst W6 false false))))

(define B5
  (make-bst W1
            (make-bst W3
                      false
                      (make-bst W4 false false))
            (make-bst W2
                      (make-bst W7 false false)
                      false)))


(define DB0 (make-db widget-name string<? string=? B0))
(define DB1 (make-db widget-name string<? string=? B1))
(define DB2 (make-db widget-name string<? string=? B2))
(define DB3 (make-db widget-quantity < = B1))
(define DBempty (make-db widget-price > = false))







(define (insert w db)
  (local
    [(define (insert-name b)
       (cond
         [(false? b) (make-bst w false false)]
         [((db-lt? db) ((db-field db) w) ((db-field db) (bst-widget b)))
          (make-bst
           (bst-widget b)
           (insert-name (bst-left b))
           (bst-right b))]
         [else
          (make-bst
           (bst-widget b)
           (bst-left b)
           (insert-name (bst-right b)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-name (db-bst db)))))






(define (find val db)
  (local [(define (find-name b)
            (cond
              [(false? b) false] 
              [((db-eq? db) val ((db-field db) (bst-widget b))) 
               (bst-widget b)]
              [((db-lt? db) val ((db-field db) (bst-widget b))) 
               (find-name (bst-left b))]
              [else                              
               (find-name (bst-right b))]))]
    (find-name (db-bst db))))






(define (insert! w db)
  (local
    [(define (insert-name b)
       (cond
         [(false? b) (set-db-bst! db (make-bst w false false))]       
         [((db-lt? db) ((db-field db) w) ((db-field db) (bst-widget b)))
          (if (false? (bst-left b)) 
              (set-bst-left! b (make-bst w false false))
              (insert-name (bst-left b)))] 
           
         [else
          (if (false? (bst-right b)) 
              (set-bst-right! b (make-bst w false false))
              (insert-name (bst-right b)))]))] 
    (begin (insert-name (db-bst db))
           db)))


(define test1-db
  (make-db widget-price > = false))
(check-expect 
 (db-bst (insert! W4 test1-db)) B0) 

(define test2-db 
  (make-db widget-name string<? string=?
           (make-bst W1
                     (make-bst W3
                               false
                               (make-bst W4 false false))
                     (make-bst W2 false false)))) 
(check-expect 
 (db-bst
  (insert!
   W5
   test2-db))
 B3) 

(define test3-db
  (make-db widget-name string<? string=?
           (make-bst W1
                     (make-bst W3
                               false
                               (make-bst W4 false false))
                     (make-bst W2 false false)))) 
(check-expect
 (db-bst
  (insert!
   W6
   test3-db))
 B4) 

(define test4-db (make-db widget-name string<? string=?
                          (make-bst W1
                                    (make-bst W3
                                              false
                                              (make-bst W4 false false))
                                    (make-bst W2 false false)))) 
(check-expect
 (db-bst
  (insert!
   W7
   test4-db))
 B5) 











(check-expect (db-bst (insert-avl W4 DBempty)) B0) 

(check-expect
 (db-bst
  (insert-avl
   W5
   (make-db widget-name string<? string=? B2)))
 B3) 

(check-expect
 (db-bst
  (insert-avl
   W6
   (make-db widget-name string<? string=? B2)))
 B4) 

(check-expect
 (db-bst
  (insert-avl
   W7
   (make-db widget-name string<? string=? B2)))
 B5) 


(define (insert-avl w db)
  (local
    [(define (insert-name b)
       (cond
         [(false? b) (make-bst w false false)]
         [((db-lt? db) ((db-field db) w) ((db-field db) (bst-widget b)))
          (make-bst
           (bst-widget b)
           (balance (insert-name (bst-left b)))
           (bst-right b))]
         [else
          (make-bst
           (bst-widget b)
           (bst-left b)
           (balance (insert-name (bst-right b))))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert-name (db-bst db)))))







(define unbalancedRR (make-bst
                      W5
                      false
                      (make-bst
                       W3
                       false
                       (make-bst
                        W4
                        false                     
                        (make-bst
                         W1
                         false
                         false)))))

(define balancedRR (make-bst
                    W3
                    (make-bst W5 false false)
                    (make-bst
                     W4
                     false                     
                     (make-bst
                      W1
                      false
                      false))))

(define unbalancedLL (make-bst
                      W1
                      (make-bst
                       W4
                       (make-bst
                        W3              
                        (make-bst
                         W5
                         false
                         false)
                        false)
                       false)
                      false))

(define balancedLL (make-bst
                    W4
                    (make-bst
                     W3
                     (make-bst W5 false false)
                     false)
                    (make-bst W1 false false)))


(define unbalancedLR (make-bst
                      W4
                      (make-bst
                       W5
                       false
                       (make-bst W3 false false))
                      false))

(define balancedLR (make-bst
                    W3
                    (make-bst W5 false false)
                    (make-bst W4 false false)))


(define unbalancedRL (make-bst
                      W5
                      false
                      (make-bst
                       W4
                       (make-bst W3 false false)
                       false)))



(define balancedRL 
  (make-bst
   W3
   (make-bst W5 false false)
   (make-bst W4 false false)))                


(define harderBST
  (make-bst
   (make-widget "20" 0 0)
   false
   (make-bst
    (make-widget "40" 0 0)
    false
    (make-bst
     (make-widget "60" 0 0)
     false
     (make-bst
      (make-widget "80" 0 0)
      (make-bst
       (make-widget "70" 0 0)
       (make-bst
        (make-widget "65" 0 0) false false)
       (make-bst
        (make-widget "75" 0 0) false false))
      (make-bst
       (make-widget "85" 0 0) false false))))))

(define harderBST-balanced
  (make-bst
   (make-widget "40" 0 0)
   (make-bst (make-widget "20" 0 0) false false)
   (make-bst
    (make-widget "60" 0 0)
    false
    (make-bst
     (make-widget "80" 0 0)
     (make-bst
      (make-widget "70" 0 0)
      (make-bst (make-widget "65" 0 0) false false)
      (make-bst (make-widget "75" 0 0) false false))
     (make-bst (make-widget "85" 0 0) false false)))))
                   

(check-expect (balance unbalancedRR) balancedRR)
(check-expect (balance unbalancedRL) balancedRL)
(check-expect (balance unbalancedLR) balancedLR)
(check-expect (balance unbalancedLL) balancedLL)

(check-expect (balance balancedRR) balancedRR) 
(check-expect (balance harderBST) harderBST-balanced)


(define (balance bst)
  (local [(define hd (height-diff bst))

          (define (rotate-right bst)
            (make-bst
             (bst-widget (bst-left bst)) 
             (bst-left (bst-left bst)) 
             (make-bst (bst-widget bst) false false))) 

          (define (rotate-left bst)
            (make-bst
             (bst-widget (bst-right bst)) 
             (make-bst (bst-widget bst) false false) 
             (bst-right (bst-right bst))))] 

    (cond [( < (abs hd) 2)
           bst] 
          [(> hd 1) 
           (local [(define hdl (height-diff (bst-left bst)))]

             (cond [(= hdl 0)
                    bst]
                   [(> hdl 0) 
                    (rotate-right bst)]
                   [(< hdl 0) 
                    (rotate-right
                     (make-bst (bst-widget bst)
                               (rotate-left (bst-left bst)) (bst-right bst)))
                    
                    ]))]
          
          [(< hd -1) 

           (local [(define hdr (height-diff (bst-right bst)))]

             (cond [(= hdr 0)
                    bst]
                   [(< hdr 0) 
                    (rotate-left bst)]
                   [(> hdr 0) 
                    (rotate-left
                     (make-bst (bst-widget bst) (bst-left bst)
                               (rotate-right (bst-right bst))))
                    
                    ]))])))




(define (time-insert num-widgets)
  (local [(define low (random-widgets-string num-widgets 10 10))
          (define db (make-db widget-name string<? string=? false))]

    (begin
      (time (foldl insert db low))
      (time (foldl insert! db low))
      "done")))



(define (time-find num-widgets)
  (local [(define low0 (random-widgets-string num-widgets 10 10))
          
          (define low (apply append (build-list 10 (lambda (n) low0))))
          
          (define db-bst (make-db widget-name string<? string=? false))
          (define db-avl (make-db widget-name string<? string=? false))
          
          (define (find-lst lst db0)
            (cond [(empty? lst) empty]
                  [else
                   (begin
                     (find (first lst) db0)
                     (find-lst (rest lst) db0))]))]
    
    
    

    (begin
      (foldl insert db-bst low0)
      (time (find-lst low db-bst))

      (foldl insert-avl db-avl low0)
      (time (find-lst low db-avl))
      "done")))










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