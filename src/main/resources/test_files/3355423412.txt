

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define-struct widget (name quantity price))



(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 10 32))
(define F1 (make-widget "F1" 7 2))

(define-struct bst (widget left right))




(define BST-NAME (make-bst W1 (make-bst A1 false
                                        (make-bst D1 false false))
                           (make-bst Z1 false false)))
(define BST-QUAN (make-bst D1
                           (make-bst A1 false false)
                           (make-bst Z1 false false)))
(define BST-PRICE (make-bst D1
                            (make-bst Z1 false false)
                            (make-bst A1 false (make-bst W1 false false))))

(define-struct db (field lessthan? equal? bst))

(define DB-NAME (make-db widget-name string<? string=? BST-NAME))
(define DB-QUAN (make-db widget-quantity < = BST-QUAN))
(define DB-PRICE (make-db widget-price > = BST-PRICE))

(define DB-EMPTY (make-db widget-name string<? string=? false))









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






(check-expect (db-bst (begin
                        (insert! W1 DB-EMPTY)
                        DB-EMPTY))
              (make-bst W1 false false))

(check-expect (db-bst (begin (insert! W1 DB-QUAN)
                             DB-QUAN))
              (make-bst D1
                        (make-bst A1 (make-bst W1 false false) false)
                        (make-bst Z1 false false)))

(check-expect (db-bst (begin (insert! B1 DB-PRICE)
                             DB-PRICE)) 
              (make-bst D1
                        (make-bst Z1 (make-bst B1 false false) false)
                        (make-bst A1 false (make-bst W1 false false))))


(define (insert! widget db) 
  (local[
         (define (insert bst)
           (local
             [(define (get-field db)
                ((db-field db) (bst-widget bst)))
              (define (is-widget-left?)
                ((db-lessthan? db) ((db-field db) widget) (get-field db)))]
             
             (cond     
               [(and (is-widget-left?) (false? (bst-left bst))) 
                (set-bst-left! bst (make-bst widget false false))]
                   
               [(and (not (is-widget-left?)) (false? (bst-right bst))) 
                (set-bst-right! bst (make-bst widget false false))]
                   
               [else 
                (if (is-widget-left?)
                    (insert (bst-left bst))
                    (insert (bst-right bst)))])))]
    (if (false? (db-bst db))
        (begin (set-db-bst! db (make-bst widget false false))
               db)
        (begin (insert (db-bst db))
               db))))


(define avl-test1 (make-bst W1 (make-bst A1 false
                                         (make-bst D1 false false))
                            (make-bst Z1 false false)))
(define avl-test2 (make-bst D1
                            (make-bst A1 false false)
                            (make-bst Z1
                                      (make-bst B1 false false)
                                      false)))
(define avl-test3 (make-bst D1
                            (make-bst Z1 false false)
                            (make-bst A1 false (make-bst W1 false false))))





(check-expect (db-bst (insert-avl F1
                                  (make-db widget-name string<? string=? avl-test1)))
              (make-bst W1
                        (make-bst D1
                                  (make-bst A1 false false)
                                  (make-bst F1 false false))
                        (make-bst Z1 false false))) 
(check-expect (db-bst (insert-avl F1 (make-db widget-quantity < = avl-test2)))
              (make-bst D1
                        (make-bst A1
                                  false
                                  false)
                        (make-bst B1
                                  (make-bst F1 false false)
                                  (make-bst Z1 false false)))) 

(check-expect (db-bst (insert-avl F1 (make-db widget-price > = avl-test3)))
              (make-bst D1
                        (make-bst Z1 false false)
                        (make-bst F1
                                  (make-bst A1 false false)
                                  (make-bst W1 false false)))) 

(check-expect (db-bst (insert-avl F1 (make-db widget-price < = false)))
              (make-bst F1 false false)) 


(define (insert-avl widget db)
  (local[
         (define (insert bst)
           (local [(define (get-field db)
                     ((db-field db) (bst-widget bst)))]
             (cond [(false? bst) (make-bst widget false false)]
                   [else
                    (if ((db-lessthan? db) ((db-field db) widget) (get-field db))
                        (balance (make-bst (bst-widget bst)  
                                           (insert (bst-left bst)) (bst-right bst)))
                        (balance (make-bst (bst-widget bst)
                                           (bst-left bst) (insert (bst-right bst)))))])))]
    (make-db (db-field db) (db-lessthan? db) (db-equal? db) (insert (db-bst db)))))








(define test1 (make-bst Z1
                        (make-bst W1 (make-bst B1 false false) false) false))
(define test2 (make-bst B1
                        false (make-bst D1 false (make-bst Z1 false false))))
(define test3 (make-bst D1
                        (make-bst A1 false (make-bst B1 false false)) false))
(define test4 (make-bst D1
                        false (make-bst Z1 (make-bst W1 false false) false)))
(define test5 (make-bst W1 (make-bst D1 false false)
                        (make-bst Z1 false false)))

(check-expect (balance test1)
              (make-bst W1 (make-bst B1 false false)
                        (make-bst Z1 false false))) 
(check-expect (balance test2)
              (make-bst D1 (make-bst B1 false false)
                        (make-bst Z1 false false))) 
(check-expect (balance test3)
              (make-bst B1 (make-bst A1 false false)
                        (make-bst D1 false false))) 
(check-expect (balance test4)
              (make-bst W1 (make-bst D1 false false)
                        (make-bst Z1 false false))) 
(check-expect (balance test5) test5) 

(define (balance bst)
  (local [(define (left-left bst) (bst-left (bst-left bst)))
          (define (left-right bst) (bst-right (bst-left bst)))
          (define (right-left bst) (bst-left (bst-right bst)))
          (define (right-right bst) (bst-right (bst-right bst)))]
            
    (cond [(<= (abs (height-diff bst)) 1) bst]
          
          [(false? bst) false]
          
          [(and (positive? (height-diff bst)) 
                (positive? (height-diff (bst-left bst)))) 
           (balance (make-bst (bst-widget (bst-left bst))
                              (balance (left-left bst))
                              (balance (make-bst (bst-widget bst)
                                                 (left-right bst)
                                                 (bst-right bst)))))]

          [(and (negative? (height-diff bst)) 
                (negative? (height-diff (bst-right bst))))
           (balance (make-bst (bst-widget (bst-right bst))
                              (balance (make-bst (bst-widget bst)
                                                 (bst-left bst)
                                                 (right-left bst)))
                              (balance (right-right bst))))]

          [(and (negative? (height-diff bst)) 
                (positive? (height-diff (bst-right bst))))
           (balance (make-bst (bst-widget bst)
                              (balance (bst-left bst))
                              (balance
                               (make-bst (bst-widget (right-left bst))
                                         (bst-left (right-left bst))
                                         (make-bst
                                          (bst-widget (bst-right bst))
                                          (bst-right (right-left bst))
                                          (right-right bst))))))]

          [(and (positive? (height-diff bst)) 
                (negative? (height-diff (bst-left bst))))
           (balance (make-bst (bst-widget bst)
                              (balance
                               (make-bst (bst-widget (left-right bst))
                                         (make-bst
                                          (bst-widget (bst-left bst))
                                          (left-left bst)
                                          (bst-left (left-right bst)))
                                         (bst-right (left-right bst))))
                              (balance (bst-right bst))))]
          [else bst]                             
          )))

                   




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




(define (insert widget db)
  (local[
         (define (insert bst)
           (local [(define (get-field db)
                     ((db-field db) (bst-widget bst)))]
             (cond [(false? bst) (make-bst widget false false)]
                   [else
                    (if ((db-lessthan? db) ((db-field db) widget) (get-field db))
                        (make-bst (bst-widget bst)  
                                  (insert (bst-left bst)) (bst-right bst))
                        (make-bst (bst-widget bst)
                                  (bst-left bst) (insert (bst-right bst))))])))]
    (make-db (db-field db) (db-lessthan? db) (db-equal? db) (insert (db-bst db)))))

(define (find key db)
  (local[
         (define (find bst)
           (local [(define (get-field db)
                     ((db-field db) (bst-widget bst)))]
             (cond [(false? bst) false]
                   [else
                    (cond [((db-equal? db) key (get-field db)) (bst-widget bst)]
                          [((db-lessthan? db) key (get-field db))
                           (find (bst-left bst))]
                          [else (find (bst-right bst))])])))]
    (find (db-bst db))))

(define (insert-list low db fn)
  (cond [(empty? low) db]
        [else
         (insert-list (rest low) 
                      (fn (first low) db)
                      fn)]))
 
(define (time-insert) 
  (local [(define rand-wid (random-widgets 250000 100))
          (define empty-db (make-db widget-name string<? string=? false))]
    (begin (time (insert-list rand-wid empty-db insert))
           (time (insert-list rand-wid empty-db insert!)) 
           "done")))



(define (find-all db low)
  (cond [(false? (db-bst db)) false]
        [else
         (find ((db-field db) (first low))
               (insert-list (rest low) db insert))]))

(define (time-find)
  (local [(define rand-wid (random-widgets 10000 100))
          (define testing-bst (insert-list rand-wid DB-EMPTY insert))
          (define testing-avl (insert-list rand-wid DB-EMPTY insert-avl))]
    (begin (time (find-all testing-bst rand-wid)) 
           (time (find-all testing-avl rand-wid))
           "done"))) 
