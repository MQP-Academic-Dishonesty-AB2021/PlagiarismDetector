

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |starter pt3-1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))







(require 2htdp/image)

(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field less-than? eq? bst))




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
(define P1 (make-widget "P1" 37 10))
(define R1 (make-widget "R1" 4 9))
(define B1 (make-widget "B1" 7 7))
(define C1 (make-widget "C1" 10 3))

(define BST-quantity (make-bst D1
                               (make-bst A1
                                         (make-bst W1 false false)
                                         false)
                               (make-bst P1
                                         (make-bst B1 false false)
                                         (make-bst Z1 false false))))
(define BST-name (make-bst R1
                           (make-bst B1
                                     (make-bst A1 false false)
                                     (make-bst D1 false false))
                           (make-bst W1
                                     false
                                     (make-bst Z1 false false))))
(define BST-price (make-bst D1
                            (make-bst A1
                                      (make-bst W1 false false)
                                      false)
                            (make-bst R1
                                      false
                                      (make-bst P1
                                                false
                                                (make-bst Z1 false false)))))
   
(define DB-quantity (make-db widget-quantity < = BST-quantity))
(define DB-name (make-db widget-name string<? string=? BST-name))
(define DB-price (make-db widget-price < = BST-price))






(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (local [(define rand (random max))]
                  (make-widget 
                   (number->string rand)
                   rand
                   (random max))))))





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









(check-expect (db-bst (insert! R1 DB-quantity))
              (make-bst D1 
                        (make-bst A1 (make-bst W1 false false) (make-bst R1 false false)) 
                        (make-bst P1 (make-bst B1 false false) (make-bst Z1 false false))))


(check-expect (db-bst (insert! B1 DB-price))
              (make-bst D1 
                        (make-bst A1 (make-bst W1 false false) false) 
                        (make-bst R1 
                                  (make-bst B1 false false) 
                                  (make-bst P1 false (make-bst Z1 false false)))))

(check-expect (db-bst (insert! P1 DB-name))
              (make-bst R1 
                        (make-bst B1 (make-bst A1 false false) 
                                  (make-bst D1 false (make-bst P1 false false))) 
                        (make-bst W1 false (make-bst Z1 false false))))

(check-expect (db-bst (insert! B1 (make-db widget-price < = false))) 
              (make-bst B1 false false))

(define (insert! widget db)
  (local [(define (mutate-down side fn! bst)
            (if (false? (side bst))
                (fn! bst (make-bst widget false false))
                (insert-inner widget (side bst))))
          (define (insert-inner widget bst)
            (cond
              [(false? bst) (set-db-bst! db (make-bst widget false false))]
              [((db-less-than? db)
                ((db-field db) widget)
                ((db-field db) (bst-widget bst)))
               (mutate-down bst-left set-bst-left! bst)]
              [else 
               (mutate-down bst-right set-bst-right! bst)]))]                           
    (begin (insert-inner widget (db-bst db)) db)))



(define (insert widget db)
  (local [(define (insert-inner widget bst)
            (cond [(false? bst) 
                   (make-bst widget false false)]
                  [((db-less-than? db)
                    ((db-field db) widget)
                    ((db-field db) (bst-widget bst)))
                   (make-bst
                    (bst-widget bst)
                    (insert-inner widget (bst-left bst))
                    (bst-right bst))]
                  [else 
                   (make-bst
                    (bst-widget bst)
                    (bst-left bst)
                    (insert-inner widget (bst-right bst)))]))]
              
    (make-db (db-field db) (db-less-than? db) (db-eq? db) (insert-inner widget (db-bst db)))))
    

(define (find value db)
  (local
    [(define (find-inner value bst)
       (cond [(false? bst) false]
             [((db-eq? db) value ((db-field db) (bst-widget bst))) (bst-widget bst)]
             [((db-less-than? db) value ((db-field db) (bst-widget bst)))
              (find-inner value (bst-left bst))]
             [else
              (find-inner value (bst-right bst))]))]
    (find-inner value (db-bst db))))


(define t1 (make-bst (make-widget "t1" 10 50) false false))
(define t2 (make-bst (make-widget "t2" 30 20) false false))
(define t3 (make-bst (make-widget "t3" 5 10) false false))
(define t4 (make-bst (make-widget "t4" 17 2) false false))







(check-expect (rotate-left (make-bst A1 false (make-bst B1 false (make-bst C1 false false))))
              (make-bst B1 (make-bst A1 false false) (make-bst C1 false false)))

(check-expect (rotate-left (make-bst A1 t1 (make-bst B1 t2 t3)))
              (make-bst B1 (make-bst A1 t1 t2) t3))

(define (rotate-left bst)
  (make-bst 
   (bst-widget (bst-right bst))
   (make-bst 
    (bst-widget bst)
    (bst-left bst)
    (bst-left (bst-right bst)))
   (bst-right (bst-right bst))))





(check-expect (rotate-right (make-bst A1 (make-bst B1 (make-bst C1 false false) false) false))
              (make-bst B1 (make-bst C1 false false) (make-bst A1 false false)))

(check-expect (rotate-right (make-bst A1 (make-bst B1 t1 t2) t3))
              (make-bst B1 t1 (make-bst A1 t2 t3)))

(define (rotate-right bst)
  (make-bst 
   (bst-widget (bst-left bst))
   (bst-left (bst-left bst))
   (make-bst 
    (bst-widget bst)
    (bst-right (bst-left bst))
    (bst-right bst))))





(check-expect (rotate-right-left (make-bst A1 false (make-bst C1 (make-bst B1 false false) false)))
              (make-bst B1 (make-bst A1 false false) (make-bst C1 false false)))


(check-expect (rotate-right-left (make-bst A1 t1 (make-bst C1 (make-bst B1 t2 t3) t4)))
              (make-bst B1 (make-bst A1 t1 t2) (make-bst C1 t3 t4)))
  
(define (rotate-right-left bst)
  (rotate-left
   (make-bst
    (bst-widget bst)
    (bst-left bst)
    (rotate-right (bst-right bst)))))





(check-expect (rotate-left-right (make-bst A1 (make-bst C1 false (make-bst B1 false false)) false))
              (make-bst B1 (make-bst C1 false false) (make-bst A1 false false)))


(check-expect (rotate-left-right (make-bst A1 (make-bst C1 t1 (make-bst B1 t2 t3)) t4))
              (make-bst B1 (make-bst C1 t1 t2) (make-bst A1 t3 t4)))

(define (rotate-left-right bst)
  (rotate-right
   (make-bst
    (bst-widget bst)
    (rotate-left (bst-left bst))
    (bst-right bst))))





(check-expect (balance (make-bst A1 false (make-bst B1 false t3)))
              (make-bst B1 (make-bst A1 false false) t3))

(check-expect (balance (make-bst A1 (make-bst B1 t1 false) false))
              (make-bst B1 t1 (make-bst A1 false false)))

(check-expect (balance (make-bst A1 t1 (make-bst C1 (make-bst B1 t2 t3) t4)))
              (make-bst B1 (make-bst A1 t1 t2) (make-bst C1 t3 t4)))

(check-expect (balance (make-bst A1 (make-bst C1 t1 (make-bst B1 t2 t3)) t4))
              (make-bst B1 (make-bst C1 t1 t2) (make-bst A1 t3 t4)))

(check-expect (balance (make-bst B1 (make-bst C1 t1 t2) (make-bst A1 t3 t4)))
              (make-bst B1 (make-bst C1 t1 t2) (make-bst A1 t3 t4)))

(check-expect (balance false) false)

(define (balance bst)
  (local [(define c-diff (height-diff bst))]
    
    (cond [(<= (abs c-diff) 1) bst]
          
          [(= -2 c-diff)
           (if (= 1 (height-diff (bst-right bst))) 
               
               (rotate-right-left bst)
               
               (rotate-left bst))]
          
          [(= 2 c-diff)
           (if (= -1 (height-diff (bst-left bst)))
               
               (rotate-left-right bst)
               
               (rotate-right bst))]
          [else (error "Not sure how we got here. BST might not be an AVL already." bst)])))






(check-expect (db-bst (insert-avl (make-widget "c" 1 8) 
                                  (make-db widget-price < = (make-bst (make-widget "a" 1 1) false 
                                                                      (make-bst
                                                                       (make-widget "b" 1 5)
                                                                       false
                                                                       false)))))
              (db-bst (make-db widget-price < = 
                               (make-bst (make-widget "b" 1 5)
                                         (make-bst (make-widget "a" 1 1) false false)
                                         (make-bst (make-widget "c" 1 8) false false)))))



(check-expect (db-bst (insert-avl (make-widget "f" 1 17) 
                                  (make-db widget-price < = (make-bst
                                                             (make-widget "a" 1 1)
                                                                      (make-bst
                                                                       (make-widget "b" 1 5)
                                                                       false false)
                                                                      (make-bst
                                                                       (make-widget "d" 1 11)
                                                                       (make-bst
                                                                        (make-widget "c" 1 8)
                                                                        false false) 
                                                                       (make-bst
                                                                        (make-widget "e" 1 14)
                                                                        false false))))))
              (db-bst (make-db widget-price < = 
                               (make-bst (make-widget "d" 1 11)
                                         (make-bst (make-widget "a" 1 1) 
                                                   (make-bst (make-widget "b" 1 5)
                                                             false false)
                                                   (make-bst (make-widget "c" 1 8)
                                                             false false))
                                         (make-bst (make-widget "e" 1 14) false 
                                                   (make-bst (make-widget "f" 1 17)
                                                             false false))))))


(check-expect (db-bst (insert-avl (make-widget "a" 1 1)
                                  (make-db widget-price < = false)))
              (make-bst (make-widget "a" 1 1) false false))

(define (insert-avl widget db)
  (local [(define (insert-inner widget bst)
            (cond [(false? bst) 
                   (make-bst widget false false)]
                  [((db-less-than? db)
                    ((db-field db) widget)
                    ((db-field db) (bst-widget bst)))
                   (balance (make-bst
                             (bst-widget bst)
                             (insert-inner widget (bst-left bst))
                             (bst-right bst)))]
                  [else 
                   (balance (make-bst
                             (bst-widget bst)
                             (bst-left bst)
                             (insert-inner widget (bst-right bst))))]))]        
    (make-db (db-field db) (db-less-than? db) (db-eq? db) 
             (insert-inner widget (db-bst db)))))




(check-expect (time-insert 1000) "done")

(define (time-insert num)
  (local [(define db-empty
            (make-db widget-quantity < =
                     false))
          (define low (random-widgets num (* 2 num)))
          (define insert-time (begin
                                (display "insert test: ")
                                (time (foldl (lambda (widget prev-db)
                                               (insert widget prev-db))
                                             db-empty
                                             low)) "done"))
                
          (define insert!-time (begin
                                 (display "insert! test: ")
                                 (time (foldl (lambda (widget prev) 
                                                (insert! widget db-empty)) 0 low)) "done"))]
    "done"))



(check-expect (time-find 1000) "done")

(define (time-find num)
  (local [(define db-empty
            (make-db widget-quantity < =
                     false))
          (define low (random-widgets num (* 2 num)))
          
          (define test-bst
            (begin 
              (display "Making BST...\n") 
              (foldr (lambda (widget prev-db)
                       (insert widget prev-db))
                     db-empty
                     low)))
          (define bst-time (begin
                             (display "Timing BST find...\n")
                             (time (map (lambda 
                                            (widget) 
                                          (find (widget-quantity widget) test-bst))
                                        low))
                             "done"))
          (define test-avl
            (begin
              (display "Making AVL...\n") 
              (foldl (lambda (widget prev-db)
                       (insert-avl widget prev-db))
                     db-empty
                     low)))
          (define avl-time
            (begin
              (display "Timing AVL find...\n")
              (time
               (map (lambda (widget) 
                      (find (widget-quantity widget) test-avl)) low))
              "done"))]
    "done"))









(define (rotate-avl bst str)
  (local [(define left (if (false? (bst-left bst)) false
                           (make-bst (bst-widget (bst-left bst))
                                     (bst-left (bst-left bst))
                                     (bst-right (bst-left bst)))))
          (define right (if (false? (bst-right bst)) false
                            (make-bst (bst-widget (bst-right bst))
                                      (bst-left (bst-right bst))
                                      (bst-right (bst-right bst)))))
          (define widget (make-widget (widget-name (bst-widget bst))
                                      (widget-quantity (bst-widget bst))
                                      (widget-price (bst-widget bst))))

          (define (rotate-left! bst)
            (begin (set-bst-widget! bst (bst-widget right))
                   (set-bst-left! (bst-left bst) left)
                   (set-bst-widget! (bst-left bst) widget)
                   (set-bst-right! (bst-left bst) (bst-left right))
                   (set-bst-right! bst (bst-right right))
                   bst))

          (define (rotate-right! bst)
            (begin (set-bst-widget! bst (bst-widget left))
                   (set-bst-left! bst (bst-left left))
                   (set-bst-widget! (bst-right bst) widget)
                   (set-bst-left! (bst-right bst) (bst-left left))
                   (set-bst-right! (bst-right bst) right)
                   bst))


          (define (rotate-right-left! bst)
            (rotate-avl (rotate-avl bst "right") "left"))

          (define (rotate-left-right! bst)
            (rotate-avl (rotate-avl bst "left") "right"))
          ]
    
    (cond [(string=? str "left") (rotate-left! bst)]
          [(string=? str "right") (rotate-right! bst)]
          [(string=? str "left-right") (rotate-left-right! bst)]
          [(string=? str "right-left") (rotate-right-left! bst)]
          [else bst])))



(define (balance! bst)
  (local [(define c-diff (height-diff bst))]
    
    (cond [(<= (abs c-diff) 1) bst]
          
          [(= -2 c-diff)
           (if (= 1 (height-diff (bst-right bst))) 
               
               (rotate-avl bst "right-left")
               
               (rotate-avl bst "left"))]
          
          [(= 2 c-diff)
           (if (= -1 (height-diff (bst-left bst)))
               
               (rotate-avl bst "left-right")
               
               (rotate-avl bst "right"))]
          [else (error "Not sure how we got here. BST might not be an AVL already." bst)])))



(define (insert-avl! widget db)
  (local [(define (mutate-down field fn! bst)
            (if (false? (field bst))
                (fn! bst (make-bst widget false false))
                (begin
                  (insert-inner widget (field bst))
                  (balance! bst))))
          (define (insert-inner widget bst)
            (cond
              [(false? bst) (set-db-bst! db (make-bst widget false false))]
              [((db-less-than? db)
                ((db-field db) widget)
                ((db-field db) (bst-widget bst)))
               (mutate-down bst-left set-bst-left! bst)]
              [else 
               (mutate-down bst-right set-bst-right! bst)]))]                           
    (begin (insert-inner widget (db-bst db)) db)))



(check-expect (db-bst (insert-avl! (make-widget "f" 1 17) 
                                  (make-db widget-price < = (make-bst
                                                             (make-widget "a" 1 1)
                                                                      (make-bst
                                                                       (make-widget "b" 1 5)
                                                                       false false)
                                                                      (make-bst
                                                                       (make-widget "d" 1 11)
                                                                       (make-bst
                                                                        (make-widget "c" 1 8)
                                                                        false false) 
                                                                       (make-bst
                                                                        (make-widget "e" 1 14)
                                                                        false false))))))
              (db-bst (make-db widget-price < = 
                               (make-bst (make-widget "d" 1 11)
                                         (make-bst (make-widget "a" 1 1) 
                                                   (make-bst (make-widget "b" 1 5)
                                                             false false)
                                                   (make-bst (make-widget "c" 1 8)
                                                             false false))
                                         (make-bst (make-widget "e" 1 14) false 
                                                   (make-bst (make-widget "f" 1 17)
                                                             false false))))))


(check-expect (db-bst (insert-avl! (make-widget "a" 1 1)
                                  (make-db widget-price < = false)))
              (make-bst (make-widget "a" 1 1) false false))
