

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))



(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define G1 (make-widget "G1" 7 39))
(define R1 (make-widget "R1" 0 8))

(define-struct bst (widget left right))







(define BST1 (make-bst D1
                       (make-bst A1
                                 false
                                 false)
                       (make-bst W1
                                 false
                                 (make-bst Z1
                                           false
                                           false))))

(define BST2 (make-bst D1
                       (make-bst A1
                                 false
                                 false)
                       (make-bst W1
                                 false
                                 false)))
(define BST1--mutate (make-bst D1
                               (make-bst A1
                                         false
                                         false)
                               (make-bst W1
                                         false
                                         (make-bst Z1
                                                   false
                                                   false))))

(define BST2--mutate (make-bst D1
                               (make-bst A1
                                         false
                                         false)
                               (make-bst W1
                                         false
                                         false)))

(define BST5 (make-bst 2
                       (make-bst 1 false false)
                       (make-bst 4
                                 (make-bst 3 false false)
                                 (make-bst 5 false
                                           (make-bst 6 false false)))))


(define BST6 (make-bst A1
                       false
                       (make-bst D1
                                 false
                                 (make-bst G1
                                           false
                                           (make-bst R1
                                                     false
                                                     (make-bst W1
                                                               false
                                                               (make-bst Z1
                                                                         false
                                                                         false)
                                                               ))))))









(define-struct db (field lt? eq? bst))

(define db-quantity (make-db widget-quantity < = BST1))

(define db-price (make-db widget-price < = BST1))

(define db-name (make-db widget-name string<? string=? BST2))

(define db-price--mutate (make-db widget-price < = BST1--mutate))

(define db-name--mutate (make-db widget-name string<? string=? BST2--mutate))

(define db-name--base (make-db widget-name string<? string=? false))







(check-expect (find "Q2" db-name) false)
(check-expect (find 51 db-quantity) (make-widget "Z1" 51 16))
(check-expect (find "D1" db-name) (make-widget "D1" 5 5))
(check-expect (find 16 db-price) (make-widget "Z1" 51 16))

(define (find val database)
  (local
    [(define (find-for-bst val b field lt? eq?)
       (cond [(false? b) false]
             [(eq? val (field (bst-widget b))) (bst-widget b)]
             [(lt? val (field (bst-widget b)))
              (find-for-bst val (bst-left b) field lt? eq?)]
             [else
              (find-for-bst val (bst-right b) field lt? eq?)]))]
    (find-for-bst val
                  (db-bst database)
                  (db-field database)
                  (db-lt? database)
                  (db-eq? database))))






(check-expect (db-bst (insert Z1 db-name))  
              (make-bst
               (make-widget "D1" 5 5)
               (make-bst (make-widget "A1" 2 3)
                         false
                         false)
               (make-bst (make-widget "W1" 1 1)
                         false
                         (make-bst (make-widget "Z1" 51 16)
                                   false
                                   false))))

(check-expect (db-bst (insert R1 db-price))
              (make-bst
               (make-widget "D1" 5 5)
               (make-bst (make-widget "A1" 2 3) false false)
               (make-bst (make-widget "W1" 1 1) false
                         (make-bst (make-widget "Z1" 51 16)
                                   (make-bst (make-widget "R1" 0 8)
                                             false
                                             false)
                                   false))))

(define (insert w database)
  (local
    [(define (insert-for-bst w b field lt? eq?)
       (cond [(false? b)
              (make-bst
               w
               false
               false)]
             [(lt? (field w) (field (bst-widget b)))
              (make-bst
               (bst-widget b)
               (insert-for-bst w (bst-left b) field lt? eq?)
               (bst-right b))]
             [else
              (make-bst
               (bst-widget b)
               (bst-left b)
               (insert-for-bst w (bst-right b) field lt? eq?))]))]
    (make-db (db-field database)
             (db-lt? database)
             (db-eq? database) 
             (insert-for-bst w 
                             (db-bst database) 
                             (db-field database) 
                             (db-lt? database) 
                             (db-eq? database)))))








(check-expect (db-bst (insert! Z1 db-name--mutate))  
              (make-bst
               (make-widget "D1" 5 5)
               (make-bst (make-widget "A1" 2 3) false false)
               (make-bst (make-widget "W1" 1 1) false
                         (make-bst (make-widget "Z1" 51 16) false false))))


(check-expect (db-bst (insert! R1 db-price--mutate))
              (make-bst
               (make-widget "D1" 5 5)
               (make-bst (make-widget "A1" 2 3) false false)
               (make-bst (make-widget "W1" 1 1) false
                         (make-bst (make-widget "Z1" 51 16)
                                   (make-bst (make-widget "R1" 0 8)
                                             false
                                             false)
                                   false))))


(define (insert! w database)
  (if (false? (db-bst database))
      (make-db  (db-field database) 
                (db-lt? database) 
                (db-eq? database)
                (make-bst w false false))
      (begin
        (local
          [(define (insert-for-bst w b field lt? eq?)
             (cond [(lt? (field w) (field (bst-widget b)))
                    (if (false? (bst-left b))
                        (set-bst-left! b (make-bst w false false))
                        (insert-for-bst w (bst-left b) field lt? eq?))]
                   [else
                    (if (false? (bst-right b))
                        (set-bst-right! b (make-bst w false false))
                        (insert-for-bst w (bst-right b) field lt? eq?))]))]
          (insert-for-bst w 
                          (db-bst database) 
                          (db-field database) 
                          (db-lt? database) 
                          (db-eq? database)))
        database)))







(check-expect (balance BST5) (make-bst 4
                                       (make-bst 2
                                                 (make-bst 1
                                                           false
                                                           false)
                                                 (make-bst 3
                                                           false
                                                           false))
                                       (make-bst 5
                                                 false
                                                 (make-bst 6
                                                           false
                                                           false))))



(check-expect (height-diff (balance (balance BST6))) -1)


(define (balance b)
  (cond
    [(> (height-diff b) 1)
     (cond [(> (height-diff (bst-left b)) 0)
            
            (local [(define A b)
                    (define B (bst-left b))
                    (define C (bst-left B))]
              (make-bst (bst-widget B)
                        C
                        (make-bst (bst-widget A)
                                  (bst-right B)
                                  (bst-right A))))]
           [(< (height-diff (bst-left b)) 0)
            
            (local [(define A b)
                    (define B (bst-left b))
                    (define C (bst-right B))]
              (make-bst (bst-widget C)
                        (make-bst (bst-widget B)
                                  (bst-left B)
                                  (bst-left C))
                        (make-bst (bst-widget A)
                                  (bst-right C)
                                  (bst-right A))))]
           [else b])]
        
    [(< (height-diff b) -1)
     (cond [(< (height-diff (bst-right b)) 0)
            
            (local [(define A b)
                    (define B (bst-right b))
                    (define C (bst-right B))]
              (make-bst (bst-widget B)
                        (make-bst (bst-widget A)
                                  (bst-left A)
                                  (bst-left B))
                        C))]
           [(> (height-diff (bst-right b)) 0)
            
            (local [(define A b)
                    (define B (bst-right b))
                    (define C (bst-left B))]
              (make-bst (bst-widget C)
                        (make-bst (bst-widget A)
                                  (bst-left A)
                                  (bst-left C))
                        (make-bst (bst-widget B)
                                  (bst-right C)
                                  (bst-right B))))]
           [else b])]
         
    [else b]))








(check-expect (db-bst (insert-avl Z1 db-name))  
              (make-bst
               D1
               (make-bst A1 false false)
               (make-bst W1 false
                         (make-bst Z1 false false))))


(check-expect (db-bst (insert-avl R1 db-price))
              (make-bst W1
                        (make-bst D1
                                  (make-bst A1
                                            false
                                            false)
                                  false)
                        (make-bst Z1
                                  (make-bst R1
                                            false
                                            false)
                                  false)))


(define (insert-avl w database)
  (local
    [(define (insert-for-bst w b field lt? eq?)
       (cond [(false? b)
              (make-bst
               w
               false
               false)]
             [(lt? (field w) (field (bst-widget b)))
              (make-bst(bst-widget b)
                       (balance (insert-for-bst w (bst-left b) field lt? eq?))
                       (bst-right b))]
             [else
              (make-bst(bst-widget b)
                       (balance (bst-left b))
                       (insert-for-bst w (bst-right b) field lt? eq?))]))]
    (make-db (db-field database)
             (db-lt? database)
             (db-eq? database) 
             (balance (insert-for-bst w 
                                      (db-bst database) 
                                      (db-field database) 
                                      (db-lt? database) 
                                      (db-eq? database))))))





(define (time-insert)
  (begin (time (foldr insert db-name--base (random-widgets 250000 10000000)))
         (set! db-name--base (make-db widget-name string<? string=? false))
         (time (foldr insert! db-name--base (random-widgets 250000 10000000)))
         (set! db-name--base (make-db widget-name string<? string=? false))
         "done"))





(define (time-find)
  (begin (local [(define low (random-widgets 10000 100000))
                 (define db-name--base
                   (make-db widget-name string<? string=? false))
                 (define avl (db-bst (foldr insert-avl db-name--base low)))
                 (define bst (db-bst (foldr insert! db-name--base low)))
                 (define (time-find-amount tree acu)
                   (if (> acu 10) 
                       "stop"
                       (begin
                         (time-find--inner tree low)
                         (time-find-amount tree (add1 acu)))))]
           (begin
             (time (time-find-amount avl 1))
             (time (time-find-amount bst 1))))
         (set! db-name--base (make-db widget-name string<? string=? false))
         "done"))

(define (time-find--inner tree low) 
  (map (lambda (w) 
         (find (widget-name w) (make-db widget-name string<? string=? tree)))
       low))




 








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