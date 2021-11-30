

#reader(lib "htdp-advanced-reader.ss" "lang")((modname assignment-5-part-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


(require 2htdp/image)




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

(define-struct widget (name quantity price))




(define-struct bst (widget left right))




(define-struct db (field lt? eq? bst))



(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 53 2))

(define BST1 (make-bst A1 false false))
(define BST2 (make-bst Z1 false false))
(define BST3 (make-bst W1 false BST2))
(define BST4 (make-bst D1 BST1 BST3))
(define BST5 (make-bst D1 false BST3))
(define BST6 (make-bst Z1 (make-bst W1 BST1 false) false))

(define DB-quantity (make-db widget-quantity < = BST4))
(define DB-name (make-db widget-name string<? string=? BST4))
(define DB-name-copy (make-db widget-name string<? string=? BST4))
(define DB-name2 (make-db widget-name string<? string=? false))
(define DB-name3 (make-db widget-name string<? string=? BST1))
(define DB-price (make-db widget-price < = BST4))




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





(check-expect (db-bst (insert Z1 DB-name3)) 
              (make-bst A1
                        false
                        (make-bst Z1 false false)))
(check-expect (db-bst (insert B1 DB-name)) 
              (make-bst D1
                        (make-bst A1 false (make-bst B1 false false))
                        BST3))
(check-expect (db-bst (insert B1 DB-price))
              (make-bst D1
                        (make-bst A1 (make-bst B1 false false) false)
                        BST3))

(define (insert widget db)
  (local [(define (insert-in-bst bst)
            (cond
              [(false? bst)
               (make-bst widget false false)]
              [((db-lt? db) ((db-field db) widget)
                            ((db-field db) (bst-widget bst)))
               (make-bst (bst-widget bst)
                         (insert-in-bst (bst-left bst))
                         (bst-right bst))]
              [else
               (make-bst (bst-widget bst)
                         (bst-left bst)
                         (insert-in-bst (bst-right bst)))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-in-bst (db-bst db)))))





(check-expect (db-bst (insert-avl Z1 DB-name3)) 
              (make-bst A1
                        false
                        (make-bst Z1 false false)))
(check-expect (db-bst (insert-avl B1 DB-name)) 
              (make-bst D1
                        (make-bst A1
                                  false
                                  (make-bst B1 false false))
                        BST3))
(check-expect (db-bst (insert-avl B1 DB-price))
              (make-bst D1
                        (make-bst A1
                                  (make-bst B1 false false)
                                  false)
                        BST3))

(define (insert-avl widget db)
  (local [(define (insert-in-bst bst)
            (cond
              [(false? bst)
               (make-bst widget false false)]
              [((db-lt? db) ((db-field db) widget)
                            ((db-field db) (bst-widget bst)))
               (make-bst (bst-widget bst)
                         (balance (insert-in-bst (bst-left bst)))
                         (bst-right bst))]
              [else
               (make-bst (bst-widget bst)
                         (bst-left bst)
                         (balance (insert-in-bst (bst-right bst))))]))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-in-bst (db-bst db)))))






(check-expect (balance BST4) BST4)
(check-expect (balance BST5) (make-bst W1
                                       (make-bst D1 false false)
                                       BST2))
(check-expect (balance BST6) (make-bst W1
                                       BST1
                                       (make-bst Z1 false false)))

(define (balance bst)
  (cond
    [(>= 1 (abs (height-diff bst))) bst]
    [(negative? (height-diff bst))
     (make-bst (bst-widget (bst-right bst))
               (make-bst (bst-widget bst)
                         (if (false? (bst-left bst))
                             false
                             (bst-left (bst-left bst)))
                         (if (false? (bst-left bst))
                             false
                             (bst-right (bst-left bst))))
               (if (false? (bst-right bst))
                   false
                   (bst-right (bst-right bst))))]
    [(positive? (height-diff bst))
     (make-bst (bst-widget (bst-left bst))
               (if (false? (bst-left bst))
                   false
                   (bst-left (bst-left bst)))
               (make-bst (bst-widget bst)
                         (if (false? (bst-right bst))
                             false
                             (bst-left (bst-right bst)))
                         (bst-right bst)))]))
                         




(check-expect (db-bst (insert! B1 DB-name)) 
              (make-bst D1
                        (make-bst A1
                                  false
                                  (make-bst B1 false false))
                        BST3))
(check-expect (db-bst (insert! Z1 DB-name3))
              (make-bst A1
                        false
                        (make-bst B1 false (make-bst Z1 false false))))
(check-expect (db-bst (insert! B1 DB-price))
              (make-bst D1
                        (make-bst A1
                                  (make-bst B1 false false)
                                  (make-bst B1
                                            false
                                            (make-bst Z1 false false)))
                        BST3))

(define (insert! widget db)
  (local [(define (insert-in-bst bst)
            (cond
              [(false? bst) (make-bst widget false false)]
              [((db-lt? db) ((db-field db) widget)
                            ((db-field db) (bst-widget bst)))
               (if (false? (bst-left bst))
                   (set-bst-left! bst (make-bst widget false false))
                   (insert-in-bst (bst-left bst)))]
              [else
               (if (false? (bst-right bst))
                   (set-bst-right! bst (make-bst widget false false))
                   (insert-in-bst (bst-right bst)))]))]
    (begin (insert-in-bst (db-bst db)) db)))






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
       (list->string (build-list len (λ (dummy)
                                       (integer->char (+ 97 (random 26)))))))]
    (build-list num
                (λ(dummy)
                  (make-widget
                   (random-string slen) 
                   (random nmax)
                   (random nmax))))))






(check-expect (smaller? "B1" BST1) false)
(check-expect (smaller? "B1" BST4) true)

(define (smaller? key bst)
  (string<? key (widget-name (bst-widget bst))))





(check-expect (same? "A1" BST1) true)
(check-expect (same? "A1" BST4) false)

(define (same? key bst)
  (equal? key (widget-name (bst-widget bst))))

(define (find-name key bst)
  (cond
    [(false? bst) false]
    [(same? key bst) (bst-widget bst)]
    [(smaller? key bst) (find-name key (bst-left bst))]
    [else (find-name key (bst-right bst))]))

(define (time-find)
  (local [(define low1 (random-widgets 10000 9999))
          (define bst (db-bst (foldr insert! DB-name low1)))
          (define low2 (random-widgets 10000 9999))
          (define avl (db-bst (foldr insert! DB-name-copy low2)))]
    (begin (time (map (λ (x y) (find-name (widget-name x) y)) low1 (build-list 10000 (λ (x) bst))))
           (time (map (λ (x y) (find-name (widget-name x) y)) low2 (build-list 10000 (λ (x) avl))))
           "done")))

(time-find)