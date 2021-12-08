

#reader(lib "htdp-advanced-reader.ss" "lang")((modname Assignment5-Part3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)


(define-struct widget (name quantity price))



(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define B1 (make-widget "B1" 0 6))
(define Z2 (make-widget "Z2" 10 15))

(define-struct bst (widget left right))





(define BST-D1 (make-bst D1 (make-bst B1 (make-bst A1 false false) (make-bst Z1 false false))
                         false))
(define BST-A1 (make-bst A1 false false))
(define BST-B1 (make-bst B1 false false))
(define BST-D1s (make-bst D1 (make-bst A1 false false) (make-bst W1 false false)))
(define BST-D1c (make-bst D1 (make-bst A1 false false)
                          (make-bst W1 false (make-bst Z1 false false))))
(define BST-Z2-QUANTITY (make-bst Z2 (make-bst D1 (make-bst A1 false false) false)
                                  (make-bst Z1 false false)))
(define BST-B1-PRICE (make-bst B1 (make-bst A1 false (make-bst D1 false false))
                               (make-bst Z2 false (make-bst Z1 false false))))
(define BST-A1-PRICE-REVERSE
  (make-bst A1 (make-bst D1 (make-bst Z2 (make-bst Z1 false false)
                                      (make-bst B1 false false)) false)
            (make-bst W1 false false)))


(define BST-REVERSE-AVL-SORT
  (make-bst A1 (make-bst Z2 (make-bst Z1 false false)
                         (make-bst D1 (make-bst B1 false false) false))
            (make-bst W1 false false)))
(define BST-Z2-QUANTITY-SORT
  (make-bst Z2 (make-bst D1 (make-bst A1 (make-bst W1 false false) false) false)
            (make-bst Z1 false false)))


(define-struct db (field lt? eq? bst))







(check-expect (db-bst (insert! A1 (make-db widget-name string<? string=? false))) BST-A1)

(check-expect (db-bst(insert! B1 (make-db widget-name string<? string=? BST-A1)))
              (make-bst A1 false BST-B1))

(check-expect (db-bst(insert! A1 (make-db widget-quantity < =
                                          (make-bst Z2 (make-bst D1 false false)
                                                    (make-bst Z1 false false))))) BST-Z2-QUANTITY)

(check-expect (db-bst(insert! Z1 (make-db widget-price < =
                                          (make-bst B1 (make-bst A1 false (make-bst D1 false false))
                                                    (make-bst Z2 false false))))) BST-B1-PRICE)

(check-expect
 (db-bst(insert! B1 (make-db widget-price > = (make-bst A1
                                                        (make-bst
                                                         D1 (make-bst Z2 (make-bst Z1 false false)
                                                                      false) false)
                                                        (make-bst W1 false false)))))
 BST-A1-PRICE-REVERSE)



(define (insert! widget db)
  (local [
          (define (insert-bst bst pbst)
            (cond
              [(false? bst)
               (cond
                 [(false? pbst) (set-db-bst! db (make-bst widget false false))]
                 [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget pbst)))
                  (set-bst-left! pbst (make-bst widget false false))]
                 [else (set-bst-right! pbst (make-bst widget false false))])]
              
              [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
               (insert-bst (bst-left bst) bst)]
              
              [else (insert-bst (bst-right bst) bst)]))]
    (begin (insert-bst (db-bst db) false) db)))








(check-expect (db-bst (insert-avl A1 (make-db widget-name string<? string=? false)))
              (make-bst A1 false false))

(check-expect (db-bst(insert-avl B1 (make-db widget-name string<? string=?
                                             (make-bst A1 false false))))
              (make-bst A1 false BST-B1))

(check-expect (db-bst(insert-avl A1 (make-db widget-quantity < =
                                             (make-bst Z2 (make-bst D1 false false)
                                                       (make-bst Z1 false false)))))
              (make-bst Z2 (make-bst D1 (make-bst A1 false false) false)
                        (make-bst Z1 false false)))

(check-expect (db-bst(insert-avl W1 (make-db widget-price < =
                                             (make-bst B1 (make-bst A1 false
                                                                    (make-bst D1 false false))
                                                       (make-bst Z2 false false)))))
              (make-bst B1 (make-bst A1 (make-bst W1 false false) (make-bst D1 false false))
                        (make-bst Z2 false false)))

(check-expect (db-bst(insert-avl
                      B1 (make-db widget-price > =
                                  (make-bst A1
                                            (make-bst D1 (make-bst Z2 false false) false)
                                            (make-bst W1 false false)))))
              (make-bst A1 (make-bst B1 (make-bst Z2 false false) (make-bst D1 false false))
                        (make-bst W1 false false))) 

(check-expect (db-bst(insert-avl W1 (make-db widget-quantity < =
                                             (make-bst D1
                                                       (make-bst B1 false (make-bst A1 false false))
                                                       (make-bst Z2 false false)))))
              (make-bst D1 (make-bst W1 (make-bst B1 false false) (make-bst A1 false false))
                        (make-bst Z2 false false)))



(define (insert-avl widget db)
  (local [
          (define (rotate-lr avl)
            (rotate-l (make-bst (bst-widget avl)
                                (rotate-r (bst-left avl))
                                (bst-right avl)))) 
          (define (rotate-rl avl)
            (rotate-r (make-bst (bst-widget avl)
                                (bst-left avl)
                                (rotate-l (bst-right avl)))))
          (define (rotate-r avl)
            (make-bst (if (false? (bst-right avl)) false (bst-widget (bst-right avl)))
                      (make-bst (bst-widget avl) (bst-left avl) false)
                      (if (false? (bst-right avl)) false (bst-right (bst-right avl)))))
          (define (rotate-l avl)
            (make-bst (if (false? (bst-left avl)) false (bst-widget (bst-left avl)))
                      (if (false? (bst-left avl)) false (bst-left (bst-left avl)))
                      (make-bst (bst-widget avl) false (bst-right avl))))

          (define (balance avl)
            (cond [(< 1 (height-diff avl))
                   (if (< 0 (height-diff (bst-left avl)))
                       (rotate-l avl)
                       (rotate-lr avl))]
                  [(> -1 (height-diff avl))
                   (if (< 0 (height-diff (bst-right avl)))
                       (rotate-rl avl)
                       (rotate-r avl))]
                  [else avl]))
          
          (define (insert bst)
            (cond
              [(false? bst) (make-bst widget false false)]
              [((db-lt? db) ((db-field db) widget) ((db-field db) (bst-widget bst)))
               (make-bst (bst-widget bst) (balance (insert (bst-left bst))) (bst-right bst))]
              [else (make-bst (bst-widget bst) (bst-left bst)
                              (balance (insert (bst-right bst))))]))]
    
    (make-db (db-field db) (db-lt? db) (db-eq? db) (balance (insert (db-bst db))))))



















































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








(define (smaller? k b)
  (string<? k (widget-name (bst-widget b))))

(define (insert-name v b)
  (local [
          (define (insert-name-inner b)
            (cond [(false? b) (make-bst v false false)]
                  [(smaller? (widget-name v) b)
                   (make-bst (bst-widget b) (insert-name-inner (bst-left b)) (bst-right b))]
                  [else (make-bst (bst-widget b) (bst-left b) (insert-name-inner (bst-right b)))]))]
    (insert-name-inner b)))

(define (find k db)
  (local[(define (find-bst bst)
           (cond[(false? bst) false]
                [((db-eq? db) k ((db-field db) (bst-widget bst))) (bst-widget bst)]
                [((db-lt? db) k ((db-field db) (bst-widget bst))) (find-bst  (bst-left bst))]
                [else (find-bst (bst-right bst))]))]
    (find-bst (db-bst db))))

(define (build-tree low)
  (foldr insert-name false low))

(define (build-tree! low)
  (foldr insert! (make-db widget-name string<? string=? false) low))

(define (build-tree-avl low)
  (foldr insert-avl (make-db widget-name string<? string=? false) low))

(define 250000widgets (random-widgets 250000 99999999))

(define (time-find a)
  (local [
          (define 10000widgets (random-widgets 250000 99999999))
          (define big-bst (db-bst (build-tree! 10000widgets)))
          (define big-avl (db-bst (build-tree-avl 10000widgets)))]
    (begin (time (map (λ (w) (find (widget-name w)
                                   (make-db widget-name string<? string=? big-bst))) 10000widgets))
           "done"
           (time (map (λ (w) (find (widget-name w)
                                   (make-db widget-name string<? string=? big-avl))) 10000widgets))
           "done")))
 

