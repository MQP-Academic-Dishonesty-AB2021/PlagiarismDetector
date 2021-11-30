

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Assignment 5 Pt 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))




(require 2htdp/image)

(define-struct widget (name quantity price))




(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))






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


(define DB-empty (make-db widget-name string<? string=? false))
(define DB-name (make-db widget-name string<? string=? (make-bst W1 (make-bst A1 false false)
                                                                 (make-bst Z1 false false)))) 
(define DB-quantity (make-db widget-quantity < = (make-bst A1 (make-bst W1 false false)
                                                           (make-bst D1 false false)))) 
(define DB-price (make-db widget-price < = (make-bst D1 (make-bst A1 false false)
                                                     (make-bst Z1 false false)))) 
(define DB-reverse (make-db widget-name string>? string=? (make-bst W1 (make-bst Z1 false false)
                                                                    (make-bst A1 false false))))





(check-expect (same? "W1" DB-name) true)

(check-expect (same? 5 DB-quantity) false)

(check-expect (same? "A1" DB-reverse) false)


(define (same? val db) ((db-eq? db) val ((db-field db)
                                         (bst-widget (db-bst db)))))





(check-expect (smaller? "A1" DB-name) true) 

(check-expect (smaller? 5 DB-quantity) false)

(check-expect (smaller? "A1" DB-reverse) false)


(define (smaller? val db) ((db-lt? db) val ((db-field db)
                                            (bst-widget (db-bst db)))))





(check-expect (find "A1" DB-empty) false)

(check-expect (find "W1" DB-name) W1)

(check-expect (find 1 DB-quantity) W1)

(check-expect (find 5 DB-price) D1)

(check-expect (find "D1" DB-name) false)

(check-expect (find "A1" DB-reverse) A1)


(define (find val db)
  (cond [(false? (db-bst db)) false]
        [(same? val db)
         (bst-widget (db-bst db))]
        [(smaller? val db)
         (find val (make-db (db-field db) (db-lt? db) (db-eq? db) (bst-left (db-bst db))))]
        [else
         (find val (make-db (db-field db) (db-lt? db) (db-eq? db) (bst-right (db-bst db))))]))




(check-expect (db-bst (insert A1 DB-empty)) (db-bst (make-db widget-name string<? string=? (make-bst A1 false false))))

(check-expect (db-bst (insert A1 DB-name)) (db-bst DB-name))

(check-expect (db-bst (insert D1 DB-name)) (db-bst (make-db widget-name string<? string=?
                                                            (make-bst W1
                                                                      (make-bst A1 false
                                                                                (make-bst D1 false false))
                                                                      (make-bst Z1 false false)))))

(check-expect (db-bst (insert Z1 DB-quantity)) (db-bst (make-db widget-quantity < =
                                                                (make-bst A1 (make-bst W1 false false)
                                                                          (make-bst D1 false
                                                                                    (make-bst Z1 false false))))))

(check-expect (db-bst (insert W1 DB-price)) (db-bst (make-db widget-price < =
                                                             (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
                                                                       (make-bst Z1 false false)))))

(check-expect (db-bst (insert D1 DB-reverse)) (db-bst (make-db widget-name string>? string=?
                                                               (make-bst W1 (make-bst Z1 false false)
                                                                         (make-bst A1 (make-bst D1 false false) false)))))


(define (insert widget db)
  (local [(define (create-db b) (make-db (db-field db) (db-lt? db) (db-eq? db) b))]
    (cond [(false? (db-bst db))
           (create-db (make-bst widget false false))]
          [(same? ((db-field db) widget) db) db]
          [(smaller? ((db-field db) widget) db)
           (create-db (make-bst (bst-widget (db-bst db))
                                (db-bst (insert widget (create-db (bst-left (db-bst db)))))
                                (bst-right (db-bst db))))]
          [else
           (create-db (make-bst (bst-widget (db-bst db))
                                (bst-left (db-bst db))
                                (db-bst (insert widget (create-db (bst-right (db-bst db)))))))])))




(check-expect (db-bst (insert! A1 DB-empty)) (db-bst (make-db widget-name string<? string=? (make-bst A1 false false))))

(check-expect (db-bst (insert! A1 DB-name)) (db-bst DB-name))

(check-expect (db-bst (insert! D1 DB-name)) (db-bst (make-db widget-name string<? string=?
                                                             (make-bst W1
                                                                       (make-bst A1 false
                                                                                 (make-bst D1 false false))
                                                                       (make-bst Z1 false false)))))

(check-expect (db-bst (insert! Z1 DB-quantity)) (db-bst (make-db widget-quantity < =
                                                                 (make-bst A1 (make-bst W1 false false)
                                                                           (make-bst D1 false
                                                                                     (make-bst Z1 false false))))))

(check-expect (db-bst (insert! W1 DB-price)) (db-bst (make-db widget-price < =
                                                              (make-bst D1 (make-bst A1 (make-bst W1 false false) false)
                                                                        (make-bst Z1 false false)))))

(check-expect (db-bst (insert! D1 DB-reverse)) (db-bst (make-db widget-name string>? string=?
                                                                (make-bst W1 (make-bst Z1 false false)
                                                                          (make-bst A1 (make-bst D1 false false) false)))))


(define (insert! widget db)
  (local [(define (insert bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [((db-lt? db) ((db-field db) widget) ((db-field db)
                                                        (bst-widget bst)))
                   (if (false? (bst-left bst))
                       (begin (set-bst-left! bst (make-bst widget false false))
                              bst)
                       (begin (insert (bst-left bst)) bst))]
                  [else
                   (if (false? (bst-right bst))
                       (begin (set-bst-right! bst (make-bst widget false false))
                              bst)
                       (begin (insert (bst-right bst)) bst))]))]
    (cond [(false? (db-bst db))
           (begin
             (set-db-bst! db (make-bst widget false false))
             db)]
          [(widget? (find ((db-field db) widget) db)) db]
          [else
           (make-db (db-field db)
                    (db-lt? db)
                    (db-eq? db)
                    (insert (db-bst db)))])))




(check-expect (db-bst (insert-avl (make-widget "0" 0 0) (make-db widget-price < = false)))
              (db-bst (make-db widget-price < = (make-bst (make-widget "0" 0 0) false false))))

(check-expect (db-bst (insert-avl (make-widget "0" 0 0)
                                  (make-db widget-price < = (make-bst (make-widget "0" 0 0) false false))))
              (db-bst (make-db widget-price < = (make-bst (make-widget "0" 0 0) false false))))

(check-expect (db-bst (insert-avl (make-widget "0" 0 0)
                                  (make-db widget-quantity
                                           < =
                                           (make-bst (make-widget "3" 3 0)
                                                     (make-bst (make-widget "2" 2 0)
                                                               (make-bst (make-widget "1" 1 0)
                                                                         false false) false)
                                                     (make-bst (make-widget "4" 4 0) false false)))))
              (db-bst (make-db widget-quantity
                               < =
                               (make-bst (make-widget "2" 2 0)
                                         (make-bst (make-widget "1" 1 0)
                                                   (make-bst (make-widget "0" 0 0) false false)
                                                   false)
                                         (make-bst (make-widget "3" 3 0) false
                                                   (make-bst (make-widget "4" 4 0) false false))))))

(check-expect (db-bst (insert-avl (make-widget "3" 0 3)
                                  (make-db widget-price < =
                                           (make-bst (make-widget "1" 0 1)
                                                     (make-bst (make-widget "0" 0 0) false false)
                                                     (make-bst (make-widget "2" 0 2) false
                                                               (make-bst (make-widget "4" 0 4) false false))))))
              (db-bst (make-db widget-price < =
                               (make-bst (make-widget "2" 0 2)
                                         (make-bst (make-widget "1" 0 1)
                                                   (make-bst (make-widget "0" 0 0) false false) false)
                                         (make-bst (make-widget "4" 0 4)
                                                   (make-bst (make-widget "3" 0 3) false false) false)))))


(define (insert-avl widget db) 
  (local [(define (create-db b) (make-db (db-field db) (db-lt? db) (db-eq? db) b))]
    (cond [(false? (db-bst db))
           (create-db (balance (make-bst widget false false)))]
          [(same? ((db-field db) widget) db) db]
          [(smaller? ((db-field db) widget) db)
           (create-db (balance (make-bst (bst-widget (db-bst db))
                                         (db-bst (insert widget (create-db (bst-left (db-bst db)))))
                                         (bst-right (db-bst db)))))]
          [else
           (create-db (balance (make-bst (bst-widget (db-bst db))
                                         (bst-left (db-bst db))
                                         (db-bst (insert widget (create-db (bst-right (db-bst db))))))))])))




(check-expect (balance (make-bst (make-widget "parent" 0 0)
                                 (make-bst (make-widget "2" 0 0)
                                           (make-bst (make-widget "1" 0 0)
                                                     (make-bst (make-widget "0" 0 0)
                                                               false false) false) false)
                                 (make-bst (make-widget "3" 0 0) false false)))
              (make-bst (make-widget "2" 0 0)
                        (make-bst (make-widget "1" 0 0)
                                  (make-bst (make-widget "0" 0 0) false false)
                                  false)
                        (make-bst (make-widget "parent" 0 0) false
                                  (make-bst (make-widget "3" 0 0) false false))))

(check-expect (balance (make-bst (make-widget "parent" 0 0)
                                 (make-bst (make-widget "2" 0 0)
                                           (make-bst (make-widget "0" 0 0) false
                                                     (make-bst (make-widget "1" 0 0)
                                                               false false)) false)
                                 (make-bst (make-widget "3" 0 0) false false)))
              (make-bst (make-widget "2" 0 0)
                        (make-bst (make-widget "0" 0 0) false
                                  (make-bst (make-widget "1" 0 0) false false))
                        (make-bst (make-widget "parent" 0 0) false
                                  (make-bst (make-widget "3" 0 0) false false))))

(check-expect (balance (make-bst (make-widget "parent" 0 0)
                                 (make-bst (make-widget "0" 0 0) false false)
                                 (make-bst (make-widget "1" 0 0)
                                           false
                                           (make-bst (make-widget "3" 0 0)
                                                     (make-bst (make-widget "2" 0 0) false false)
                                                     false))))
              (make-bst (make-widget "1" 0 0)
                        (make-bst (make-widget "parent" 0 0)
                                  (make-bst (make-widget "0" 0 0) false false) false)
                        (make-bst (make-widget "3" 0 0)
                                  (make-bst (make-widget "2" 0 0) false false) false)))

(check-expect (balance (make-bst (make-widget "parent" 0 0)
                                 (make-bst (make-widget "0" 0 0) false false)
                                 (make-bst (make-widget "1" 0 0)
                                           false
                                           (make-bst (make-widget "2" 0 0)
                                                     false
                                                     (make-bst (make-widget "3" 0 0) false false)))))
              (make-bst (make-widget "1" 0 0)
                        (make-bst (make-widget "parent" 0 0)
                                  (make-bst (make-widget "0" 0 0) false false) false)
                        (make-bst (make-widget "2" 0 0)
                                  false
                                  (make-bst (make-widget "3" 0 0) false false))))


(check-expect (balance (make-bst (make-widget "6" 0 0)
                                 (make-bst (make-widget "3" 0 0)
                                           (make-bst (make-widget "2" 0 0) false false)
                                           (make-bst (make-widget "4" 0 0)
                                                     false
                                                     (make-bst (make-widget "5" 0 0) false false)))
                                 (make-bst (make-widget "7" 0 0) false false)))
              (make-bst (make-widget "4" 0 0)
                        (make-bst (make-widget "3" 0 0) 
                                  (make-bst (make-widget "2" 0 0) false false) false)
                        (make-bst (make-widget "6" 0 0)
                                  (make-bst (make-widget "5" 0 0) false false)
                                  (make-bst (make-widget "7" 0 0) false false))))


(define (balance bst)
  (cond [(> (height-diff bst) 1) 
         (if (> (height-diff (bst-left bst)) 0)
             (left-rotate bst)
             (balance (make-bst (bst-widget bst)
                                (right-rotate (bst-left bst))
                                (bst-right bst))))]
        [(< (height-diff bst) -1) 
         (if (> (height-diff (bst-right bst)) 0)
             (balance (make-bst (bst-widget bst)
                                (bst-left bst)
                                (left-rotate (bst-right bst))))
             (right-rotate bst))]                             
        [else bst]))



(check-expect (left-rotate (make-bst (make-widget "2" 0 0)
                                     (make-bst (make-widget "1" 0 0)
                                               (make-bst (make-widget "0" 0 0) false false) false) false))
              (make-bst (make-widget "1" 0 0)
                        (make-bst (make-widget "0" 0 0) false false)
                        (make-bst (make-widget "2" 0 0) false false)))
(check-expect (left-rotate (make-bst (make-widget "2" 0 0)
                                     (make-bst (make-widget "0" 0 0) false
                                               (make-bst (make-widget "1" 0 0) false false)) false))
              (make-bst (make-widget "0" 0 0) false
                        (make-bst (make-widget "2" 0 0)
                                  (make-bst (make-widget "1" 0 0) false false) false)))

(define (left-rotate bst)
  (make-bst (bst-widget (bst-left bst))
            (bst-left (bst-left bst))
            (make-bst (bst-widget bst)
                      (bst-right (bst-left bst))
                      (bst-right bst))))



(check-expect (right-rotate (make-bst (make-widget "0" 0 0) false
                                      (make-bst (make-widget "1" 0 0) false
                                                (make-bst (make-widget "2" 0 0) false false))))
              (make-bst (make-widget "1" 0 0)
                        (make-bst (make-widget "0" 0 0) false false)
                        (make-bst (make-widget "2" 0 0) false false)))
(check-expect (right-rotate (make-bst (make-widget "0" 0 0) false
                                      (make-bst (make-widget "2" 0 0)
                                                (make-bst (make-widget "1" 0 0) false false) false)))
              (make-bst (make-widget "2" 0 0)
                        (make-bst (make-widget "0" 0 0) false
                                  (make-bst (make-widget "1" 0 0) false false)) false))

(define (right-rotate bst)
  (make-bst (bst-widget (bst-right bst))
            (make-bst (bst-widget bst)
                      (bst-left bst)
                      (bst-left (bst-right bst)))
            (bst-right (bst-right bst))))


 




(define (random-widgets num max)
  (build-list num
              (λ(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))





(define (build-tree low)
  (local [(define (inserting db low)
            (cond [(empty? low) db]
                  [else (inserting (insert (first low) db) (rest low))]))]
    (db-bst (inserting (make-db widget-name string<? string=? false) low))))


(define time-insert
  (local [(define insert-widgets (random-widgets 250000 250000))]
    (begin (time (build-tree insert-widgets)) "done")))



(define (build-tree! low)
  (local [(define (inserting db low)
            (cond [(empty? low) db]
                  [else (inserting (insert! (first low) db) (rest low))]))]
    (db-bst (inserting (make-db widget-name string<? string=? false) low))))

(define time-insert!
  (local [(define insert-widgets (random-widgets 250000 250000))]
    (begin (time (build-tree! insert-widgets)) "done")))



(define (find-all db low)
  (cond [(empty? low) db]
        [else (begin (find ((db-field db) (first low)) db)
                     (find-all db (rest low)))]))

(define time-find
  (local [(define ten-thousand (random-widgets 10000 10000))
          (define tree-db (make-db widget-quantity < = (build-tree ten-thousand)))]
    (begin (time (find-all tree-db ten-thousand)) "done")))



(define (build-tree-avl low)
  (local [(define (inserting db low)
            (cond [(empty? low) db]
                  [else (inserting (insert-avl (first low) db) (rest low))]))]
    (db-bst (inserting (make-db widget-name string<? string=? false) low))))

(define time-find-avl
  (local [(define ten-thousand (random-widgets 10000 10000))
          (define tree-db-avl (make-db widget-quantity < = (build-tree-avl ten-thousand)))]
    (begin (time (find-all tree-db-avl ten-thousand)) "done")))









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