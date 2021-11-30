

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Kai Nakamura and Aashi Mehta Assignment 5 Part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))








(require 2htdp/image)

(define-struct widget (name quantity price))





(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 1 2))
(define C1 (make-widget "C1" 3 4))
(define D1 (make-widget "D1" 5 5))
(define E1 (make-widget "E1" 6 7))
(define F1 (make-widget "F1" 8 9))
(define G1 (make-widget "G1" 10 11))
(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))

(define-struct bst (widget left right))



 
















(define-struct db (field lt? eq? bst))

(define DB-name (make-db widget-name string<? string=? false))
(define DB-quantity (make-db widget-quantity < = false))
(define DB-price (make-db widget-price < = false))









(define (compare-insertion-time widget-amount)
  (local [(define widgets (random-widgets-string widget-amount 10 1000))]
  (begin (time (insert-widgets widgets DB-name))
         (time (insert-widgets! widgets DB-name))
         "done")))






(define (compare-find-time widget-amount)
  (local [(define widgets (random-widgets-string widget-amount 10 1000))
          (define bst (insert-widgets widgets DB-name))
          (define avl (insert-avl-widgets widgets DB-name))]
  (begin (time (can-find? widgets bst))
         (time (can-find? widgets avl))
         "done")))





(check-expect (local [(define widgets (random-widgets-string 1000 10 1000))]
                (can-find? widgets (insert-widgets widgets DB-name)))
              true)

(define (insert-widgets widgets db)
  (cond [(empty? widgets) db]
        [else
         (insert-widgets (rest widgets)
                         (insert (first widgets) db))]))








(check-expect (db-bst (insert W1 DB-name)) (make-bst W1 false false))


(check-expect (db-bst
               (insert A1 (make-db widget-name string<? string=?
                                   (make-bst D1 false false))))
              (make-bst D1 (make-bst A1 false false) false))


(check-expect (db-bst
               (insert Z1 (make-db widget-name string<? string=?
                                   (make-bst D1 false false))))
              (make-bst D1 false (make-bst Z1 false false)))


(check-expect
 (render (db-bst
          (insert
           W1 (make-db widget-name string<? string=?
                       (make-bst A1 false
                                 (make-bst D1 false
                                           (make-bst Z1 false false)))))))
 (render (make-bst A1 false
                   (make-bst D1 false
                             (make-bst Z1
                                       (make-bst W1 false false)
                                       false)))))


(check-expect (db-bst
               (insert
                W1 (make-db widget-price < =
                            (make-bst A1 false (make-bst D1 false false)))))
              (make-bst A1 (make-bst W1 false false)
                        (make-bst D1 false false)))


(check-expect (db-bst
               (insert
                A1 (make-db widget-quantity < =
                            (make-bst W1 false
                                      (make-bst D1 false
                                                (make-bst Z1 false false))))))
              (make-bst W1 false (make-bst D1 (make-bst A1 false false)
                                           (make-bst Z1 false false))))

(define (insert widget db)
  (local [
          
          (define (insert--bst widget bst)
            (if (false? bst)
                (make-bst widget false false)
                (local [(define widget-value ((db-field db) widget))
                        (define current-widget (bst-widget bst))
                        (define current-value ((db-field db) current-widget))]
                  (cond [((db-lt? db) widget-value current-value)
                         (make-bst (bst-widget bst)
                                   (insert--bst widget (bst-left bst))
                                   (bst-right bst))]
                        [else
                         (make-bst (bst-widget bst)
                                   (bst-left bst)
                                   (insert--bst widget (bst-right bst)))]))))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert--bst widget (db-bst db)))))





(check-expect (local [(define widgets (random-widgets-string 1000 10 1000))]
                (can-find? widgets (insert-widgets! widgets DB-name)))
              true)

(define (insert-widgets! widgets db)
  (cond [(empty? widgets) db]
        [else
         (insert-widgets! (rest widgets)
                          (insert! (first widgets) db))]))





(check-expect (render (db-bst (insert! W1 DB-name)))
              (render (make-bst W1 false false)))


(check-expect (render (db-bst
                       (insert! A1 (make-db widget-name string<? string=?
                                            (make-bst D1 false false)))))
              (render (make-bst D1 (make-bst A1 false false) false)))


(check-expect (render (db-bst
                       (insert! Z1 (make-db widget-name string<? string=?
                                            (make-bst D1 false false)))))
              (render (make-bst D1 false (make-bst Z1 false false))))


(check-expect (render
               (db-bst
                (insert! W1
                         (make-db widget-name string<? string=?
                                  (make-bst A1 false
                                            (make-bst D1 false
                                                      (make-bst Z1 false
                                                                false)))))))
              (render
               (make-bst A1 false
                         (make-bst D1 false
                                   (make-bst Z1 (make-bst W1 false false)
                                             false)))))


(check-expect (render
               (db-bst
                (insert! W1
                         (make-db widget-name string<? string=?
                                  (make-bst Z1
                                            (make-bst D1
                                                      (make-bst A1 false false)
                                                      false)
                                            false)))))
              (render
               (make-bst Z1
                         (make-bst D1
                                   (make-bst A1 false false)
                                   (make-bst W1 false false))
                         false)))


(check-expect (render
               (db-bst
                (insert! W1 (make-db widget-price < =
                                     (make-bst A1 false
                                               (make-bst D1 false false))))))
              (render
               (make-bst A1 (make-bst W1 false false)
                         (make-bst D1 false false))))


(check-expect (render
               (db-bst
                (insert! A1 (make-db widget-quantity < =
                                     (make-bst W1 false
                                               (make-bst D1 false
                                                         (make-bst Z1 false
                                                                   false)))))))
              (render
               (make-bst W1 false (make-bst D1 (make-bst A1 false false)
                                            (make-bst Z1 false false)))))

(define (insert! widget db)
  (local [
          
          (define (insert!--bst widget bst)
            (if (false? bst)
                (make-bst widget false false)
                (local [(define widget-value ((db-field db) widget))
                        (define current-widget (bst-widget bst))
                        (define current-value ((db-field db) current-widget))]
                  (cond [((db-lt? db) widget-value current-value)
                         (if (false? (bst-left bst))
                             (begin (set-bst-left!
                                     bst
                                     (make-bst widget false false))
                                    (db-bst db))
                             (insert!--bst widget (bst-left bst)))]
                        [else
                         (if (false? (bst-right bst))
                             (begin (set-bst-right!
                                     bst
                                     (make-bst widget false false))
                                    (db-bst db))
                             (insert!--bst widget (bst-right bst)))]))))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert!--bst widget (db-bst db)))))






(check-expect (<= (abs
                   (height-diff
                    (db-bst
                     (insert-avl-widgets (random-widgets-string 1000 10 1000)
                                         DB-name))))
                  1)
              true)


(check-expect (local [(define widgets (random-widgets-string 1000 10 1000))]
                (can-find? widgets (insert-avl-widgets widgets DB-name)))
              true)

(define (insert-avl-widgets widgets db)
  (cond [(empty? widgets) db]
        [else
         (insert-avl-widgets (rest widgets)
                             (insert-avl (first widgets) db))]))






(check-expect (render (db-bst (insert-avl W1 DB-name)))
              (render (make-bst W1 false false)))


(check-expect (render (db-bst
                       (insert-avl
                        A1 (make-db widget-name string<? string=?
                                    (make-bst D1 false false)))))
              (render (make-bst D1 (make-bst A1 false false) false)))


(check-expect (render (db-bst
                       (insert-avl
                        Z1 (make-db widget-name string<? string=?
                                    (make-bst D1 false false)))))
              (render (make-bst D1 false (make-bst Z1 false false))))


(check-expect (render
               (db-bst
                (insert-avl
                 W1 (make-db widget-name string<? string=?
                             (make-bst B1 (make-bst A1 false false)
                                       (make-bst D1 false
                                                 (make-bst Z1 false false)))))))
              (render
               (make-bst B1 (make-bst A1 false false)
                         (make-bst W1 (make-bst D1 false false)
                                   (make-bst Z1 false false)))))

(check-expect (render
               (db-bst
                (insert-avl
                 W1 (make-db widget-name string<? string=?
                             (make-bst Z1
                                       (make-bst D1
                                                 (make-bst A1 false false)
                                                 false)
                                       false)))))
              (render
               (make-bst D1 (make-bst A1 false false)
                         (make-bst Z1 (make-bst W1 false false)
                                   false))))


(check-expect (render
               (db-bst
                (insert-avl
                 W1 (make-db widget-price < =
                             (make-bst A1 false
                                       (make-bst D1 false false))))))
              (render
               (make-bst A1 (make-bst W1 false false)
                         (make-bst D1 false false))))


(check-expect (render
               (db-bst
                (insert-avl
                 A1 (make-db widget-quantity < =
                             (make-bst W1 false
                                       (make-bst D1 false
                                                 (make-bst Z1 false false)))))))
              (render
               (make-bst D1 (make-bst W1 false
                                      (make-bst A1 false false))
                         (make-bst Z1 false false))))

(define (insert-avl widget db)
  (local [
          
          
          (define (insert-avl--bst widget bst)
            (if (false? bst)
                (make-bst widget false false)
                (local [(define widget-value ((db-field db) widget))
                        (define current-widget (bst-widget bst))
                        (define current-value ((db-field db) current-widget))]
                  (cond [((db-lt? db) widget-value current-value)
                         (balance
                          (make-bst (bst-widget bst)
                                    (balance
                                     (insert-avl--bst
                                      widget
                                      (bst-left bst)))
                                    (bst-right bst)))]
                        [else
                         (balance
                          (make-bst (bst-widget bst)
                                    (bst-left bst)
                                    (balance
                                     (insert-avl--bst
                                      widget
                                      (bst-right bst)))))]))))]
    (make-db (db-field db)
             (db-lt? db)
             (db-eq? db)
             (insert-avl--bst widget (db-bst db)))))






(check-expect (render
               (balance
                (make-bst A1 false
                          (make-bst D1 false
                                    (make-bst Z1 false false)))))
              (render
               (make-bst D1 (make-bst A1 false false)
                         (make-bst Z1 false false))))


(check-expect (render
               (balance
                (make-bst Z1 (make-bst D1 (make-bst A1 false false)
                                       false)
                          false)))
              (render
               (make-bst D1 (make-bst A1 false false)
                         (make-bst Z1 false false))))


(check-expect (render
               (balance
                (make-bst Z1
                          (make-bst A1 false
                                    (make-bst D1 false false))
                          false)))
              (render (make-bst D1 (make-bst A1 false false)
                                (make-bst Z1 false false))))


(check-expect (render
               (balance
                (make-bst A1 false
                          (make-bst Z1
                                    (make-bst D1 false false)
                                    false))))
              (render (make-bst D1 (make-bst A1 false false)
                                (make-bst Z1 false false))))

(define (balance bst)
  (local [(define height-diff-root (height-diff bst))]
    (cond [(> height-diff-root 1) 
           (local [(define left (bst-left bst))]
             (if (false? left)
                 (error "Could not balance, found false in left")
                 (local [(define height-diff-left (height-diff left))]
                   (cond [(< height-diff-left 0) 
                          (rotate-left-right bst)]
                         [else 
                          (rotate-right bst)]))))]
          [(< height-diff-root -1) 
           (local [(define right (bst-right bst))]
             (if (false? right)
                 (error "Could not balance, found false in right")
                 (local [(define height-diff-right (height-diff right))]
                   (cond [(> height-diff-right 0) 
                          (rotate-right-left bst)]
                         [else 
                          (rotate-left bst)]))))]
          [else bst]))) 

















(check-expect (render
               (rotate-left
                (make-bst B1 (make-bst A1 false false)
                          (make-bst D1 (make-bst C1 false false)
                                    (make-bst E1 false false)))))
              (render
               (make-bst D1 (make-bst B1 (make-bst A1 false false)
                                      (make-bst C1 false false))
                         (make-bst E1 false false))))


(check-expect (render
               (rotate-left
                (make-bst A1 false
                          (make-bst D1 false false))))
              (render
               (make-bst D1 (make-bst A1 false false) false)))


(check-error (rotate-left
              (make-bst A1 (make-bst D1 false false) false)))

(define (rotate-left bst)
  (local [(define right (bst-right bst))]
    (if (false? right)
        (error "Could not rotate bst left, no widget right of root")
        (make-bst (bst-widget right)
                  (make-bst (bst-widget bst)
                            (bst-left bst)
                            (bst-left right))
                  (bst-right right)))))
     

















(check-expect (render
               (rotate-right
                (make-bst D1 (make-bst B1 (make-bst A1 false false)
                                       (make-bst C1 false false))
                          (make-bst E1 false false))))
              (render
               (make-bst B1 (make-bst A1 false false)
                         (make-bst D1 (make-bst C1 false false)
                                   (make-bst E1 false false)))))


(check-expect (render
               (rotate-right
                (make-bst Z1 (make-bst D1 false false)
                          false)))
              (render
               (make-bst D1 false (make-bst Z1 false false))))


(check-error (rotate-right
              (make-bst Z1 false (make-bst D1 false false))))

(define (rotate-right bst)
  (local [(define left (bst-left bst))]
    (if (false? left)
        (error "Could not rotate bst right, no widget left of root")
        (make-bst (bst-widget left)
                  (bst-left left)
                  (make-bst (bst-widget bst)
                            (bst-right left)
                            (bst-right bst))))))



























(check-expect (render
               (rotate-left-right
                (make-bst F1 (make-bst B1 (make-bst A1 false false)
                                       (make-bst D1 (make-bst C1 false false)
                                                 (make-bst E1 false false)))
                          (make-bst G1 false false))))
              (render (make-bst D1 (make-bst B1 (make-bst A1 false false)
                                             (make-bst C1 false false))
                                (make-bst F1 (make-bst E1 false false)
                                          (make-bst G1 false false)))))


(check-error (rotate-left-right
              (make-bst F1
                        (make-bst B1 (make-bst A1 false false)
                                  false))))

(check-error (rotate-left-right
              (make-bst F1 false
                        (make-bst G1 false false))))

(define (rotate-left-right bst)
  (rotate-right
   (make-bst (bst-widget bst)
             (rotate-left (bst-left bst))
             (bst-right bst))))



























(check-expect (render
               (rotate-right-left
                (make-bst B1 (make-bst A1 false false)
                          (make-bst F1
                                    (make-bst D1 (make-bst C1 false false)
                                              (make-bst E1 false false))
                                    (make-bst G1 false false)))))
              (render (make-bst D1 (make-bst B1 (make-bst A1 false false)
                                             (make-bst C1 false false))
                                (make-bst F1 (make-bst E1 false false)
                                          (make-bst G1 false false)))))


(check-error (rotate-right-left
              (make-bst B1 false
                        (make-bst F1 false
                                  (make-bst G1 false false)))))

(check-error (rotate-right-left
              (make-bst B1 (make-bst A1 false false)
                        false)))

(define (rotate-right-left bst)
  (rotate-left
   (make-bst (bst-widget bst)
             (bst-left bst)
             (rotate-right (bst-right bst)))))









(check-expect (find 1 DB-quantity) false)


(check-expect
 (find 5 (make-db widget-quantity < = (make-bst D1 false false))) D1)


(check-expect (find 51 (make-db widget-quantity < =
                                (make-bst A1 false
                                          (make-bst Z1 false false)))) Z1)


(check-expect (find 1 (make-db widget-quantity < =
                               (make-bst A1 (make-bst W1 false false)
                                         false))) W1)


(check-expect (find 1 DB-price) false)


(check-expect (find 5 (make-db widget-price < = (make-bst D1 false false))) D1)


(check-expect (find 16 (make-db widget-price < =
                                (make-bst A1 false
                                          (make-bst Z1 false false)))) Z1)


(check-expect (find 1 (make-db widget-price < =
                               (make-bst A1
                                         (make-bst W1 false false) false))) W1)


(check-expect (find "W1" DB-name) false)


(check-expect (find "D1" (make-db widget-name string<? string=?
                                  (make-bst D1 false false))) D1)


(check-expect (find "W1" (make-db widget-name string<? string=?
                                  (make-bst D1 false
                                            (make-bst W1 false false)))) W1)


(check-expect (find "A1" (make-db widget-name string<? string=?
                                  (make-bst D1
                                            (make-bst A1 false false)
                                            false))) A1)

(define (find value db)
  (if (false? (db-bst db))
      false
      (local [(define bst (db-bst db))
              (define current-widget (bst-widget bst))
              (define current-value ((db-field db) current-widget))]
        (cond [((db-eq? db) value current-value)
               (bst-widget bst)]
              [((db-lt? db) value current-value)
               (find value (make-db (db-field db)
                                    (db-lt? db)
                                    (db-eq? db)
                                    (bst-left bst)))]
              [else
               (find value (make-db (db-field db)
                                    (db-lt? db)
                                    (db-eq? db)
                                    (bst-right bst)))]))))





(check-expect (can-find? (list A1)
                         (make-db widget-name string<? string=? false))
              false)


(check-expect
 (can-find?
  (list A1 B1 C1 D1 E1 F1 G1)
  (make-db widget-name string<? string=?
           (make-bst B1 (make-bst A1 false false)
                     (make-bst F1
                               (make-bst D1 (make-bst C1 false false)
                                         (make-bst E1 false false))
                               (make-bst G1 false false)))))
 true)


(check-expect
 (can-find?
  (list A1 B1 C1 D1 E1 F1 G1)
  (make-db widget-name string<? string=?
           (make-bst B1 (make-bst A1 false false)
                     (make-bst F1
                               (make-bst D1 (make-bst C1 false false)
                                         (make-bst E1 false false))
                               false))))
 false)

(define (can-find? widgets db)
  (andmap (lambda (widget)
            (not (false? (find (widget-name widget) db))))
          widgets))





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