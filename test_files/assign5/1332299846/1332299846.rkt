

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(define-struct bst (widget left right))




(define-struct widget (name quantity price))



(define-struct db (field lt? eq? bst))




(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define B1 (make-widget "B1" 3 5))
(define D1 (make-widget "D1" 5 7))





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



(check-expect (db-bst (insert! A1
                               (make-db widget-name string<? string=? false)))
              (make-bst A1 false false))
(check-expect (db-bst (insert! A1
                               (make-db widget-name string<? string=?
                                        (make-bst B1 false false))))
              (make-bst B1
                        (make-bst A1 false false)
                        false))
(check-expect (db-bst (insert! D1
                               (make-db widget-price > = (make-bst B1 false false))))
              (make-bst B1
                        (make-bst D1 false false)
                        false))
(check-expect (db-bst (insert! W1
                               (make-db widget-quantity < =
                                        (make-bst B1
                                                  (make-bst A1 false false)
                                                  (make-bst D1 false false)))))
              (make-bst B1
                        (make-bst A1
                                  (make-bst W1 false false)
                                  false)
                        (make-bst D1
                                  false
                                  false)))
(define (insert! widget db)
  (local [(define (smaller? widget bst)
            ((db-lt? db)
             ((db-field db) widget)
             ((db-field db) (bst-widget bst))))
          (define (insert-bst bst)
            (cond [(smaller? widget bst)
                   (if (false? (bst-left bst))
                       (set-bst-left! bst (make-bst widget false false))
                       (insert-bst (bst-left bst)))]
                  [else
                   (if (false? (bst-right bst))
                       (set-bst-right! bst (make-bst widget false false))
                       (insert-bst (bst-right bst)))]))]
    (begin (if (false? (db-bst db))
               (set-db-bst! db (make-bst widget false false))
               (insert-bst (db-bst db))) db)))









(check-expect (empty-bst W1) (make-bst W1 false false))
(check-expect (empty-bst A1) (make-bst A1 false false))

(define (empty-bst widget)
  (make-bst widget false false))





(define price-lt-db (make-db widget-price < = false))
(define quantity-gt-db (make-db widget-quantity > = false))
(define name-alph-db (make-db widget-name string<? string=? false))



(check-expect (balance price-lt-db false) false)

(check-expect (balance name-alph-db (make-bst A1 false false))
              (make-bst A1 false false))

(check-expect (balance quantity-gt-db (make-bst A1 false false))
              (make-bst A1 false false))
(check-expect (balance price-lt-db
                       (make-bst B1
                                 (empty-bst A1)
                                 false))
              (make-bst B1
                        (empty-bst A1)
                        false))
(check-expect (balance price-lt-db
                       (make-bst B1
                                 (empty-bst A1)
                                 (empty-bst D1)))
              (make-bst B1
                        (empty-bst A1)
                        (empty-bst D1)))

(check-expect (balance price-lt-db
                       (make-bst B1
                                 (make-bst A1
                                           (empty-bst W1)
                                           false)
                                 (empty-bst D1)))
              (make-bst B1
                        (make-bst A1
                                  (empty-bst W1)
                                  false)
                        (empty-bst D1)))



(check-expect (balance price-lt-db
                       (make-bst D1
                                 (make-bst B1
                                           (empty-bst A1)
                                           false)
                                 false))
              (make-bst B1
                        (empty-bst A1)
                        (empty-bst D1)))

(check-expect (render (balance price-lt-db
                               (make-bst D1
                                         (make-bst A1
                                                   false
                                                   (empty-bst B1))
                                         false)))
              (render (make-bst B1
                                (empty-bst A1)
                                (empty-bst D1))))

(check-expect (render (balance quantity-gt-db
                               (make-bst Z1
                                         false
                                         (make-bst D1
                                                   false
                                                   (empty-bst B1)))))
              (render (make-bst D1
                                (empty-bst Z1)
                                (empty-bst B1))))


(check-expect (render (balance name-alph-db
                               (make-bst A1
                                         false
                                         (make-bst D1
                                                   (empty-bst B1)
                                                   false))))
              (render (make-bst B1
                                (empty-bst A1)
                                (empty-bst D1))))


(define (balance db bst)
  (local[(define depthdiff (height-diff bst))
         (define (smaller? widget bst)
           ((db-lt? db)
            ((db-field db) widget)
            ((db-field db) (bst-widget bst))))
         (define (check-bases bst)
           (cond [(false? bst) false]
                 [(>= 1 (abs depthdiff)) bst]
                 [else
                  (if (< 0 depthdiff)
                      (fix-left bst)
                      (fix-right bst))]))
         (define (fix-left bst)
           (if (< 0 (height-diff (bst-left bst)))
               
               (right-rotate bst)
               
               
               (right-rotate
                (make-bst (bst-widget bst)
                          (left-rotate (bst-left bst))
                          (bst-right bst)))))
         (define (fix-right bst)
           (if (> 0 (height-diff (bst-right bst)))
               
               (left-rotate bst)
               
               
               (left-rotate
                (make-bst (bst-widget bst)
                          (bst-left bst)
                          (right-rotate (bst-right bst))))))
         
         
         
         
         
         
         (define (right-rotate bst)
           (make-bst (bst-widget (bst-left bst))
                     (bst-left (bst-left bst))
                     (make-bst (bst-widget bst)
                               (bst-right (bst-left bst))
                               (bst-right bst))))
         
         
         
         
         
         
         (define (left-rotate bst)
           (make-bst (bst-widget (bst-right bst))
                     (make-bst (bst-widget bst)
                               (bst-left bst)
                               (bst-left (bst-right bst)))
                     (bst-right (bst-right bst))))]
    (check-bases bst)))




(check-expect (num-widget 17) (make-widget "17" 17 17))
(check-expect (num-widget 2) (make-widget "2" 2 2))

(define (num-widget num)
  (make-widget (number->string num) num num))





(check-expect (db-bst (insert-avl W1 (make-db widget-name string<? string=? false)))
              (make-bst W1 false false))


(check-expect (db-bst (insert-avl A1 (make-db widget-name string<? string=?
                                              (empty-bst W1))))
              (make-bst W1 (empty-bst A1) false))


(check-expect (db-bst (insert-avl A1 (make-db widget-name string<? string=?
                                              (make-bst W1 (empty-bst D1) false))))
              (make-bst D1 (empty-bst A1) (empty-bst W1)))


(define W2 (num-widget 2))
(define W5 (num-widget 5))
(define W8 (num-widget 8))
(define W9 (num-widget 9))
(define W17 (num-widget 17))
(define W19 (num-widget 19))
(define W20 (num-widget 20))
(define W23 (num-widget 23))
(define W27 (num-widget 27))
(define W32 (num-widget 32))


(define W10 (num-widget 10))
(define W24 (num-widget 24))


(define COMP-TREE1
  (make-bst W17
            (make-bst W5
                      (empty-bst W2)
                      (make-bst W8
                                false
                                (empty-bst W9)))
            (make-bst W20
                      (empty-bst W19)
                      (make-bst W27
                                (empty-bst W23)
                                (empty-bst W32)))))

(define COMP-TREE1-W10
  (make-bst W17
            (make-bst W5
                      (empty-bst W2)
                      (make-bst W9
                                (empty-bst W8)
                                (empty-bst W10)))
            (make-bst W20
                      (empty-bst W19)
                      (make-bst W27
                                (empty-bst W23)
                                (empty-bst W32)))))

(define COMP-TREE1-W24
  (make-bst W17
            (make-bst W5
                      (empty-bst W2)
                      (make-bst W8
                                false
                                (empty-bst W9)))
            (make-bst W23
                      (make-bst W20
                                (empty-bst W19)
                                false)
                      (make-bst W27
                                (empty-bst W24)
                                (empty-bst W32)))))

(check-expect (db-bst (insert-avl W10
                                  (make-db widget-price < = COMP-TREE1)))
              COMP-TREE1-W10)
(check-expect (db-bst (insert-avl W24
                                  (make-db widget-price < = COMP-TREE1)))
              COMP-TREE1-W24)

(define (insert-avl widget db)
  (local
    [(define (smaller? widget bst)
       ((db-lt? db)
        ((db-field db) widget)
        ((db-field db) (bst-widget bst))))
     (define (insert-bst bst)
       (cond [(false? bst) (make-bst widget false false)]
             [(smaller? widget bst)
              (balance db
                       (make-bst (bst-widget bst)
                                 (balance db (insert-bst (bst-left bst)))
                                 (bst-right bst)))]
             [else
              (balance db
                       (make-bst (bst-widget bst)
                                 (bst-left bst)
                                 (balance db (insert-bst (bst-right bst)))))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (insert-bst (db-bst db)))))





(define (insert widget db)
  (local [(define (smaller? widget bst)
            ((db-lt? db)
             ((db-field db) widget)
             ((db-field db) (bst-widget bst))))
          (define (insert-bst bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [(smaller? widget bst)
                   (make-bst (bst-widget bst)
                             (insert-bst (bst-left bst))
                             (bst-right bst))]
                  [else (make-bst (bst-widget bst)
                                  (bst-left bst)
                                  (insert-bst (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (insert-bst (db-bst db)))))

(define (time-insert)
  (local [(define listofW1 (random-widgets 250000 2000000))]
    (begin (display "insert:")
           (time (foldl
                  (λ (widget db) (insert widget db))
                  (make-db widget-price < = false)
                  listofW1))
           (display "insert!:")
           (time (foldl
                  (λ (widget db) (insert! widget db))
                  (make-db widget-price < = false)
                  listofW1))
           ""
           ))
  )

(time-insert)
(time-insert)
(time-insert)
(time-insert)
(time-insert)



(define (find key db)
  (local [(define (smaller? key bst)
            ((db-lt? db) key ((db-field db) (bst-widget bst))))
          (define (same? key bst)
            ((db-eq? db) key ((db-field db) (bst-widget bst))))
          (define (find-bst bst)
            (cond [(false? bst) false]
                  [(same? key bst) (bst-widget bst)]
                  [else
                   (if (smaller? key bst)
                       (find-bst (bst-left bst))
                       (find-bst (bst-right bst)))])
            )]
    (find-bst (db-bst db)))
  )

(define (time-find)
  (local[(define listofW2 (random-widgets 10000 2000000))
         (define bst1db (foldl (λ (widget db)
                                 (insert! widget db))
                               (make-db widget-price < = false)
                               listofW2))
         (define avl1db (foldl (λ (widget db)
                                 (insert-avl widget db))
                               (make-db widget-price < = false)
                               listofW2))]
    (begin (display "find bst:")
           (time (map (λ (widget) (find (widget-price widget) bst1db)) listofW2))
           (display "find avl:")
           (time (map (λ (widget) (find (widget-price widget) avl1db)) listofW2))
           ""
           )))
(time-find)
(time-find)
(time-find)
(time-find)
(time-find)
(time-find)
(time-find)
(time-find)
(time-find)
(time-find)