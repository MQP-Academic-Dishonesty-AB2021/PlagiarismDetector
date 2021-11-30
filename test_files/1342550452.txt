

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 5 Pt 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)

(define-struct widget (name quantity price))




(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))



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
         





(define (build-tree low)
  (foldr insert false low))





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