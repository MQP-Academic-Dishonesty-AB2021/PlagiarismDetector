

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)

(define-struct widget (name quantity price))

(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 25 25))

 


(define-struct bst (widget left right))



(define BSTA1 (make-bst A1 false false))
(define BSTW1 (make-bst W1 false false))
(define BSTZ1 (make-bst Z1 BSTW1 false))
(define BSTD1 (make-bst D1 BSTA1 BSTZ1))




(define BSTA2 (make-bst A1 false false))
(define BSTW2 (make-bst W1 false BSTA2))
(define BSTZ2 (make-bst Z1 false false))
(define BSTD2 (make-bst D1 BSTW2 BSTZ2))




(define BSTA3 (make-bst A1 false false))
(define BSTW3 (make-bst W1 BSTA3 false))
(define BSTZ3 (make-bst Z1 false false))
(define BSTD3 (make-bst D1 BSTZ3 BSTW3))




(define BSTA4 (make-bst A1 false false))
(define BSTZ4 (make-bst Z1 false false))
(define BSTW4 (make-bst W1 BSTZ4 false))
(define BSTD4 (make-bst D1 BSTW4 BSTA4))




 


(define-struct db (field lt? eq? bst))









(define DB-quantity (make-db widget-quantity < = false))
(define DB-name (make-db widget-name string<? string=? false))
(define DB1 (make-db widget-name string<? string=? BSTD1))
(define DB2 (make-db widget-quantity < = BSTD2))
(define DB3 (make-db widget-price > = BSTD3))
(define DB4 (make-db widget-name string>? string=? BSTD4))






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




(check-expect (find "W3" DB-name) false)
(check-expect (find "D1" DB4) D1)
(check-expect (find 3 DB3) A1)
(check-expect (find 7 DB2) false)



(define (find key db)
  (cond [(false? (db-bst db)) false]
        [((db-eq? db) key ((db-field db) (bst-widget (db-bst db))))
         (bst-widget (db-bst db))]
        [((db-lt? db) key ((db-field db) (bst-widget (db-bst db))))
         (find key (make-db (db-field db) (db-lt? db) (db-eq? db)
                            (bst-left (db-bst db))))]
        [else
         (find key (make-db (db-field db) (db-lt? db) (db-eq? db)
                            (bst-right (db-bst db))))]))






(check-expect (db-bst (insert C1 DB1))
              (make-bst D1
                        (make-bst A1
                                  false
                                  (make-bst C1
                                            false
                                            false))
                        BSTZ1))

(check-expect (db-bst (insert C1 DB2))
              (make-bst D1
                        BSTW2
                        (make-bst Z1
                                  (make-bst C1
                                            false
                                            false)
                                  false)))

(check-expect (db-bst (insert C1 DB3))
              (make-bst D1
                        (make-bst Z1
                                  (make-bst C1
                                            false
                                            false)
                                  false)
                        BSTW3))

(check-expect (db-bst (insert C1 DB4))
              (make-bst D1
                        BSTW4
                        (make-bst A1
                                  (make-bst C1
                                            false
                                            false)
                                  false)))



(define (insert widget db)
  (local [(define (insert2 bst)
            (cond [(false? bst) (make-bst widget false false)]
                  [(or ((db-eq? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst)))
                       ((db-lt? db) ((db-field db) widget)
                                    ((db-field db) (bst-widget bst))))
                   (make-bst (bst-widget bst)
                             (insert2 (bst-left bst))
                             (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst)
                             (bst-left bst)
                             (insert2 (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert2 (db-bst db)))))