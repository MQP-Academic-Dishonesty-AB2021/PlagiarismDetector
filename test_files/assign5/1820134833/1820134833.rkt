

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Kai Nakamura and Aashi Mehta Assignment 5 Part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))



(define BST-W1 (make-bst W1 false false))
(define BST-Z1 (make-bst Z1 false false))
(define BST-A1 (make-bst A1 false false))
(define BST-D1 (make-bst D1 false false))
 
















(define-struct db (field lt? eq? bst))

(define DB-name (make-db widget-name string<? string=? false))
(define DB-quantity (make-db widget-quantity < = false))
(define DB-price (make-db widget-price < = false))












(check-expect (find 1 DB-quantity) false)


(check-expect (find 5 (make-db widget-quantity < = BST-D1)) D1)


(check-expect (find 51 (make-db widget-quantity < =
                                (make-bst A1 false BST-Z1))) Z1)


(check-expect (find 1 (make-db widget-quantity < =
                               (make-bst A1 BST-W1 false))) W1)


(check-expect (find 1 DB-price) false)


(check-expect (find 5 (make-db widget-price < = BST-D1)) D1)


(check-expect (find 16 (make-db widget-price < =
                                (make-bst A1 false BST-Z1))) Z1)


(check-expect (find 1 (make-db widget-price < =
                               (make-bst A1 BST-W1 false))) W1)


(check-expect (find "W1" DB-name) false)


(check-expect (find "D1" (make-db widget-name string<? string=? BST-D1)) D1)


(check-expect (find "W1" (make-db widget-name string<? string=?
                                  (make-bst D1 false BST-W1))) W1)


(check-expect (find "A1" (make-db widget-name string<? string=?
                                  (make-bst D1 BST-A1 false))) A1)

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








(check-expect (db-bst (insert W1 DB-name)) BST-W1)


(check-expect (db-bst
               (insert A1 (make-db widget-name string<? string=? BST-D1)))
              (make-bst D1 BST-A1 false))


(check-expect (db-bst
               (insert Z1 (make-db widget-name string<? string=? BST-D1)))
              (make-bst D1 false BST-Z1))


(check-expect (render (db-bst
               (insert W1 (make-db widget-name string<? string=?
                                   (make-bst A1 false
                                             (make-bst D1 false BST-Z1))))))
              (render (make-bst A1 false
                        (make-bst D1 false
                                  (make-bst Z1 BST-W1 false)))))


(check-expect (db-bst
               (insert W1 (make-db widget-price < =
                                   (make-bst A1 false BST-D1))))
              (make-bst A1 BST-W1 BST-D1))


(check-expect (db-bst
               (insert A1 (make-db widget-quantity < =
                                   (make-bst W1 false
                                             (make-bst D1 false BST-Z1)))))
              (make-bst W1 false (make-bst D1 BST-A1 BST-Z1)))

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