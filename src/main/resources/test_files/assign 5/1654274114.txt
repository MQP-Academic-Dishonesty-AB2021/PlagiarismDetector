

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define-struct widget (name quantity price))




(define-struct bst (widget left right))




(define-struct db (field lt? eq? bst))


(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
[define F1 [make-widget "F1" 7 32]]

[define BST1 [make-bst A1 false false]]
[define BST2 [make-bst W1
                       [make-bst D1
                                 [make-bst A1 false false] false]
                       [make-bst Z1 false false]]]
[define BST3 [make-bst A1
                       [make-bst W1 false false]
                       [make-bst Z1 [make-bst D1 false false] false]]]
(define BST4 (make-bst D1
                       (make-bst Z1 (make-bst F1 false false) false)
                       (make-bst A1 false (make-bst W1 false false))))

(define DB-quantity (make-db widget-quantity < = BST3))
(define DB-name (make-db widget-name string<? string=? BST2))
(define DB-name-empty (make-db widget-name string<? string=? false))
(define DB-price (make-db widget-price > = BST4))






[check-expect [find "E1" DB-name] false]

[check-expect [find "W1" DB-name] W1]
[check-expect [find "A1" DB-name] A1]
[check-expect [find "Z1" DB-name] Z1]
[check-expect [find 1 DB-quantity] W1]
[check-expect [find 51 DB-quantity] Z1]
[check-expect [find 5 DB-quantity] D1]
[check-expect [find 16 DB-price] Z1]
[check-expect [find 1 DB-price] W1]



(define (find key db)
  (local [(define (find-inner bst)
            (cond [(false? bst) false]
                  [((db-eq? db) key ((db-field db) (bst-widget bst)))
                   (bst-widget bst)]
                  [else (if ((db-lt? db) key ((db-field db)
                                              (bst-widget bst)))
                            (find-inner (bst-left bst))
                            (find-inner (bst-right bst)))]))]
    (find-inner (db-bst db))))



[check-expect [db-bst [insert A1 DB-name-empty]]
              [make-bst A1 false false]]

[check-expect [db-bst [insert F1 DB-name]]
              [make-bst W1
                        [make-bst D1
                                  [make-bst A1 false false]
                                  [make-bst F1 false false]]
                        [make-bst Z1 false false]]]
[check-expect [db-bst [insert F1 DB-quantity]]
              [make-bst A1
                        [make-bst W1 false false]
                        [make-bst Z1
                                  [make-bst D1 false
                                            [make-bst F1 false false]]
                                  false]]]





(define (insert wid db)
  (local [(define (insert-inner key bst)
            (cond [(false? bst) (make-bst key false false)]
                  [((db-lt? db) [[db-field db] key] ((db-field db)
                                                     (bst-widget bst))) 
                   (make-bst (bst-widget bst)
                             (insert-inner key (bst-left bst))
                             (bst-right bst))]
                  [else
                   (make-bst (bst-widget bst)
                             (bst-left bst)
                             (insert-inner key (bst-right bst)))]))]
    (make-db (db-field db) (db-lt? db) (db-eq? db)
             (insert-inner wid (db-bst db)))))











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
                                       (integer->char
                                        (+ 97 (random 26)))))))]
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