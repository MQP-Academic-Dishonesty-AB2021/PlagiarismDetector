

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |5 pt2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct widget (name quantity price))




(define-struct bst (widget left right))







(define-struct db (field lt? eq? bst))






(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 12 5))

(define B-Z1 (make-bst Z1 false false))
(define B-A1 (make-bst A1 false false))
(define B-W1 (make-bst W1 false B-Z1))
(define B-D1 (make-bst D1 B-A1 B-W1))

(define Q-C1 (make-bst C1 false false))
(define Q-Z1 (make-bst Z1 Q-C1 false))
(define Q-W1 (make-bst W1 false false))
(define Q-A1 (make-bst A1 Q-W1 false))
(define Q-D1 (make-bst D1 Q-A1 Q-Z1))


(define (DB-quantity bst) (make-db widget-quantity < = bst))
(define (DB-name bst) (make-db widget-name string<? string=? bst))






(define (comp? fn? db value bst)
  (fn? value ((db-field db) (bst-widget bst))))

(check-expect (comp? (db-lt? (DB-name B-Z1)) (DB-name B-Z1) "A1" B-Z1) #t)
(check-expect (comp? (db-lt? (DB-name B-Z1)) (DB-name B-A1) "Z1" B-A1) #f)
(check-expect (comp? (db-eq? (DB-name B-Z1)) (DB-name B-Z1) "Z1" B-Z1) #t)
(check-expect (comp? (db-eq? (DB-name B-Z1)) (DB-name B-A1) "Z1" B-A1) #f)
(check-expect (comp? (db-lt? (DB-quantity Q-Z1)) (DB-quantity Q-Z1) 20 Q-Z1) #t)
(check-expect (comp? (db-lt? (DB-quantity Q-Z1)) (DB-quantity Q-A1) 50 Q-A1) #f)
(check-expect (comp? (db-eq? (DB-quantity Q-Z1)) (DB-quantity Q-Z1) 51 Q-Z1) #t)
(check-expect (comp? (db-eq? (DB-quantity Q-Z1)) (DB-quantity Q-A1) 51 Q-A1) #f)





(define (find db value)
  (local {(define (find bst) (cond
                               [(false? bst) false]
                               [(comp? (db-eq? db) db value bst) (bst-widget bst)]
                               [(comp? (db-lt? db) db value bst) (find (bst-left bst))]
                               [else (find (bst-right bst))]))}
    (find (db-bst db))))

(check-expect (find (DB-name B-Z1) "Z1") Z1)
(check-expect (find (DB-name B-Z1) "q3rt") #f)
(check-expect (find (DB-name B-D1) "Z1") Z1)
(check-expect (find (DB-quantity Q-Z1) 51) Z1)
(check-expect (find (DB-quantity Q-Z1) 50) #f)
(check-expect (find (DB-quantity Q-D1) 12) C1)







(define (insert db wid)
  (local {(define (insert bst) (cond
                                 [(false? bst) (make-bst wid false false)]
                                 [(comp? (db-lt? db) db ((db-field db) wid) bst) (make-bst (bst-widget bst)
                                                                             (insert (bst-left bst))
                                                                             (bst-right bst))]          
                                 [else (make-bst (bst-widget bst)
                                                 (bst-left bst)
                                                 (insert (bst-right bst)))]))}
    (make-db (db-field db) (db-lt? db) (db-eq? db) (insert (db-bst db)))))
  
(check-expect (db-bst (insert (DB-name B-A1) Z1)) (make-bst A1 false (make-bst Z1 false false)))
(check-expect (db-bst (insert (DB-name B-Z1) A1)) (make-bst Z1 (make-bst A1 false false) false))
(check-expect (db-bst (insert (DB-name B-D1) C1)) (make-bst (make-widget "D1" 5 5)
                                              (make-bst A1 #f (make-bst C1 #f #f))
                                              (make-bst W1 #f (make-bst Z1 #f #f))))









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

(render B-D1)
(render Q-D1)