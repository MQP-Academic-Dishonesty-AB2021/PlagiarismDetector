

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname A5P1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define B1 (make-widget "B1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))







(define (build-tree low)
  (foldr insert-name false low))




(define BSTZ (make-bst Z1 false false))
(define BSTW (make-bst W1 false BSTZ))
(define BSTB (make-bst B1 false false))
(define BSTD (make-bst D1 BSTB BSTW))








(check-expect (smaller? "Z1" BSTD) false) 
(check-expect (smaller? "A1" BSTD) true) 
(check-expect (smaller? "A2" false) false) 


(define (smaller? key bst)
  (cond
    [(false? bst) false]
    [(string<? key (widget-name (bst-widget bst))) true]
    [else false]))






(check-expect (same? "D1" BSTD) true) 
(check-expect (same? "Y1" BSTB) false) 
(check-expect (same? "E" false) false) 


(define (same? key bst)
  (cond
    [(false? bst) false]
    [(string=? key (widget-name (bst-widget bst))) true]
    [else false]))






(check-expect (find-name "Z1" BSTD) Z1) 
(check-expect (find-name "B1" BSTD) B1) 
(check-expect (find-name "WeHeartBeck" BSTD) false) 

(define (find-name key bst)
  (cond
    [(false? bst) false] 
    [(string=? key (widget-name (bst-widget bst))) (bst-widget bst)] 
    [(string<? key (widget-name (bst-widget bst))) (find-name key (bst-left bst))] 
    [(string>? key (widget-name (bst-widget bst))) (find-name key (bst-right bst))] 
    [else false])) 





(define yurt (make-widget "YURT" 1 1))
(define BSTY-check (make-bst yurt false false))
(define BSTZ-check (make-bst Z1 BSTY-check false))
(define BSTW-check (make-bst W1 false BSTZ-check))
(define BSTB-check (make-bst B1 false false))
(define BSTD-yurt-check (make-bst D1 BSTB-check BSTW-check)) 
                                                             

(define aye (make-widget "AYE" 3 4))
(define BSTZ-aye-check (make-bst Z1 false false))
(define BSTA-aye-check (make-bst aye false false))
(define BSTW-aye-check (make-bst W1 false BSTZ-aye-check))
(define BSTB-aye-check (make-bst B1 BSTA-aye-check false))
(define BSTD-aye-check (make-bst D1 BSTB-aye-check BSTW-aye-check))

(check-expect (insert-name aye BSTD) BSTD-aye-check) 
(check-expect (insert-name yurt BSTD) BSTD-yurt-check) 
(check-expect (insert-name Z1 BSTD)  BSTD) 

(define (insert-name widget bst)
  (cond 
    [(false? bst)
     (make-bst widget
               false
               false)]
    [(smaller? (widget-name widget) bst)
     (make-bst (bst-widget bst)
               (insert-name widget (bst-left bst))
               (bst-right bst))]
    [(same? (widget-name widget) bst)
     bst] 
    [(not (smaller? (widget-name widget) bst))
     (make-bst (bst-widget bst)
               (bst-left bst)
               (insert-name widget (bst-right bst)))]))









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