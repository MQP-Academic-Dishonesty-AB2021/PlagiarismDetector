

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define C1 (make-widget "C1" 2 4))

(define-struct bst (widget left right))




(define TREE1 (make-bst W1 
                        (make-bst D1 (make-bst A1 false false) false) (make-bst Z1 false false)))
(define TREE2 (make-bst Z1 
                         (make-bst A1 false (make-bst D1 false false)) false))
(define TREE3 (make-bst A1 
                        false (make-bst D1 false (make-bst W1 false (make-bst Z1 false false)))))



(define (build-tree low)
  (foldr insert-name false low))


(check-expect (smaller? "" TREE3) true)

(check-expect (smaller? "B1" TREE1) true)

(check-expect (smaller? "Z1" TREE2) false)




(define (smaller? key tree) 
  (string<? key (widget-name (bst-widget tree))))


(check-expect (same? "W1" TREE1) true)

(check-expect (same? "Z1" TREE1) false)



(define (same? key tree) 
  (string=? key (widget-name (bst-widget tree))))
  

(check-expect (find-name "W1" TREE1) W1)

(check-expect (find-name "W1" TREE3) W1)

(check-expect (find-name "W1" TREE2) false)




(define (find-name key tree)
  (cond
    [(same? key tree) (bst-widget tree)]
    [(smaller? key tree) (if 
                          (false? (bst-left tree)) false 
                          (find-name key (bst-left tree)))]
    [else (if 
           (false? (bst-right tree)) false 
           (find-name key (bst-right tree)))]))


(check-expect (insert-name W1 TREE2) 
                      (make-bst Z1 
                        (make-bst A1 false (make-bst D1 false (make-bst W1 false false))) false))

(check-expect (insert-name C1 TREE2) 
                      (make-bst Z1 
                        (make-bst A1 false (make-bst D1 (make-bst C1 false false) false)) false))


(define (insert-name widget bst)
  (cond
    [(false? bst) (make-bst widget false false)]
    [(smaller? (widget-name widget) bst)
     (make-bst
      (bst-widget bst)
      (insert-name widget (bst-left bst))
      (bst-right bst))]
    [(not (smaller? (widget-name widget) bst))
     (make-bst
      (bst-widget bst)
      (bst-left bst)
      (insert-name widget (bst-right bst)))]
    [(same? (widget-name widget) bst) bst]))







(define (random-widgets num max)
  (build-list num
              (lambda(dummy)
                (make-widget 
                 (number->string (random max))
                 (random max)
                 (random max)))))





(define (random-widgets-string num slen nmax)
  (local
    [(define (random-string len)
       (list->string (build-list len (lambda(dummy)
                                       (integer->char (+ 97 (random 26)))))))]
    (build-list num
                (lambda(dummy)
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
       (list->string (build-list n (lambda(x) #\ ))))
     
     
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