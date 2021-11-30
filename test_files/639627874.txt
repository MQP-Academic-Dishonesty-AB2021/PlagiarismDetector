

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment5_P1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))








(define BST1 (make-bst W1 false false))
(define BST2 (make-bst W1
                       false
                       (make-bst Z1 false false)))
(define BST3 (make-bst D1
                       false
                       (make-bst W1 false false)))
(define BST4 (make-bst D1
                       (make-bst A1 false false)
                       BST2))

(define (build-tree low)
  (foldr insert-name false low))





(check-expect (smaller? "" BST1) true)

(check-expect (smaller? (widget-name W1) BST1) false)

(check-expect (smaller? (widget-name D1) BST1) true)

(check-expect (smaller? (widget-name A1) BST3) true)


  

(define (smaller? name bst)
  (string<? name (widget-name (bst-widget bst))))







(check-expect (same? "" BST1) false)

(check-expect (same? (widget-name W1) BST1) true)

(check-expect (same? (widget-name D1) BST1) false)

(check-expect (same? (widget-name A1) BST3) false)

  

(define (same? name bst) 
  (string=? name (widget-name (bst-widget bst))))






(check-expect (find-name "" BST1) false)

(check-expect (find-name (widget-name W1) BST1) W1)

(check-expect (find-name (widget-name D1) BST1) false)

(check-expect (find-name (widget-name A1) BST3) false)

(check-expect (find-name (widget-name W1) BST4) W1)

  

(define (find-name name bst)
  (cond
    [(false? bst) false]
    [(same? name bst) (bst-widget bst)]
    [(smaller? name bst) (find-name name (bst-left bst))]
    [else (find-name name (bst-right bst))]))





(check-expect (insert-name A1 BST1)
              (make-bst W1
                        (make-bst A1 false false)
                        false))

(check-expect (insert-name W1 BST2)
              (make-bst W1
                        false
                        (make-bst Z1
                                  (make-bst W1 false false)
                                  false)))

(check-expect (insert-name Z1 BST3)
              (make-bst D1
                        false
                        (make-bst W1 false
                                  (make-bst Z1 false false))))

  

(define (insert-name widget bst)
  (cond [(false? bst) (make-bst widget false false)]
        
        [(smaller? (widget-name widget) bst)
         (make-bst
          (bst-widget bst)
          (insert-name widget (bst-left bst))
          (bst-right bst))]
        
        [else
         (make-bst
          (bst-widget bst)
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