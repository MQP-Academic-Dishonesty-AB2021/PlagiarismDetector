

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Kai Nakamura and Aashi Mehta Assignment 5 Part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








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
 












(define (build-tree low)
  (foldr insert-name false low))






(check-expect (smaller? "A1" false) false)


(check-expect (smaller? "A1" (make-bst D1 false false)) true)


(check-expect (smaller? "E1" (make-bst D1 false false)) false)


(check-expect (smaller? "D1" (make-bst D1 false false)) false)

(define (smaller? k b)
  (cond [(false? b) false]
        [else
         (string<? k (widget-name (bst-widget b)))]))





(check-expect (same? "A1" false) false)


(check-expect (same? "A1" (make-bst A1 false false)) true)


(check-expect (same? "A1" (make-bst D1 false false)) false)

(define (same? k b)
  (cond [(false? b) false]
        [else
         (string=?  k (widget-name (bst-widget b)))]))






(check-expect (find-name "A1" false) false)


(check-expect (find-name "D1" (make-bst A1 false false)) false)


(check-expect (find-name "A1" (make-bst A1 false false)) A1)


(check-expect (find-name "Z1" (make-bst A1 false
                                        (make-bst D1 false
                                                  (make-bst Z1 false
                                                            false)))) Z1)

(check-expect (find-name "A1" (make-bst Z1
                                        (make-bst D1
                                                  (make-bst A1 false false)
                                                  false) false)) A1)

(define (find-name k b)
  (cond [(false? b) false]
        [(same? k b) (bst-widget b)]
        [(smaller? k b)
         (find-name k (bst-left b))]
        [else
         (find-name k (bst-right b))]))





(check-expect (insert-name D1 false) BST-D1)

(check-expect (insert-name A1 BST-D1)
              (make-bst D1 BST-A1 false))

(check-expect (insert-name Z1 (make-bst A1 false BST-D1))
              (make-bst A1 false (make-bst D1 false BST-Z1)))

(check-expect (insert-name W1 (make-bst A1 false
                                        (make-bst D1 false BST-Z1)))
              (make-bst A1 false
                        (make-bst D1 false (make-bst Z1 BST-W1 false))))

(define (insert-name v b)
  (cond [(false? b)
         (make-bst v false false)]
        [(smaller? (widget-name v) b)
         (make-bst (bst-widget b)
                   (insert-name v (bst-left b))
                   (bst-right b))]
        [else
         (make-bst (bst-widget b)
                   (bst-left b)
                   (insert-name v (bst-right b)))]))







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