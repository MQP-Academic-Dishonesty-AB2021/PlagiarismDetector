

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))






(define (build-tree low)
  (foldr insert-name false low))








(check-expect (smaller? "W2" (make-bst W1 false false)) false)
(check-expect (smaller? "A0" (make-bst W1 false false)) true)
(check-expect (smaller? "W1" (make-bst W1 false false)) false)
(check-expect (smaller? "D1" (make-bst W1 A1    Z1))    true)



(define (smaller? key b)
  (string<? key (widget-name (bst-widget b))))





(check-expect (same? "W2" (make-bst W1 false false)) false)
(check-expect (same? "A1" (make-bst A1 false false)) true)
(check-expect (same? "W1" (make-bst W1 false false)) true)
(check-expect (same? "D1" (make-bst
                           W1
                           (make-bst A1 false false)
                           (make-bst Z1 false false)))
              false)


(define (same? key b)
  (string=? key (widget-name (bst-widget b))))





(check-expect (find-name "X2" (make-bst D1 false false)) false)
(check-expect (find-name "D1" (make-bst D1 false false)) D1)
(check-expect (find-name "X2" false) false)

(check-expect (find-name "D1" (make-bst
                               W1
                               (make-bst
                                A1
                                false
                                (make-bst D1 false false))
                               (make-bst Z1 false false)))
              D1)

(check-expect (find-name "X2"
                         (make-bst
                          W1
                          (make-bst
                           A1
                           false
                           (make-bst D1 false false))
                          (make-bst Z1 false false)))
              false)


(define (find-name key b)
  (cond
    [(false? b) false]
    [(same? key b) (bst-widget b)]
    [(smaller? key b) (find-name key (bst-left b))]
    [else (find-name key (bst-right b))]))




(check-expect (insert-name A1 (make-bst W1 false false))
              (make-bst
               W1
               (make-bst A1 false false)
               false))

(check-expect (insert-name D1 false) (make-bst D1 false false))

(check-expect (insert-name D1 (make-bst W1
                                        (make-bst A1 false false)
                                        (make-bst Z1 false false)))
              (make-bst W1
                        (make-bst A1
                                  false
                                  (make-bst D1 false false))
                        (make-bst Z1 false false)))



(define (insert-name w b)
  (cond
    [(false? b) (make-bst w false false)]
    [(smaller? (widget-name w) b)
     (make-bst
      (bst-widget b)
      (insert-name w (bst-left b))
      (bst-right b))]
    [else
     (make-bst
      (bst-widget b)
      (bst-left b)
      (insert-name w (bst-right b)))]))










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