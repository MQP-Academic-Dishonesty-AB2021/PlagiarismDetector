

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter pt1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))




(define TW1 (make-bst W1 #f #f))
(define TZ1 (make-bst Z1 #f #f))
(define TA1 (make-bst A1 #f #f))
(define TD1 (make-bst D1 #f #f))




(define T0 (make-bst W1 TA1 TZ1))
(define T1 (make-bst A1 #f TW1))
(define T2 (make-bst W1 #f TW1))
(define T3 (make-bst D1 TA1 #f))
(define T4 (make-bst W1
                     (make-bst A1
                               #f
                               TD1)
                     TZ1))



(define (build-tree low)
  (foldr insert-name false low))




(define (smaller? k b)
  (cond
    [(false? b) false]
    [else
     (if (string<? k (widget-name (bst-widget b)))
       true
       false)]))

(check-expect (smaller? "W1" TZ1) true)
(check-expect (smaller? "Z1" TA1) false)
(check-expect (smaller? "A1" TD1) true)





(define (same? k b)
  (cond [(false? b) false]
        [else
         (string=? k (widget-name (bst-widget b)))]))

(check-expect (same? "W1" TW1) #t)

(check-expect (same? "" TW1) #f)

(check-expect (same? "Abc" #f) #f)

(check-expect (same? "W2" TW1) #f)

(check-expect (same? "Z1" TZ1) #t)





(define (find-name k b)
  (cond [(false? b) false]
        [(string=? k (widget-name (bst-widget b)))
         (bst-widget b)]
        [(string<? k (widget-name (bst-widget b)))
         (find-name k (bst-left b))]
        [(not (string<? k (widget-name (bst-widget b))))
         (find-name k (bst-right b))]
        [else false]))
  
(check-expect (find-name "W1" TW1) W1)
(check-expect (find-name "Z1" TZ1) Z1)
(check-expect (find-name "A1" T0) A1)
(check-expect (find-name "D1" T0) false)
(check-expect (find-name "A1" T3) A1)




(define (insert-name v b)
  (cond [(eq? b #f) (make-bst v #f #f)]
        [(smaller? (widget-name v) b)
         (make-bst
          (bst-widget b)
          (insert-name v (bst-left b))
          (bst-right b))]
        [else
         (make-bst
          (bst-widget b)
          (bst-left b)
          (insert-name v (bst-right b)))]))


(check-expect (insert-name W1 #f) TW1)

(check-expect (insert-name W1 TA1) T1)

(check-expect (insert-name W1 TW1) T2)

(check-expect (insert-name A1 TD1) T3)

(check-expect (insert-name D1 T0) T4)








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