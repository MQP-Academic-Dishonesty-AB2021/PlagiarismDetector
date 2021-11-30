

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part1_DanielB_BenjaminS final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct widget (name quantity price))





(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))

(define-struct bst (widget left right))






(define (build-tree low)
  (foldr insert-name false low))



(define (smaller? key bst) 
  (string<? (widget-name key) (widget-name (bst-widget bst))
  ))
(check-expect (smaller? A1 (make-bst D1 false false)) true) 
(check-expect (smaller? A1 (make-bst A1 false false)) false) 
(check-expect (smaller? W1 (make-bst D1 false false)) false) 


(check-expect (same? W1 (make-bst W1 false false)) true)
(check-expect (same? W1 (make-bst Z1 false false)) false)
(define (same? key bst)
  (string=? (widget-name key) (widget-name (bst-widget bst)))
)


(define (find-name key bst)
  (cond
    [(false? bst) false]
    [(same? key bst) key]
    [else (if (not (false? (find-name key (bst-left bst))))
              key
              (find-name key (bst-right bst)))]
    
  )
)
(check-expect (find-name W1 (make-bst W1 false false)) W1 ) 
(check-expect (find-name W1 (make-bst Z1 false false)) false) 
(check-expect (find-name Z1 (make-bst D1 (make-bst A1 false false)
                                      (make-bst W1 false (make-bst Z1 false false)))) Z1)
(check-expect (find-name (make-widget "P1" 0 0)
                         (make-bst D1 (make-bst A1 false false)
                                   (make-bst W1 false (make-bst Z1 false false)))) false)


(define (insert-name widget bst) 
  (cond
   [(false? bst) (make-bst widget false false)]
   [else (if (smaller? widget bst) 
        (make-bst (bst-widget bst) (insert-name widget (bst-left bst)) (bst-right bst))
        (make-bst (bst-widget bst) (bst-left bst) (insert-name widget (bst-right bst))))]
   )
  ) 
(check-expect (insert-name A1 (make-bst D1 false false))
              (make-bst D1 (make-bst A1 false false) false)) 
(check-expect (insert-name W1 (make-bst D1 (make-bst A1 false false) false))
              (make-bst D1 (make-bst A1 false false) (make-bst W1 false false))) 

              

(define (in-order bst)
  (cond
   [(false? bst) empty]
   [else (append (in-order (bst-left bst)) (cons (widget-name (bst-widget bst))
                                                 (in-order (bst-right bst))))]
  )
)
(check-random (in-order (build-tree (random-widgets 100 1000)))
              (sort (map widget-name (random-widgets 100 1000)) string<?))









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