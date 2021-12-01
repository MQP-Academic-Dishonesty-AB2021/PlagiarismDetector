

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")







(define-struct widget(name quantity time price parts))

(define (fn-for-widget-tree w)
  (local
    [(define (fn-for-widget w)
       (... (widget-name w)
            (widget-quantity w)
            (widget-time w)
            (widget-price w)
            (fn-for-low (widget-parts w))))
     (define (fn-for-low low)
       (cond
         [(empty? low) empty]
         [(cons? low)
          (... (fn-for-widget (first low))
               (fn-for-low (rest low)))]))]
    (fn-for-widget w)))

(define (fn-for-widget w)
  (... (widget-name w)
       (widget-quantity w)
       (widget-time w)
       (widget-price w)
       (fn-for-low (widget-parts w))))

(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))
(define Telephone (make-widget "Telephone" 5 20 5000
                               (list Receiver Buttons Cord)))





(check-expect (widget-description Wire) "Wire : 3 @ $5")
(define (widget-description w)
  (format "~a : ~a @ $~a"
          (widget-name w)
          (widget-quantity w)
          (widget-price w)))




(check-expect
 (simple-render Wire)
 (text
  (string-append 
   (blanks (* TAB 0))
   (widget-description Wire))
  TEXT-SIZE
  "black"))
(define (simple-render w) (render w (λ(x) "black")))







(check-expect (test-colorscheme-1 Wire) "red")
(check-expect (test-colorscheme-1 Numbers) "yellow")
(check-expect (test-colorscheme-1 Receiver) TEXT-COLOR)
(define test-colorscheme-1 (lambda (w)
                             (cond
                               [(< (widget-quantity w) 5) "red"]
                               [(< (widget-quantity w) 10) "yellow"]
                               [else TEXT-COLOR])))







(check-expect (test-colorscheme-2 Wire) "pink")
(check-expect (test-colorscheme-2 Numbers) "purple")
(check-expect (test-colorscheme-2 Receiver) "green")
(define test-colorscheme-2 (lambda (w)
                             (cond
                               [(< (widget-quantity w) 5) "pink"]
                               [(< (widget-quantity w) 10) "purple"]
                               [else "green"])))










(check-expect
 (render Wire test-colorscheme-1)
 (text
  (string-append 
   (blanks (* TAB 0))
   (widget-description Wire))
  TEXT-SIZE
  "red"))
(check-expect
 (render Numbers test-colorscheme-1)
 (text
  (string-append 
   (blanks (* TAB 0))
   (widget-description Numbers))
  TEXT-SIZE
  "yellow"))
(check-expect
 (render Receiver test-colorscheme-1)
 (text
  (string-append 
   (blanks (* TAB 0))
   (widget-description Receiver))
  TEXT-SIZE
  TEXT-COLOR))
(check-expect
 (render Wire test-colorscheme-2)
 (text
  (string-append 
   (blanks (* TAB 0))
   (widget-description Wire))
  TEXT-SIZE
  "pink"))
(check-expect
 (render Numbers test-colorscheme-2)
 (text
  (string-append 
   (blanks (* TAB 0))
   (widget-description Numbers))
  TEXT-SIZE
  "purple"))
(check-expect
 (render Receiver test-colorscheme-2)
 (text
  (string-append 
   (blanks (* TAB 0))
   (widget-description Receiver))
  TEXT-SIZE
  "green"))
(check-expect
 (render Buttons test-colorscheme-1)
 (above/align "left"
              (text
               (string-append 
                (blanks (* TAB 0))
                (widget-description Buttons))
               TEXT-SIZE
               "yellow")
              (text
               (string-append 
                (blanks (* TAB 1))
                (widget-description Numbers))
               TEXT-SIZE
               "yellow")))

(define (render w color-fn)
  (local
    [(define (fn-for-widget w level)
       (apply above/align
              (append
               (list "left"
                     (text
                      (string-append 
                       (blanks (* TAB level))
                       (widget-description w))
                      TEXT-SIZE
                      (color-fn w)))
               (fn-for-low (widget-parts w) (add1 level)))))
     (define (fn-for-low low level)
       (cond
         [(empty? low) (list empty-image)]
         [(cons? low)
          (cons (fn-for-widget (first low) level)
                (fn-for-low (rest low) level))]))]
    (fn-for-widget w 0)))