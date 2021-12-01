

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment3_part_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(define-struct widget (name quantity time price parts))

(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))
(define Telephone (make-widget "Telephone" 5 20 15
                               (list Receiver Buttons Cord)))


(define Glass (make-widget "Glass" 6 9 4 empty))
(define Beads (make-widget "Beads" 25 12 7 (list Glass)))
(define Bracelet (make-widget "Bracelet" 5 3 5 (list Beads)))
(define Chain (make-widget "Chain" 7 2 1 empty))
(define Pendant (make-widget "Pendant" 4 3 1 empty))
(define Necklace (make-widget "Necklace" 10 7 3
                              (list Chain Pendant)))
(define Rings (make-widget "Rings" 15 8 11 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30
                             (list Rings Necklace Bracelet)))


(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")

(define tab-image (text (blanks TAB) TEXT-SIZE TEXT-COLOR))





(check-expect (simple-render Wire)
              (text (write Wire) TEXT-SIZE TEXT-COLOR))

(check-expect (simple-render Jewelry)
              (above/align
               "left"
               (text (write Jewelry) TEXT-SIZE TEXT-COLOR)
               (beside tab-image (simple-low (widget-parts Jewelry)))))

  

 

(define (simple-render widget)
  (render widget (lambda (n) TEXT-COLOR)))




(check-expect (simple-low (list Wire))
              (simple-render Wire))

(check-expect (simple-low empty)
              (square 0 "solid" "white"))

(check-expect (simple-low (widget-parts Bracelet))
              (above/align 
               "left"
               (text (write Beads) TEXT-SIZE TEXT-COLOR)
               (beside tab-image (text (write Glass) TEXT-SIZE TEXT-COLOR))))
               

  
(define (simple-low low)
  (if (empty? low) (square 0 "solid" "white")
      (above/align
       "left"
       (simple-render (first low))
       (simple-low (rest low)))))  





(check-expect (write Glass) "Glass : 6 @ $4")

(check-expect (write Jewelry) "Jewelry set : 4 @ $30")

  

(define (write widget)
  (string-append
   (widget-name widget)
   " : "
   (number->string (widget-quantity widget))
   " @ $"
   (number->string (widget-price widget))))





(check-expect (color1 Glass) "yellow")

(check-expect (color1 Beads) TEXT-COLOR)

(check-expect (color1 Jewelry) "red")

(check-expect (color1 Bracelet) "yellow")

(check-expect (color1 Necklace) TEXT-COLOR)

  

(define (color1 widget)
  (cond [(< (widget-quantity widget) 5) "red"]
        [(< (widget-quantity widget) 10) "yellow"]
        [else TEXT-COLOR]))






(check-expect (color2 Glass) "green")

(check-expect (color2 Beads) TEXT-COLOR)

(check-expect (color2 Jewelry) "red")

(check-expect (color2 Rings) TEXT-COLOR)

(check-expect (color2
               (make-widget "Food" 0 5 25 empty)) TEXT-COLOR)

  

(define (color2 widget)
  (cond [(< (widget-price widget) 7) "green"]
        [(> (widget-price widget) 25) "red"]
        [else TEXT-COLOR]))
  





(check-expect (render Glass color2)
              (text (write Glass) TEXT-SIZE "green"))

(check-expect (render Jewelry color2)
              (above/align
               "left"
               (text (write Jewelry) TEXT-SIZE "red")
               (beside tab-image (render-low (widget-parts Jewelry) color2))))

(check-expect (render Glass color1)
              (text (write Glass) TEXT-SIZE "yellow"))

  
(define (render widget fn) 
  (cond [(empty?
          (widget-parts widget))
         (text (write widget) TEXT-SIZE (fn widget))]
        [else
         (above/align
          "left"
          (text (write widget) TEXT-SIZE (fn widget))
          (beside tab-image (render-low (widget-parts widget) fn)))]))






(check-expect (render-low (list Wire) color1)
              (render Wire color1))

(check-expect (render-low (list Wire) color2)
              (render Wire color2))

(check-expect (render-low empty color1)
              (square 0 "solid" "white"))

(check-expect (render-low (widget-parts Bracelet) color2)
              (above/align 
               "left"
               (text (write Beads) TEXT-SIZE TEXT-COLOR)
               (beside tab-image (text (write Glass) TEXT-SIZE "green"))))
  
(define (render-low low fn)
  (if (empty? low) (square 0 "solid" "white")
      (above/align "left" (render (first low) fn) (render-low (rest low) fn))))
