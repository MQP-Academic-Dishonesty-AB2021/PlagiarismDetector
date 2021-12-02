

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 3 Part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)

(define-struct widget(name quantity time price parts))



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
(define TEXT-COLOR "white")  

(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")




(check-expect (widget->string Glass 0)
              (string-append (widget-name Glass)
                             " : "
                             (number->string (widget-quantity Glass))
                             " @ $"
                             (number->string (widget-price Glass))))
(check-expect (widget->string Buttons 1)
              (string-append (blanks (* TAB 1))
                             (widget-name Buttons)
                             " : "
                             (number->string (widget-quantity Buttons))
                             " @ $"
                             (number->string (widget-price Buttons))))

(define (widget->string awidget depth)
  (string-append (blanks (* TAB depth))
                 (widget-name awidget)
                 " : "
                 (number->string (widget-quantity awidget))
                 " @ $"
                 (number->string (widget-price awidget)))
  )







(check-expect (simple-render Glass)
              (text (widget->string Glass 0) TEXT-SIZE TEXT-COLOR)) 
(check-expect (simple-render Cord)
              (above/align "left"
                           (text (widget->string Cord 0) TEXT-SIZE TEXT-COLOR)
                           (text (widget->string Wire 1) TEXT-SIZE TEXT-COLOR)))
(check-expect
 (simple-render Necklace)
 (above/align "left"
              (text (widget->string Necklace 0) TEXT-SIZE TEXT-COLOR)
              (text (widget->string Chain 1) TEXT-SIZE TEXT-COLOR)
              (text (widget->string Pendant 1) TEXT-SIZE TEXT-COLOR)))

               
 

(define (simple-render awidget)
  (render awidget (λ (n) TEXT-COLOR))
  )







(check-expect (render Wire (λ (awidget)
                             (cond
                               [(> (widget-quantity awidget) 2) "red"]
                               [else "black"])))
              (text (widget->string Wire 0) TEXT-SIZE "red"))
(check-expect (render Cord (λ (awidget)
                             (cond
                               [(> (widget-quantity awidget) 5) "blue"]
                               [else "green"])))
              (above/align "left"
                           (text (widget->string Cord 0) TEXT-SIZE "blue")
                           (text (widget->string Wire 1) TEXT-SIZE "green")))
(check-expect (render Necklace (λ (awidget)
                                 (cond
                                   [(< (widget-quantity awidget) 5) "red"]
                                   [(< (widget-quantity awidget) 10) "yellow"]
                                   [else "black"])))
              (above/align "left"
                           (text (widget->string Necklace 0) TEXT-SIZE "black")
                           (text (widget->string Chain 1) TEXT-SIZE "yellow")
                           (text (widget->string Pendant 1) TEXT-SIZE "red")))
                                                                                                               


(define (render awidget fn)
  (local [(define (display-widget awidget depth)
            (above/align "left"
                         (text (widget->string awidget depth)
                               TEXT-SIZE (fn awidget))
                         (display-widget--low (widget-parts awidget)
                                              (+ 1 depth)))
            )

          (define (display-widget--low low depth)
            (cond
              [(empty? low) (square 0 "solid" "black")]
              [else
               (above/align "left" (display-widget (first low) depth)
                            (display-widget--low (rest low) depth))]
              )
            )
          
          ]
    
    (display-widget awidget 0)))

