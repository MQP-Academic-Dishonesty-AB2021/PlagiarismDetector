

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define Jewel (make-widget "Jewel" 0 17 300
                           empty))
(define Rings (make-widget "Rings" 15 8 11 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30
                             (list Rings Necklace Bracelet)))

 
 

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB "\t") 





(define (blanks n)
  (replicate n TAB))
(check-expect (blanks 0) "")
(check-expect (blanks 1) "\t")
(check-expect (blanks 3) "\t\t\t")



(check-expect (simple-render Jewelry) (text (string-append 
                                             (widget-name Jewelry)
                                             " : "
                                             (number->string
                                              (widget-quantity Jewelry))
                                             " @ $"
                                             (number->string
                                              (widget-price Jewelry))
                                             "\n\t" (widget-name Rings)
                                             " : "
                                             (number->string
                                              (widget-quantity Rings))
                                             " @ $"
                                             (number->string
                                              (widget-price Rings))
                                             "\n\t" (widget-name Necklace)
                                             " : "
                                             (number->string
                                              (widget-quantity Necklace))
                                             " @ $"
                                             (number->string
                                              (widget-price Necklace))
                                             "\n\t\t"
                                             (widget-name Chain)
                                             " : "
                                             (number->string
                                              (widget-quantity Chain))
                                             " @ $"
                                             (number->string
                                              (widget-price Chain))
                                             "\n\t\t"
                                             (widget-name Pendant)
                                             " : "
                                             (number->string
                                              (widget-quantity Pendant))
                                             " @ $"
                                             (number->string
                                              (widget-price Pendant))
                                             "\n\t" (widget-name Bracelet)
                                             " : "
                                             (number->string
                                              (widget-quantity Bracelet))
                                             " @ $"
                                             (number->string
                                              (widget-price Bracelet))
                                             "\n\t\t" (widget-name Beads)
                                             " : "
                                             (number->string
                                              (widget-quantity Beads))
                                             " @ $"
                                             (number->string
                                              (widget-price Beads))
                                             "\n\t\t\t"
                                             (widget-name Glass)
                                             " : "
                                             (number->string
                                              (widget-quantity Glass))
                                             " @ $"
                                             (number->string
                                              (widget-price Glass)))
                                            TEXT-SIZE
                                            TEXT-COLOR))
(check-expect (simple-render Wire) (text (string-append 
                                          (widget-name Wire)
                                          " : "
                                          (number->string
                                           (widget-quantity Wire))
                                          " @ $" (number->string
                                                  (widget-price Wire)))
                                         TEXT-SIZE
                                         TEXT-COLOR))
 
 

(define (simple-render widget)
  (render (λ (n) TEXT-COLOR) widget))
                                            



(check-expect (render (λ (n) (cond[(> 7 (widget-price n)) "green"] 
                                  [(< 25 (widget-price n)) "red"]
                                  [else "black"])) Jewelry)
              (above/align "left" (text (string-append
                                         (widget-name Jewelry)
                                         " : "
                                         (number->string
                                          (widget-quantity Jewelry))
                                         " @ $"
                                         (number->string
                                          (widget-price Jewelry)))
                                        TEXT-SIZE "red")
                           (text (string-append "\t"
                                                (widget-name Rings)
                                                " : "
                                                (number->string
                                                 (widget-quantity Rings))
                                                " @ $"
                                                (number->string
                                                 (widget-price Rings)))
                                 TEXT-SIZE "black")
                           (text (string-append "\t"
                                                (widget-name Necklace)
                                                " : " (number->string
                                                       (widget-quantity
                                                        Necklace))
                                                " @ $"
                                                (number->string
                                                 (widget-price Necklace)))
                                 TEXT-SIZE "green")
                           (text (string-append "\t\t"
                                                (widget-name Chain)
                                                " : "
                                                (number->string
                                                 (widget-quantity Chain))
                                                " @ $"
                                                (number->string
                                                 (widget-price Chain)))
                                 TEXT-SIZE "green")
                           (text (string-append "\t\t"
                                                (widget-name Pendant)
                                                " : "
                                                (number->string
                                                 (widget-quantity Pendant))
                                                " @ $"
                                                (number->string
                                                 (widget-price Pendant)))
                                 TEXT-SIZE "green")
                           (text (string-append "\t"
                                                (widget-name Bracelet)
                                                " : "
                                                (number->string
                                                 (widget-quantity Bracelet))
                                                " @ $"
                                                (number->string
                                                 (widget-price Bracelet)))
                                 TEXT-SIZE "green")
                           (text (string-append "\t\t"
                                                (widget-name Beads)
                                                " : "
                                                (number->string
                                                 (widget-quantity Beads))
                                                " @ $"
                                                (number->string
                                                 (widget-price Beads)))
                                 TEXT-SIZE "black")
                           (text (string-append "\t\t\t"
                                                (widget-name Glass)
                                                " : "
                                                (number->string
                                                 (widget-quantity Glass))
                                                " @ $"
                                                (number->string
                                                 (widget-price Glass)))
                                 TEXT-SIZE "green")))
(check-expect (render (λ (n) (cond[(> 5 (widget-quantity n)) "red"] 
                                  [(> 10 (widget-quantity n)) "yellow"]
                                  [else "green"])) Jewelry )
              (above/align "left" (text (string-append (widget-name Jewelry)
                                                       " : "
                                                       (number->string
                                                        (widget-quantity
                                                         Jewelry))
                                                       " @ $"
                                                       (number->string
                                                        (widget-price
                                                         Jewelry)))
                                        TEXT-SIZE "red")
                           (text (string-append "\t"
                                                (widget-name Rings)
                                                " : "
                                                (number->string
                                                 (widget-quantity Rings))
                                                " @ $"
                                                (number->string
                                                 (widget-price Rings)))
                                 TEXT-SIZE "green")
                           (text (string-append "\t"
                                                (widget-name Necklace)
                                                " : "
                                                (number->string
                                                 (widget-quantity Necklace))
                                                " @ $"
                                                (number->string
                                                 (widget-price Necklace)))
                                 TEXT-SIZE "green")
                           (text (string-append "\t\t" (widget-name Chain)
                                                " : "
                                                (number->string
                                                 (widget-quantity Chain))
                                                " @ $"
                                                (number->string
                                                 (widget-price Chain)))
                                 TEXT-SIZE "yellow")
                           (text (string-append "\t\t"
                                                (widget-name Pendant)
                                                " : "
                                                (number->string
                                                 (widget-quantity Pendant))
                                                " @ $"
                                                (number->string
                                                 (widget-price Pendant)))
                                 TEXT-SIZE "red")
                           (text (string-append "\t"
                                                (widget-name Bracelet)
                                                " : "
                                                (number->string
                                                 (widget-quantity Bracelet))
                                                " @ $"
                                                (number->string
                                                 (widget-price Bracelet)))
                                 TEXT-SIZE "yellow")
                           (text (string-append "\t\t" (widget-name Beads)
                                                " : "
                                                (number->string
                                                 (widget-quantity Beads))
                                                " @ $"
                                                (number->string
                                                 (widget-price Beads)))
                                 TEXT-SIZE "green")
                           (text (string-append "\t\t\t"
                                                (widget-name Glass)
                                                " : "
                                                (number->string
                                                 (widget-quantity Glass))
                                                " @ $"
                                                (number->string
                                                 (widget-price Glass)))
                                 TEXT-SIZE "yellow")))
(check-expect (render (λ (n) (cond[(> 5 (widget-quantity n)) "red"] 
                                  [(> 10 (widget-quantity n)) "yellow"]
                                  [else "green"])) Wire)
              (text (string-append (widget-name Wire) " : "
                                   (number->string (widget-quantity Wire))
                                   " @ $" (number->string
                                           (widget-price Wire)))
                    TEXT-SIZE "red"))
 

(define (render fn? widget)
  (local [
          (define (render--widget widget tabs)
            (above/align "left" (text (string-append (blanks tabs)
                                                     (widget-name widget)
                                                     " : " (number->string
                                                            (widget-quantity
                                                             widget))
                                                     " @ $" (number->string
                                                             (widget-price
                                                              widget)))
                                      TEXT-SIZE (fn? widget))
                         (render--low (widget-parts widget) (+ 1 tabs))))
          (define (render--low low tabs)
            (cond [(empty? low) empty-image]
                  [else
                   (above/align "left" (render--widget (first low) tabs)
                                (render--low (rest low) tabs))]))]
    (render--widget widget 0)))