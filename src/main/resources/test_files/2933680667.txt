

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Part_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black") 
(define TAB 5)



(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")

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










(check-expect (simple-render Wire)
              (text (string-append (widget-name Wire)
                                   " : "
                                   (number->string(widget-quantity Wire))
                                   " @ $"
                                   (number->string(widget-price Wire)))
                    TEXT-SIZE TEXT-COLOR))
(check-expect (simple-render Bracelet)
              (above/align
               "left"
               (text (string-append (widget-name Bracelet)
                                    " : "
                                    (number->string
                                     (widget-quantity Bracelet))
                                    " @ $"
                                    (number->string
                                     (widget-price Bracelet)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks TAB)
                                    (widget-name Beads)
                                    " : "
                                    (number->string
                                     (widget-quantity Beads))
                                    " @ $"
                                    (number->string
                                     (widget-price Beads)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks TAB)
                                    (blanks TAB)
                                    (widget-name Glass)
                                    " : "
                                    (number->string
                                     (widget-quantity Glass))
                                    " @ $"
                                    (number->string
                                     (widget-price Glass)))
                     TEXT-SIZE TEXT-COLOR)))

(check-expect (simple-render Necklace)
              (above/align
               "left"
               (text (string-append (widget-name Necklace)
                                    " : "
                                    (number->string
                                     (widget-quantity Necklace))
                                    " @ $"
                                    (number->string
                                     (widget-price Necklace)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks TAB)
                                    (widget-name Chain)
                                    " : "
                                    (number->string
                                     (widget-quantity Chain))
                                    " @ $"
                                    (number->string
                                     (widget-price Chain)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks TAB)
                                    (widget-name Pendant)
                                    " : "
                                    (number->string
                                     (widget-quantity Pendant))
                                    " @ $"
                                    (number->string
                                     (widget-price Pendant)))
                     TEXT-SIZE TEXT-COLOR)))


 

[define [simple-render wid]
  [render wid [λ [wid] TEXT-COLOR]]]











[define [render wid fn?]
  
  
  
  
  
  [local [
          [define [fn-for--widget wid level]
            [local [[define numBlanks (blanks (* level TAB))]]
              (above/align
               "left"
               (text (string-append numBlanks (widget-name wid)
                                    " : "
                                    (number->string(widget-quantity wid))
                                    " @ $"
                                    (number->string(widget-price wid)))
                     TEXT-SIZE [fn? wid])
               [fn-for--low [widget-parts wid] level])]]
          
          [define [fn-for--low low part]
            [cond [[empty? low] empty-image]
                  [else
                   [above/align "left"
                                [fn-for--widget [first low] [add1 part]] 
                                [fn-for--low [rest low] part]]]]]] 
    [fn-for--widget wid 0]]]





[check-expect [color-by-quantity Rings 5 10]
              (text (string-append (widget-name Rings)
                                   " : "
                                   (number->string(widget-quantity Rings))
                                   " @ $"
                                   (number->string(widget-price Rings)))
                    TEXT-SIZE TEXT-COLOR)]


[check-expect [color-by-quantity Wire 5 10]
              (text (string-append (widget-name Wire)
                                   " : "
                                   (number->string(widget-quantity Wire))
                                   " @ $"
                                   (number->string(widget-price Wire)))
                    TEXT-SIZE "red")]

(check-expect (color-by-quantity Jewelry 5 10)
              (above/align
               "left"
               (text (string-append (widget-name Jewelry)
                                    " : "
                                    (number->string
                                     (widget-quantity Jewelry))
                                    " @ $"
                                    (number->string
                                     (widget-price Jewelry)))
                     TEXT-SIZE "red")
               (text (string-append (blanks TAB)
                                    (widget-name Rings)
                                    " : "
                                    (number->string
                                     (widget-quantity Rings))
                                    " @ $"
                                    (number->string
                                     (widget-price Rings)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks TAB)
                                    (widget-name Necklace)
                                    " : "
                                    (number->string
                                     (widget-quantity Necklace))
                                    " @ $"
                                    (number->string
                                     (widget-price Necklace)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks TAB)
                                    (blanks TAB)
                                    (widget-name Chain)
                                    " : "
                                    (number->string
                                     (widget-quantity Chain))
                                    " @ $"
                                    (number->string
                                     (widget-price Chain)))
                     TEXT-SIZE "yellow")
               (text (string-append (blanks TAB)
                                    (blanks TAB)
                                    (widget-name Pendant)
                                    " : "
                                    (number->string
                                     (widget-quantity Pendant))
                                    " @ $"
                                    (number->string
                                     (widget-price Pendant)))
                     TEXT-SIZE "red")
               (text (string-append (blanks TAB)
                                    (widget-name Bracelet)
                                    " : "
                                    (number->string
                                     (widget-quantity Bracelet))
                                    " @ $"
                                    (number->string
                                     (widget-price Bracelet)))
                     TEXT-SIZE "yellow")
               (text (string-append (blanks TAB)
                                    (blanks TAB)
                                    (widget-name Beads)
                                    " : "
                                    (number->string
                                     (widget-quantity Beads))
                                    " @ $"
                                    (number->string
                                     (widget-price Beads)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks TAB)
                                    (blanks TAB)
                                    (blanks TAB)
                                    (widget-name Glass)
                                    " : "
                                    (number->string
                                     (widget-quantity Glass))
                                    " @ $"
                                    (number->string
                                     (widget-price Glass)))
                     TEXT-SIZE "yellow")))


[define [color-by-quantity wid numLow numHigh]
  [render wid [λ [wid] [cond [[< [widget-quantity wid] numLow]
                              "red"]
                             [[< [widget-quantity wid] numHigh]
                              "yellow"]
                             [else
                              TEXT-COLOR]]]]]








(check-expect (color-by-price Jewelry 7 25)
              (above/align
               "left"
               (text (string-append (widget-name Jewelry)
                                    " : "
                                    (number->string
                                     (widget-quantity Jewelry))
                                    " @ $"
                                    (number->string
                                     (widget-price Jewelry)))
                     TEXT-SIZE "red")
               (text (string-append (blanks TAB)
                                    (widget-name Rings)
                                    " : "
                                    (number->string
                                     (widget-quantity Rings))
                                    " @ $"
                                    (number->string
                                     (widget-price Rings)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks TAB)
                                    (widget-name Necklace)
                                    " : "
                                    (number->string
                                     (widget-quantity Necklace))
                                    " @ $"
                                    (number->string
                                     (widget-price Necklace)))
                     TEXT-SIZE "green")
               (text (string-append (blanks (* TAB 2))
                                    (widget-name Chain)
                                    " : "
                                    (number->string
                                     (widget-quantity Chain))
                                    " @ $"
                                    (number->string
                                     (widget-price Chain)))
                     TEXT-SIZE "green")
               (text (string-append (blanks (* TAB 2))
                                    (widget-name Pendant)
                                    " : "
                                    (number->string
                                     (widget-quantity Pendant))
                                    " @ $"
                                    (number->string
                                     (widget-price Pendant)))
                     TEXT-SIZE "green")
               (text (string-append (blanks TAB)
                                    (widget-name Bracelet)
                                    " : "
                                    (number->string
                                     (widget-quantity Bracelet))
                                    " @ $"
                                    (number->string
                                     (widget-price Bracelet)))
                     TEXT-SIZE "green")
               (text (string-append (blanks (* TAB 2))
                                    (widget-name Beads)
                                    " : "
                                    (number->string
                                     (widget-quantity Beads))
                                    " @ $"
                                    (number->string
                                     (widget-price Beads)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks (* TAB 3))
                                    (widget-name Glass)
                                    " : "
                                    (number->string
                                     (widget-quantity Glass))
                                    " @ $"
                                    (number->string
                                     (widget-price Glass)))
                     TEXT-SIZE "green")) )


(check-expect (color-by-price Buttons 6 10)
              (above/align
               "left"
               (text (string-append (widget-name Buttons)
                                    " : "
                                    (number->string
                                     (widget-quantity Buttons))
                                    " @ $"
                                    (number->string
                                     (widget-price Buttons)))
                     TEXT-SIZE "green")
               (text (string-append (blanks TAB)
                                    (widget-name Numbers)
                                    " : "
                                    (number->string
                                     (widget-quantity Numbers))
                                    " @ $"
                                    (number->string
                                     (widget-price Numbers)))
                     TEXT-SIZE "green")))

(check-expect (color-by-price Receiver 2 5) 
              (text (string-append (widget-name Receiver)
                                   " : "
                                   (number->string
                                    (widget-quantity Receiver))
                                   " @ $"
                                   (number->string
                                    (widget-price Receiver)))
                    TEXT-SIZE "red"))


(check-expect (color-by-price Cord 3 3)
              (above/align
               "left"
               (text (string-append (widget-name Cord)
                                    " : "
                                    (number->string(widget-quantity Cord))
                                    " @ $"
                                    (number->string(widget-price Cord)))
                     TEXT-SIZE "red")
               (text (string-append (blanks TAB)
                                    (widget-name Wire)
                                    " : "
                                    (number->string(widget-quantity Wire))
                                    " @ $"
                                    (number->string(widget-price Wire)))
                     TEXT-SIZE "red")) )


[define [color-by-price wid numLow numHigh]
  [render wid [λ [wid] [cond [[< [widget-price wid] numLow]
                              "green"]
                             [[> [widget-price wid] numHigh]
                              "red"]
                             [else
                              TEXT-COLOR]]]]]

