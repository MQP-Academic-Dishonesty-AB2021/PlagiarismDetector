

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define SPACES-PER-LAYER 3)                             
(define TAB 5)




(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")
(check-expect (blanks 3) "   ")

(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))








(check-expect (widget-attributes Necklace 4) (string-append
    (blanks (* SPACES-PER-LAYER 4))  
    (widget-name Necklace) 
    " : " 
    (number->string (widget-quantity Necklace)) 
    " @ $" 
    (number->string (widget-price Necklace))))
    

(check-expect (widget-attributes Jewelry 1) (string-append
    (blanks (* SPACES-PER-LAYER 1)) 
    (widget-name Jewelry) 
    " : " 
    (number->string (widget-quantity Jewelry)) 
    " @ $" 
    (number->string (widget-price Jewelry))))
    
(define (widget-attributes widget depth)
    (string-append 
        (blanks (* depth SPACES-PER-LAYER))
        (widget-name widget) 
        " : " 
        (number->string (widget-quantity widget)) 
        " @ $" 
        (number->string (widget-price widget))))









(check-expect (simple-render Necklace)
    (above/align "left"
        (text (widget-attributes Necklace 0) 24 TEXT-COLOR)
        (text (widget-attributes Chain 1) 24 TEXT-COLOR)
        (text (widget-attributes Pendant 1) 24 TEXT-COLOR)))


(check-expect (simple-render Telephone)
    (above/align "left"
        (text (widget-attributes Telephone 0) TEXT-SIZE TEXT-COLOR)
        (text (widget-attributes Receiver 1) TEXT-SIZE TEXT-COLOR)
        (text (widget-attributes Buttons 1) TEXT-SIZE TEXT-COLOR)
        (text (widget-attributes Numbers 2) TEXT-SIZE TEXT-COLOR)
        (text (widget-attributes Cord 1) TEXT-SIZE TEXT-COLOR)
        (text (widget-attributes Wire 2) TEXT-SIZE TEXT-COLOR)))


(check-expect (simple-render Glass)
    (text (widget-attributes Glass 0) TEXT-SIZE TEXT-COLOR))

(define (simple-render widget)
    (render widget (lambda (w) TEXT-COLOR)))









(check-expect
    (render Wire 
        (lambda (wid) 
            (local [(define quantity (widget-quantity wid))]
                    (cond 
                        [(< quantity 7) "blue"]
                        [(> quantity 25) "yellow"]
                        [else TEXT-COLOR]))))
    (text (widget-attributes Wire 0) TEXT-SIZE "blue"))


(check-expect 
    (render Telephone 
        (lambda (wid) 
            (local [(define price (widget-price wid))]
                    (cond 
                        [(< price 7) "green"]
                        [(> price 14) "red"]
                        [else TEXT-COLOR]))))
    (above/align "left"
        (text (widget-attributes Telephone 0) TEXT-SIZE "red")
        (text (widget-attributes Receiver 1) TEXT-SIZE TEXT-COLOR)
        (text (widget-attributes Buttons 1) TEXT-SIZE "green") 
        (text (widget-attributes Numbers 2) TEXT-SIZE "green")
        (text (widget-attributes Cord 1) TEXT-SIZE "green")
        (text (widget-attributes Wire 2) TEXT-SIZE "green")))


(check-expect 
    (render Jewelry 
        (lambda (wid) 
            (local [(define price (widget-price wid))]
                    (cond 
                        [(< price 4) "green"]
                        [(> price 22) "red"]
                        [else TEXT-COLOR]))))
    (above/align "left"
        (text (widget-attributes Jewelry 0) TEXT-SIZE "red")
        (text (widget-attributes Rings 1) TEXT-SIZE TEXT-COLOR)
        (text (widget-attributes Necklace 1) TEXT-SIZE "green")
        (text (widget-attributes Chain 2) TEXT-SIZE "green")
        (text (widget-attributes Pendant 2) TEXT-SIZE "green")
        (text (widget-attributes Bracelet 1) TEXT-SIZE TEXT-COLOR)
        (text (widget-attributes Beads 2) TEXT-SIZE TEXT-COLOR)
        (text (widget-attributes Glass 3) TEXT-SIZE TEXT-COLOR)))

(define (render widget fn)
    (local
        [(define (simple-render--widget widget depth-acc)
            (above/align "left" (text
                (widget-attributes widget depth-acc)
                TEXT-SIZE (fn widget))
               (simple-render--low (widget-parts widget) depth-acc)))
        (define (simple-render--low low depth-acc)
            (cond
                [(empty? low) empty-image]
                [else
                    (above/align "left"
                        (simple-render--widget (first low) (add1 depth-acc))
                        (simple-render--low (rest low) depth-acc))]))]
    (simple-render--widget widget 0))) 