

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  
                             
(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")




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










(check-expect (simple-render Wire)
              (text (string-append (widget-name Wire)
                                    ": "
                                    (number->string (widget-quantity Wire))
                                    " @ $"
                                    (number->string (widget-price Wire)))
                     TEXT-SIZE TEXT-COLOR))
                     

(check-expect (simple-render Bracelet)
               (above/align "left"
                            (text (string-append (widget-name Bracelet)
                                                 ": "
                                                 (number->string (widget-quantity Bracelet))
                                                 " @ $"
                                                (number->string (widget-price Bracelet)))
                                  TEXT-SIZE TEXT-COLOR)

                            (text (string-append (blanks TAB)
                                                 (widget-name Beads)
                                                 ": "
                                                 (number->string (widget-quantity Beads))
                                                 " @ $"
                                                 (number->string (widget-price Beads)))
                                  TEXT-SIZE TEXT-COLOR)

                            (text (string-append (blanks (* 2 TAB))
                                                 (widget-name Glass)
                                                 ": "
                                                 (number->string (widget-quantity Glass))
                                                 " @ $"
                                                 (number->string (widget-price Glass)))
                                  TEXT-SIZE TEXT-COLOR)))
                                  

(check-expect (simple-render Telephone)
               (above/align "left"
                            (text (string-append (widget-name Telephone)
                                                 ": "
                                                 (number->string (widget-quantity Telephone))
                                                 " @ $"
                                                (number->string (widget-price Telephone)))
                                  TEXT-SIZE TEXT-COLOR)

                            (text (string-append (blanks TAB)
                                                 (widget-name Receiver)
                                                 ": "
                                                 (number->string (widget-quantity Receiver))
                                                 " @ $"
                                                 (number->string (widget-price Receiver)))
                                  TEXT-SIZE TEXT-COLOR)
                            
                            (text (string-append (blanks TAB)
                                                 (widget-name Buttons)
                                                 ": "
                                                 (number->string (widget-quantity Buttons))
                                                 " @ $"
                                                 (number->string (widget-price Buttons)))
                                  TEXT-SIZE TEXT-COLOR)

                            (text (string-append (blanks (* 2 TAB))
                                                 (widget-name Numbers)
                                                 ": "
                                                 (number->string (widget-quantity Numbers))
                                                 " @ $"
                                                 (number->string (widget-price Numbers)))
                                  TEXT-SIZE TEXT-COLOR)

                            (text (string-append (blanks TAB)
                                                 (widget-name Cord)
                                                 ": "
                                                 (number->string (widget-quantity Cord))
                                                 " @ $"
                                                 (number->string (widget-price Cord)))
                                  TEXT-SIZE TEXT-COLOR)

                            (text (string-append (blanks (* 2 TAB))
                                                 (widget-name Wire)
                                                 ": "
                                                 (number->string (widget-quantity Wire))
                                                 " @ $"
                                                 (number->string (widget-price Wire)))
                                  TEXT-SIZE TEXT-COLOR)))
                     

 








(check-expect (render Wire color-price)
              (text (string-append (widget-name Wire)
                                    ": "
                                    (number->string (widget-quantity Wire))
                                    " @ $"
                                    (number->string (widget-price Wire)))
                     TEXT-SIZE "green"))
                     

(check-expect (render Bracelet color-price)
               (above/align "left"
                            (text (string-append (widget-name Bracelet)
                                                 ": "
                                                 (number->string (widget-quantity Bracelet))
                                                 " @ $"
                                                (number->string (widget-price Bracelet)))
                                  TEXT-SIZE "green")

                            (text (string-append (blanks TAB)
                                                 (widget-name Beads)
                                                 ": "
                                                 (number->string (widget-quantity Beads))
                                                 " @ $"
                                                 (number->string (widget-price Beads)))
                                  TEXT-SIZE "yellow")

                            (text (string-append (blanks (* 2 TAB))
                                                 (widget-name Glass)
                                                 ": "
                                                 (number->string (widget-quantity Glass))
                                                 " @ $"
                                                 (number->string (widget-price Glass)))
                                  TEXT-SIZE "green")))
                                  

(check-expect (render Telephone stockcheck)
               (above/align "left"
                            (text (string-append (widget-name Telephone)
                                                 ": "
                                                 (number->string (widget-quantity Telephone))
                                                 " @ $"
                                                (number->string (widget-price Telephone)))
                                  TEXT-SIZE "yellow")

                            (text (string-append (blanks TAB)
                                                 (widget-name Receiver)
                                                 ": "
                                                 (number->string (widget-quantity Receiver))
                                                 " @ $"
                                                 (number->string (widget-price Receiver)))
                                  TEXT-SIZE "black")
                            
                            (text (string-append (blanks TAB)
                                                 (widget-name Buttons)
                                                 ": "
                                                 (number->string (widget-quantity Buttons))
                                                 " @ $"
                                                 (number->string (widget-price Buttons)))
                                  TEXT-SIZE "yellow")

                            (text (string-append (blanks (* 2 TAB))
                                                 (widget-name Numbers)
                                                 ": "
                                                 (number->string (widget-quantity Numbers))
                                                 " @ $"
                                                 (number->string (widget-price Numbers)))
                                  TEXT-SIZE "yellow")

                            (text (string-append (blanks TAB)
                                                 (widget-name Cord)
                                                 ": "
                                                 (number->string (widget-quantity Cord))
                                                 " @ $"
                                                 (number->string (widget-price Cord)))
                                  TEXT-SIZE "yellow")

                            (text (string-append (blanks (* 2 TAB))
                                                 (widget-name Wire)
                                                 ": "
                                                 (number->string (widget-quantity Wire))
                                                 " @ $"
                                                 (number->string (widget-price Wire)))
                                  TEXT-SIZE "red")))
                       

(define (render widge fn)
  (local [(define (r-widget wid lvl) 
            (above/align "left"        
                         (wid->text wid lvl)
                         (r-low (widget-parts wid) (+ lvl 1))))

          (define (r-low low lvl)
            (cond
              [(empty? low) empty-image]
              [else
               (above/align "left"
                            (r-widget (first low) lvl)
                            (r-low (rest low) lvl))]))
            
          (define (wid->text wid lvl)
            (text (string-append (blanks (* TAB lvl))
                                 (widget-name wid)
                                 ": "
                                 (number->string (widget-quantity wid))
                                 " @ $"
                                 (number->string (widget-price wid)))
                  TEXT-SIZE (fn wid)))] 
  
    (r-widget widge 0)))









(check-expect (color-price Wire) "green") 
(check-expect (color-price Receiver) "yellow") 
(check-expect (color-price Telephone) "yellow") 
(check-expect (color-price Jewelry) "red") 


(define (color-price wid)
  (cond [(<= (widget-price wid) 5) "green"]
        [(<= (widget-price wid) 15) "yellow"]
        [else "red"]))








(check-expect (stockcheck Receiver) "black") 
(check-expect (stockcheck Telephone) "yellow") 


(check-expect (stockcheck Wire) "red") 
(check-expect (stockcheck Rings) "black") 

(define (stockcheck wid)
  (cond
    [(< (widget-quantity wid) 5) "red"]
    [(< (widget-quantity wid) 10) "yellow"]
    [else "black"]))


(define (simple-render widget)
  (render widget (λ (x) "black"))) 