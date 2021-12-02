

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 3 Pt 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define TEXT-COLOR "black")  

(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")



 






(check-expect (simple-render Wire)
              (text (string-append (widget-name Wire) " : "
                                   (number->string (widget-quantity Wire)) " @ $"
                                   (number->string (widget-price Wire)))
                    TEXT-SIZE TEXT-COLOR)) 
(check-expect (simple-render Cord)
              (above/align "left" (text (string-append (widget-name Cord) " : "
                                                       (number->string (widget-quantity Cord)) " @ $"
                                                       (number->string (widget-price Cord)))
                                        TEXT-SIZE TEXT-COLOR)
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render Wire)))) 
(check-expect (simple-render Jewelry)
              (above/align "left" (text (string-append (widget-name Jewelry) " : "
                                                       (number->string (widget-quantity Jewelry)) " @ $"
                                                       (number->string (widget-price Jewelry)))
                                        TEXT-SIZE TEXT-COLOR)
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render Rings))
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render Necklace))
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render Bracelet)))) 

(define (simple-render widget)
  (local [(define (fn-for-widget widget)
            (above/align "left" (text (string-append (widget-name widget) " : "
                                                     (number->string (widget-quantity widget)) " @ $"
                                                     (number->string (widget-price widget)))
                                      TEXT-SIZE TEXT-COLOR)
                   
                         (fn-for-low (widget-parts widget))))

          (define (fn-for-low low)
            (cond [(empty? low) empty-image]
                  [else (above/align "left" (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                                    (fn-for-widget (first low)))
                                     (fn-for-low (rest low)))]))]
    (fn-for-widget widget)))



(check-expect (render Glass (λ(w) (if (< (widget-quantity w) 7) "green" TEXT-COLOR)))
              (text (string-append (widget-name Glass) " : "
                                   (number->string (widget-quantity Glass)) " @ $"
                                   (number->string (widget-price Glass)))
                    TEXT-SIZE "green")) 
(check-expect (render Bracelet (λ(w) (if (< (string-length (widget-name w)) 6) "green" TEXT-COLOR)))
              (above/align "left" (text (string-append (widget-name Bracelet) " : "
                                                       (number->string (widget-quantity Bracelet)) " @ $"
                                                       (number->string (widget-price Bracelet)))
                                        TEXT-SIZE TEXT-COLOR)
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (render Beads (λ(w) (if (< (string-length
                                                               (widget-name w)) 6)
                                                           "green" TEXT-COLOR)))))) 
(check-expect (render Jewelry (λ(w) (cond [(< (widget-price w) 7) "green"]
                                          [(> (widget-price w) 25) "red"]
                                          [else TEXT-COLOR])))
              (above/align "left" (text (string-append (widget-name Jewelry) " : "
                                                       (number->string (widget-quantity Jewelry)) " @ $"
                                                       (number->string (widget-price Jewelry)))
                                        TEXT-SIZE "red")
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (render Rings (λ(w) (cond [(< (widget-price w) 7) "green"]
                                                             [(> (widget-price w) 25) "red"]
                                                             [else TEXT-COLOR]))))
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (render Necklace (λ(w) (cond [(< (widget-price w) 7) "green"]
                                                                [(> (widget-price w) 25) "red"]
                                                                [else TEXT-COLOR]))))
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (render Bracelet (λ(w) (cond [(< (widget-price w) 7) "green"]
                                                                [(> (widget-price w) 25) "red"]
                                                                [else TEXT-COLOR])))))) 
              
              

(define (render widget fn)
  (local [(define (fn-for-widget widget)
            (above/align "left" (text (string-append (widget-name widget) " : "
                                                     (number->string (widget-quantity widget)) " @ $"
                                                     (number->string (widget-price widget)))
                                      TEXT-SIZE (fn widget))
                   
                         (fn-for-low (widget-parts widget))))

          (define (fn-for-low low)
            (cond [(empty? low) empty-image]
                  [else (above/align "left" (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                                    (fn-for-widget (first low)))
                                     (fn-for-low (rest low)))]))]
    (fn-for-widget widget)))





(check-expect (simple-render2 Wire)
              (text (string-append (widget-name Wire) " : "
                                   (number->string (widget-quantity Wire)) " @ $"
                                   (number->string (widget-price Wire)))
                    TEXT-SIZE "red"))
(check-expect (simple-render2 Cord)
              (above/align "left" (text (string-append (widget-name Cord) " : "
                                                       (number->string (widget-quantity Cord)) " @ $"
                                                       (number->string (widget-price Cord)))
                                        TEXT-SIZE "yellow")
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render2 Wire))))
(check-expect (simple-render2 Bracelet)
              (above/align "left" (text (string-append (widget-name Bracelet) " : "
                                                       (number->string (widget-quantity Bracelet)) " @ $"
                                                       (number->string (widget-price Bracelet)))
                                        TEXT-SIZE "yellow")
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render2 Beads))))

(define (simple-render2 widget)
  (render widget (λ(w) (cond [(< (widget-quantity w) 5) "red"]
                             [(< (widget-quantity w) 10) "yellow"]
                             [else TEXT-COLOR]))))





(check-expect (simple-render3 Telephone)
              (above/align "left" (text (string-append (widget-name Telephone) " : "
                                                       (number->string (widget-quantity Telephone)) " @ $"
                                                       (number->string (widget-price Telephone)))
                                        TEXT-SIZE "Snow")
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render3 Receiver))
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render3 Buttons))
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render3 Cord)))) 
(check-expect (simple-render3 Jewelry)
              (above/align "left" (text (string-append (widget-name Jewelry) " : "
                                                       (number->string (widget-quantity Jewelry)) " @ $"
                                                       (number->string (widget-price Jewelry)))
                                        TEXT-SIZE "Light Cyan")
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render3 Rings))
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render3 Necklace))
                           (beside (text "\t" TEXT-SIZE TEXT-COLOR)
                                   (simple-render3 Bracelet)))) 

(define (simple-render3 widget)
  (render widget (λ(w) (cond[(>= (widget-time w) 20) "Snow"]
                            [(< (widget-quantity w) 10) "Light Cyan"]
                            [else "Papaya Whip"]))))