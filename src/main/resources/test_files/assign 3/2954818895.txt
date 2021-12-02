

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





 


(check-expect (simple-render Bracelet) (above/align "left"
                                                    (text "Bracelet : 5 @ $5" TEXT-SIZE TEXT-COLOR)
                                                    (text (string-append (blanks TAB) "Beads : 25 @ $7") TEXT-SIZE TEXT-COLOR)
                                                    (text (string-append (blanks TAB)
                                                                         (blanks TAB) "Glass : 6 @ $4") TEXT-SIZE TEXT-COLOR)))
(check-expect (simple-render Necklace) (above/align "left"
                                                    (text "Necklace : 10 @ $3" TEXT-SIZE TEXT-COLOR)
                                                    (text "     Chain : 7 @ $1" TEXT-SIZE TEXT-COLOR)
                                                    (text "     Pendant : 4 @ $1" TEXT-SIZE TEXT-COLOR)))

(check-expect (simple-render Jewelry) (above/align "left"
                                                   (text "Jewelry set : 4 @ $30" TEXT-SIZE TEXT-COLOR)
                                                   (text "     Rings : 15 @ $11" TEXT-SIZE TEXT-COLOR)
                                                   (text "     Necklace : 10 @ $3" TEXT-SIZE TEXT-COLOR)
                                                   (text "          Chain : 7 @ $1" TEXT-SIZE TEXT-COLOR)
                                                   (text "          Pendant : 4 @ $1" TEXT-SIZE TEXT-COLOR)
                                                   (text "     Bracelet : 5 @ $5" TEXT-SIZE TEXT-COLOR)
                                                   (text "          Beads : 25 @ $7" TEXT-SIZE TEXT-COLOR)
                                                   (text "               Glass : 6 @ $4" TEXT-SIZE TEXT-COLOR)))





(define (simple-render wid)
  (local [(define (fn-for-widget wid)
            (above/align "left"
                         (widget-text wid)
                         (fn-for-low(widget-parts wid))))
          (define (fn-for-low low)
            (cond [(empty? low) empty-image]
                  [else
                   (above/align "left"
                                (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                                        (fn-for-widget (first low)))
                                (fn-for-low (rest low)))]))]
    (fn-for-widget wid)))




(define (widget-text wid)
  (text (string-append (widget-name wid) " : "
                       (number->string(widget-quantity wid)) " @ $"
                       (number->string(widget-price wid)))
        TEXT-SIZE
        TEXT-COLOR))



(define (text-render wid color)
  (text (string-append (widget-name wid) " : "
                       (number->string(widget-quantity wid)) " @ $"
                       (number->string(widget-price wid)))
        TEXT-SIZE
        color))


(define (color-text-red wid)
  (λ(w)
    (if (< (widget-quantity w) 5)
        "red"
        TEXT-COLOR)))
         
(define (color-text-green wid)
  (λ(w)
    (if (< (widget-price w) 25)
        "green"
        TEXT-COLOR)))


(define (color-text-yellow wid)
  (λ(w)
    (if (< (widget-quantity w) 20)
        "yellow"
        TEXT-COLOR)))

(define (color-text-orange wid)
  (λ(w)
    (if (> (widget-price w) 15)
        "orange"
        TEXT-COLOR)))



              
      
(check-expect (render Wire (color-text-red Wire)) (text-render Wire "red"))
(check-expect (render Cord (color-text-red Cord)) (above/align "left" 
                                                               (text "Cord : 7 @ $5" TEXT-SIZE TEXT-COLOR)
                                                               (text "     Wire : 3 @ $5" TEXT-SIZE "red")))
(check-expect (render Rings (color-text-yellow Rings)) (text-render Rings "yellow"))
(check-expect (render Jewelry (color-text-green Jewelry)) (above/align "left"
                                                                        (text "Jewelry set : 4 @ $30" TEXT-SIZE TEXT-COLOR)
                                                                        (text "     Rings : 15 @ $11" TEXT-SIZE "green")
                                                                        (text "     Necklace : 10 @ $3" TEXT-SIZE "green")
                                                                        (text "          Chain : 7 @ $1" TEXT-SIZE "green")
                                                                        (text "          Pendant : 4 @ $1" TEXT-SIZE "green")
                                                                        (text "     Bracelet : 5 @ $5" TEXT-SIZE "green")
                                                                        (text "          Beads : 25 @ $7" TEXT-SIZE "green")
                                                                        (text "               Glass : 6 @ $4" TEXT-SIZE "green")))

(check-expect (render Jewelry (color-text-orange Jewelry)) (above/align "left"
                                                                        (text "Jewelry set : 4 @ $30" TEXT-SIZE "orange")
                                                                        (text "     Rings : 15 @ $11" TEXT-SIZE TEXT-COLOR)
                                                                        (text "     Necklace : 10 @ $3" TEXT-SIZE TEXT-COLOR)
                                                                        (text "          Chain : 7 @ $1" TEXT-SIZE TEXT-COLOR)
                                                                        (text "          Pendant : 4 @ $1" TEXT-SIZE TEXT-COLOR)
                                                                        (text "     Bracelet : 5 @ $5" TEXT-SIZE TEXT-COLOR)
                                                                        (text "          Beads : 25 @ $7" TEXT-SIZE TEXT-COLOR)
                                                                        (text "               Glass : 6 @ $4" TEXT-SIZE TEXT-COLOR)))







(define (render wid fn)
  (local [(define (fn-for-widget wid fn)
            (above/align "left"
                         (text-render wid (fn wid))   
                         (fn-for-low(widget-parts wid) fn)))
          (define (fn-for-low low fn)
            (cond [(empty? low) empty-image]
                  [else
                   (above/align "left"
                                (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                                        (fn-for-widget (first low) fn))
                                (fn-for-low (rest low) fn))]))]
    (fn-for-widget wid fn)))



