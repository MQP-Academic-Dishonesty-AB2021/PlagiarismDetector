

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)




(define TEXT-SIZE 24)
(define TEXT-COLOR "black")

(define TAB 5)





(define-struct widget (name quantity time price parts))







(define W1 (make-widget "" 0 0 0 empty))

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

 







(check-expect (blanks 0) "") 
(check-expect (blanks TAB) "     ") 
(check-expect (blanks (* TAB 2)) "          ") 



(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))





(check-expect (simple-render W1) 
              (text " : 0 @ $0"
                    TEXT-SIZE TEXT-COLOR))
(check-expect (simple-render Wire) 
              (text (string-append "Wire : 3 @ $5")
                    TEXT-SIZE TEXT-COLOR))
(check-expect (simple-render Telephone) 
              (text (string-append "Telephone : 5 @ $15" "\n"
                                   "     Receiver : 10 @ $7" "\n"
                                   "     Buttons : 8 @ $5" "\n"
                                   "          Numbers : 9 @ $5" "\n"
                                   "     Cord : 7 @ $5" "\n"
                                   "          Wire : 3 @ $5")
                    TEXT-SIZE TEXT-COLOR))

(check-expect (simple-render Jewelry) 
              (text (string-append "Jewelry set : 4 @ $30" "\n"
                                   "     Rings : 15 @ $11" "\n"
                                   "     Necklace : 10 @ $3" "\n"
                                   "          Chain : 7 @ $1" "\n"
                                   "          Pendant : 4 @ $1" "\n"
                                   "     Bracelet : 5 @ $5" "\n"
                                   "          Beads : 25 @ $7" "\n"
                                   "               Glass : 6 @ $4")
                    TEXT-SIZE TEXT-COLOR))



 

(define (simple-render w)
  (render w (λ (w)
              TEXT-COLOR)))





(check-expect (render W1 (λ (w) 
                           TEXT-COLOR))     
              (text " : 0 @ $0" TEXT-SIZE TEXT-COLOR))
(check-expect (render Telephone (λ (w) 
                                  (cond [(< (widget-quantity w) 5)
                                         "red"]
                                        [(< (widget-quantity w) 10)
                                         "yellow"]
                                        [else
                                         TEXT-COLOR])))
              (above/align "left"
                           (text "Telephone : 5 @ $15"
                                 TEXT-SIZE "yellow")
                           (text "     Receiver : 10 @ $7"
                                 TEXT-SIZE TEXT-COLOR)
                           (text (string-append "     Buttons : 8 @ $5" "\n"
                                                "          Numbers : 9 @ $5" "\n"
                                                "     Cord : 7 @ $5")
                                 TEXT-SIZE "yellow")
                           (text "          Wire : 3 @ $5"
                                 TEXT-SIZE "red")))

(check-expect (render Jewelry (λ (w) 
                                (cond [(< (widget-price w) 7)
                                       "green"]
                                      [(> (widget-price w) 25)
                                       "red"]
                                      [else
                                       TEXT-COLOR])))
              (above/align "left"
                           (text "Jewelry set : 4 @ $30"
                                 TEXT-SIZE "red")
                           (text "     Rings : 15 @ $11"
                                 TEXT-SIZE TEXT-COLOR)
                           (text (string-append "     Necklace : 10 @ $3" "\n"
                                                "          Chain : 7 @ $1" "\n"
                                                "          Pendant : 4 @ $1" "\n"
                                                "     Bracelet : 5 @ $5")
                                 TEXT-SIZE "green")
                           (text "          Beads : 25 @ $7"
                                 TEXT-SIZE TEXT-COLOR)
                           (text "               Glass : 6 @ $4"
                                 TEXT-SIZE "green")))



(define (render w fn)
  (local [(define (render--widget w)
            (above/align "left"
                         (text (string-append (widget-name w)
                                              " : "
                                              (number->string (widget-quantity w))
                                              " @ "
                                              "$" (number->string (widget-price w)))
                               TEXT-SIZE (fn w))
                         (render--low (widget-parts w))))

          (define (render--low low)
            (cond [(empty? low) empty-image]
                  [else
                   (above/align "left"
                                (beside (text (blanks TAB) TEXT-SIZE (fn w))
                                        (render--widget (first low)))
                                (render--low (rest low)))]))]
    (render--widget w)))