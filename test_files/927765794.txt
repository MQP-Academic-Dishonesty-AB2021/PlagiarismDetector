

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

(define Test-123 (make-widget "Test-123" 15 8 11 empty))
(define test (make-widget "Test" 15 8 11 (list Test-123)))

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black") 
                            
(define TAB 5)





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")





(check-expect (simple-render Jewelry) .)
(check-expect (simple-render Telephone) .)



(define (simple-render widget)
  (render widget (λ(w) "black")))








(check-expect (render Jewelry (λ (w)
                                (cond [(> (widget-quantity w) 10) "yellow"]
                                      [(< (widget-quantity w) 5) "red"]
                                      [else "black"])))
              .)
(check-expect (render Jewelry (λ (w)
                                (cond [(> (widget-price w) 25) "red"]
                                      [(< (widget-price w) 7) "green"]
                                      [else "black"])))
              .)
(check-expect (render Telephone (λ (w)
                                (cond [(> (widget-time w) 6) "red"]
                                      [(< (widget-price w) 7) "green"]
                                      [else "black"])))
              .)



(define (render widget theming)
  (local [(define (render-widget-list low acc)
            (cond
              [(empty? low) empty-image]
              [else
               (above/align "left"
                            (text (string-append (blanks (* TAB acc))
                                  (widget-name (first low)) " : "
                                  (number->string (widget-quantity (first low)))
                                  " @ $"
                                  (number->string (widget-price (first low))))
                                  TEXT-SIZE
                                  (theming (first low)))
                            (render-widget-list (widget-parts (first low)) (add1 acc))
                            (render-widget-list (rest low) acc))]))]
    (render-widget-list (list widget) 0)))

