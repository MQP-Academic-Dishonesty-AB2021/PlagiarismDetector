

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





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



(define WIDTH 400)
(define HEIGHT 400)
(define TEXT-X (/ WIDTH 2))
(define TEXT-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define TEXT-SIZE 24)
(define TEXT-COLOR "black")
(define TAB 5)
(define BLANK-IMG (square 0 "solid" "white"))






(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))

(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")
(check-expect (blanks 7) "       ")







(check-expect (simple-render Wire)                                                    
              (place-image (text (string-append "Wire"
                                                " : "
                                                (number->string (widget-quantity Wire))
                                                " @ $"
                                                (number->string (widget-price Wire)))
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           TEXT-X
                           TEXT-Y
                           MTS))

(check-expect (simple-render Telephone)                                              
              (place-image (above/align "left" (text (string-append "Telephone"
                                                                    " : "
                                                                    (number->string (widget-quantity Telephone))
                                                                    " @ $"
                                                                    (number->string (widget-price Telephone)))
                                                     TEXT-SIZE
                                                     TEXT-COLOR)
                                        (text (string-append (blanks TAB)
                                                             "Receiver"
                                                             " : "
                                                             (number->string (widget-quantity Receiver))
                                                             " @ $"
                                                             (number->string (widget-price Receiver)))
                                              TEXT-SIZE
                                              TEXT-COLOR)
                                        (text (string-append (blanks TAB)
                                                             "Buttons"
                                                             " : "
                                                             (number->string (widget-quantity Buttons))
                                                             " @ $"
                                                             (number->string (widget-price Buttons)))
                                              TEXT-SIZE
                                              TEXT-COLOR)
                                        (text (string-append (blanks (+ TAB TAB))
                                                             "Numbers"
                                                             " : "
                                                             (number->string (widget-quantity Numbers))
                                                             " @ $"
                                                             (number->string (widget-price Numbers)))
                                              TEXT-SIZE
                                              TEXT-COLOR)
                                        (text (string-append (blanks TAB)
                                                             "Cord"
                                                             " : "
                                                             (number->string (widget-quantity Cord))
                                                             " @ $"
                                                             (number->string (widget-price Cord)))
                                              TEXT-SIZE
                                              TEXT-COLOR)
                                        (text (string-append (blanks (+ TAB TAB))
                                                             "Wire"
                                                             " : "
                                                             (number->string (widget-quantity Wire))
                                                             " @ $"
                                                             (number->string (widget-price Wire)))
                                              TEXT-SIZE
                                              TEXT-COLOR))
                                        
                           TEXT-X
                           TEXT-Y
                           MTS))

(define (simple-render widget)
  (render widget (λ (widget)
                   TEXT-COLOR)))








(check-expect (render Wire (λ (widget)                                               
                             (cond
                               [(< (widget-quantity widget) 5) "red"]
                               [(< (widget-quantity widget) 10) "yellow"]
                               [else
                                TEXT-COLOR])))                                                   
              (place-image (text (string-append "Wire"
                                                " : "
                                                (number->string (widget-quantity Wire))
                                                " @ $"
                                                (number->string (widget-price Wire)))
                                 TEXT-SIZE
                                 "red")
                           TEXT-X
                           TEXT-Y
                           MTS))

(check-expect (render Telephone (λ (widget)                                               
                                  (cond
                                    [(< (widget-quantity widget) 5) "blue"]
                                    [(< (widget-quantity widget) 10) "green"]
                                    [else
                                     TEXT-COLOR])))                                              
              (place-image (above/align "left" (text (string-append "Telephone"
                                                                    " : "
                                                                    (number->string (widget-quantity Telephone))
                                                                    " @ $"
                                                                    (number->string (widget-price Telephone)))
                                                     TEXT-SIZE
                                                     "green")
                                        (text (string-append (blanks TAB)
                                                             "Receiver"
                                                             " : "
                                                             (number->string (widget-quantity Receiver))
                                                             " @ $"
                                                             (number->string (widget-price Receiver)))
                                              TEXT-SIZE
                                              TEXT-COLOR)
                                        (text (string-append (blanks TAB)
                                                             "Buttons"
                                                             " : "
                                                             (number->string (widget-quantity Buttons))
                                                             " @ $"
                                                             (number->string (widget-price Buttons)))
                                              TEXT-SIZE
                                              "green")
                                        (text (string-append (blanks (+ TAB TAB))
                                                             "Numbers"
                                                             " : "
                                                             (number->string (widget-quantity Numbers))
                                                             " @ $"
                                                             (number->string (widget-price Numbers)))
                                              TEXT-SIZE
                                              "green")
                                        (text (string-append (blanks TAB)
                                                             "Cord"
                                                             " : "
                                                             (number->string (widget-quantity Cord))
                                                             " @ $"
                                                             (number->string (widget-price Cord)))
                                              TEXT-SIZE
                                              "green")
                                        (text (string-append (blanks (+ TAB TAB))
                                                             "Wire"
                                                             " : "
                                                             (number->string (widget-quantity Wire))
                                                             " @ $"
                                                             (number->string (widget-price Wire)))
                                              TEXT-SIZE
                                              "blue"))
                                        
                           TEXT-X
                           TEXT-Y
                           MTS))

(define (render widget fn)
  (local[
         (define (generate-text widget)
           (local[
                  
                  
                  (define (fn-for-widget widget num-blanks)
                    (above/align "left" (text (string-append (blanks num-blanks)
                                                             (widget-name widget) " : "
                                                             (number->string (widget-quantity widget)) " @ $"
                                                             (number->string (widget-price widget)))
                                              TEXT-SIZE
                                              (fn widget))
                                 (fn-for-low (widget-parts widget) num-blanks))) 

                  (define (fn-for-low low num-blanks)
                    (cond
                      [(empty? low) BLANK-IMG]
                      [else
                       (above/align "left" (fn-for-widget (first low) (+ TAB num-blanks))
                                    (fn-for-low (rest low) num-blanks))]))]
             (fn-for-widget widget 0)))]
    (place-image (generate-text widget) TEXT-X TEXT-Y MTS)))








(check-expect (render-stock Wire 10 5)                                                
              (place-image (text (string-append "Wire"
                                                " : "
                                                (number->string (widget-quantity Wire))
                                                " @ $"
                                                (number->string (widget-price Wire)))
                                 TEXT-SIZE
                                 "red")
                           TEXT-X
                           TEXT-Y
                           MTS))

(check-expect (render-stock Telephone 10 5)                                              
              (place-image (above/align "left" (text (string-append "Telephone"
                                                                    " : "
                                                                    (number->string (widget-quantity Telephone))
                                                                    " @ $"
                                                                    (number->string (widget-price Telephone)))
                                                     TEXT-SIZE
                                                     "yellow")
                                        (text (string-append (blanks TAB)
                                                             "Receiver"
                                                             " : "
                                                             (number->string (widget-quantity Receiver))
                                                             " @ $"
                                                             (number->string (widget-price Receiver)))
                                              TEXT-SIZE
                                              TEXT-COLOR)
                                        (text (string-append (blanks TAB)
                                                             "Buttons"
                                                             " : "
                                                             (number->string (widget-quantity Buttons))
                                                             " @ $"
                                                             (number->string (widget-price Buttons)))
                                              TEXT-SIZE
                                              "yellow")
                                        (text (string-append (blanks (+ TAB TAB))
                                                             "Numbers"
                                                             " : "
                                                             (number->string (widget-quantity Numbers))
                                                             " @ $"
                                                             (number->string (widget-price Numbers)))
                                              TEXT-SIZE
                                              "yellow")
                                        (text (string-append (blanks TAB)
                                                             "Cord"
                                                             " : "
                                                             (number->string (widget-quantity Cord))
                                                             " @ $"
                                                             (number->string (widget-price Cord)))
                                              TEXT-SIZE
                                              "yellow")
                                        (text (string-append (blanks (+ TAB TAB))
                                                             "Wire"
                                                             " : "
                                                             (number->string (widget-quantity Wire))
                                                             " @ $"
                                                             (number->string (widget-price Wire)))
                                              TEXT-SIZE
                                              "red"))
                                        
                           TEXT-X
                           TEXT-Y
                           MTS))

(define (render-stock widget upper lower)
  (render widget
          (λ (widget)
            (cond
              [(< (widget-quantity widget) lower) "red"]
              [(< (widget-quantity widget) upper) "yellow"]
              [else
               TEXT-COLOR]))))








(check-expect (render-price Wire 25 7)                                                
              (place-image (text (string-append "Wire"
                                                " : "
                                                (number->string (widget-quantity Wire))
                                                " @ $"
                                                (number->string (widget-price Wire)))
                                 TEXT-SIZE
                                 "green")
                           TEXT-X
                           TEXT-Y
                           MTS))

(check-expect (render-price Telephone 14 7)                                              
              (place-image (above/align "left" (text (string-append "Telephone"
                                                                    " : "
                                                                    (number->string (widget-quantity Telephone))
                                                                    " @ $"
                                                                    (number->string (widget-price Telephone)))
                                                     TEXT-SIZE
                                                     "red")
                                        (text (string-append (blanks TAB)
                                                             "Receiver"
                                                             " : "
                                                             (number->string (widget-quantity Receiver))
                                                             " @ $"
                                                             (number->string (widget-price Receiver)))
                                              TEXT-SIZE
                                              TEXT-COLOR)
                                        (text (string-append (blanks TAB)
                                                             "Buttons"
                                                             " : "
                                                             (number->string (widget-quantity Buttons))
                                                             " @ $"
                                                             (number->string (widget-price Buttons)))
                                              TEXT-SIZE
                                              "green")
                                        (text (string-append (blanks (+ TAB TAB))
                                                             "Numbers"
                                                             " : "
                                                             (number->string (widget-quantity Numbers))
                                                             " @ $"
                                                             (number->string (widget-price Numbers)))
                                              TEXT-SIZE
                                              "green")
                                        (text (string-append (blanks TAB)
                                                             "Cord"
                                                             " : "
                                                             (number->string (widget-quantity Cord))
                                                             " @ $"
                                                             (number->string (widget-price Cord)))
                                              TEXT-SIZE
                                              "green")
                                        (text (string-append (blanks (+ TAB TAB))
                                                             "Wire"
                                                             " : "
                                                             (number->string (widget-quantity Wire))
                                                             " @ $"
                                                             (number->string (widget-price Wire)))
                                              TEXT-SIZE
                                              "green"))
                                        
                           TEXT-X
                           TEXT-Y
                           MTS))

(define (render-price widget upper lower)
  (render widget
          (λ (widget)
            (cond
              [(< (widget-price widget) lower) "green"]
              [(> (widget-price widget) upper) "red"]
              [else
               TEXT-COLOR]))))








