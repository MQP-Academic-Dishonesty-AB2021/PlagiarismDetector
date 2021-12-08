

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment3Part3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

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
(define EMPTY (square 0 "solid" "white")) 
 




(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")


 







(define (simple-render widg)
   (render widg (λ (widg)
                  TEXT-COLOR)))

(check-expect (simple-render Jewelry)
  (above/align "left"

    (text "Jewelry set : 4 @ $30" TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks TAB) "Rings : 15 @ $11") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks TAB) "Necklace : 10 @ $3") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks (* TAB 2)) "Chain : 7 @ $1") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks (* TAB 2)) "Pendant : 4 @ $1") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks TAB) "Bracelet : 5 @ $5") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks (* TAB 2)) "Beads : 25 @ $7") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks (* TAB 3)) "Glass : 6 @ $4") TEXT-SIZE TEXT-COLOR)
    EMPTY))

(check-expect (simple-render Receiver)
       (above/align "left" 
            (text "Receiver : 10 @ $7" TEXT-SIZE TEXT-COLOR)
            EMPTY))

(check-expect (simple-render Buttons)
         (above/align "left"
                (text "Buttons : 8 @ $5" TEXT-SIZE TEXT-COLOR)
                (text (string-append (blanks TAB) "Numbers : 9 @ $5") TEXT-SIZE TEXT-COLOR
                      )))
             
























             






         
(check-expect (render Jewelry
                      (λ(w)
                        (cond [(< (widget-quantity w) 5) "red"]
                              [(< (widget-quantity w) 10) "yellow"]
                              [else TEXT-COLOR]
                             )))
  (above/align "left"

               (text "Jewelry set : 4 @ $30" TEXT-SIZE "red")
               (text (string-append (blanks TAB) "Rings : 15 @ $11") TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks TAB) "Necklace : 10 @ $3") TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks (* TAB 2)) "Chain : 7 @ $1") TEXT-SIZE "yellow")
               (text (string-append (blanks (* TAB 2)) "Pendant : 4 @ $1") TEXT-SIZE "red")
               (text (string-append (blanks TAB) "Bracelet : 5 @ $5") TEXT-SIZE "yellow")
               (text (string-append (blanks (* TAB 2)) "Beads : 25 @ $7") TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks (* TAB 3)) "Glass : 6 @ $4") TEXT-SIZE "yellow")
               EMPTY))

(check-expect (render Buttons
                      (λ(w) 
                        (if (string=? (widget-name w) "Buttons")
                           "blue"
                           TEXT-COLOR))
                        )
                        
         (above/align "left"
                (text "Buttons : 8 @ $5" TEXT-SIZE "blue")
                (text (string-append (blanks TAB) "Numbers : 9 @ $5") TEXT-SIZE TEXT-COLOR
                )))

(check-expect (render Jewelry
                      (λ(w)
                        (if (= (widget-price w) 1)
                            "green"
                            TEXT-COLOR)))
     (above/align "left"

    (text "Jewelry set : 4 @ $30" TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks TAB) "Rings : 15 @ $11") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks TAB) "Necklace : 10 @ $3") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks (* TAB 2)) "Chain : 7 @ $1") TEXT-SIZE "green")
    (text (string-append (blanks (* TAB 2)) "Pendant : 4 @ $1") TEXT-SIZE "green")
    (text (string-append (blanks TAB) "Bracelet : 5 @ $5") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks (* TAB 2)) "Beads : 25 @ $7") TEXT-SIZE TEXT-COLOR)
    (text (string-append (blanks (* TAB 3)) "Glass : 6 @ $4") TEXT-SIZE TEXT-COLOR)
    EMPTY))

(check-expect (render Telephone
                      (λ(w)
                        (if (= (widget-price w) 5)
                            "orange"
                            TEXT-COLOR)))
           (above/align "left"
                        (text "Telephone : 5 @ $15" TEXT-SIZE TEXT-COLOR)
                        (text (string-append (blanks TAB) "Receiver : 10 @ $7") TEXT-SIZE TEXT-COLOR)
                        (text (string-append (blanks TAB) "Buttons : 8 @ $5") TEXT-SIZE "orange")
                        (text (string-append (blanks (* 2 TAB)) "Numbers : 9 @ $5") TEXT-SIZE "orange")
                        (text (string-append (blanks TAB) "Cord : 7 @ $5") TEXT-SIZE "orange")
                        (text (string-append (blanks (* 2 TAB)) "Wire : 3 @ $5") TEXT-SIZE "orange")
                        EMPTY))



                        

(define (render widg fn)
  (local [(define (fn-for-widget--widget w layer)
            (above/align "left"
             (text (get-string w layer) TEXT-SIZE (fn w))
             (fn-for-widget--low (widget-parts w) layer)))

          (define (fn-for-widget--low low layer)
            (cond [(empty? low) EMPTY]
                  [else
                   (above/align "left"
                        (fn-for-widget--widget (first low) (+ layer 1))
                        (fn-for-widget--low (rest low) layer))]))
          
          (define (get-string w layer)
             (string-append (blanks (* layer TAB)) (widget-name w) " : "
               (number->string (widget-quantity w)) " @ $" (number->string (widget-price w))))

          ]
    
    [fn-for-widget--widget widg 0]))





