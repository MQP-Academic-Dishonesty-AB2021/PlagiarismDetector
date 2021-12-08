

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 


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










(check-expect (blanks 0) "")
(check-expect (blanks 3) "   ")
(check-expect (blanks TAB) "     ")


(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))













(check-expect (simple-render Wire)
              (text "Wire : 3 @ $5" TEXT-SIZE TEXT-COLOR))


(check-expect (simple-render Jewelry) 
              (above/align "left"
                           (text "Jewelry set : 4 @ $30" TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks TAB) "Rings : 15 @ $11")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks TAB) "Necklace : 10 @ $3")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks (* TAB 2)) "Chain : 7 @ $1")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks (* TAB 2)) "Pendant : 4 @ $1")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks TAB) "Bracelet : 5 @ $5")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks (* TAB 2)) "Beads : 25 @ $7")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks (* TAB 3)) "Glass : 6 @ $4")
                                 TEXT-SIZE
                                 TEXT-COLOR)))


(check-expect (simple-render Buttons) 
              (above/align "left"
                           (text "Buttons : 8 @ $5" TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks TAB) "Numbers : 9 @ $5")
                                 TEXT-SIZE
                                 TEXT-COLOR)))


(define (simple-render widget)
  (local [(define (render-widgets widgets level)
            (cond
              [(empty? widgets) empty-image]
              [else (above/align
                     "left"
                     (text (string-append (blanks (* TAB level))
                                          (widget-name (first widgets))
                                          " : "
                                          (number->string (widget-quantity (first widgets)))
                                          " @ $"
                                          (number->string (widget-price (first widgets))))
                           TEXT-SIZE
                           TEXT-COLOR)
                     (render-widgets (widget-parts (first widgets)) (+ level 1))
                     (render-widgets (rest widgets) level))]))
          (define (render-widget--widget widget level-acc)
            (render-widgets (list widget) level-acc))]
    (render-widget--widget widget 0)))
















(check-expect
 (render Numbers (λ (widget) TEXT-COLOR)) 
 (text "Numbers : 9 @ $5" TEXT-SIZE TEXT-COLOR))



(check-expect (render Telephone (λ (widget)
                                  (cond [(< (widget-quantity widget) 5) "red"]
                                        [(< (widget-quantity widget) 10) "yellow"]
                                        [else TEXT-COLOR])))
              
              (above/align "left"
                           (text "Telephone : 5 @ $15" TEXT-SIZE "yellow")
                           (text (string-append (blanks TAB) "Receiver : 10 @ $7")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks TAB) "Buttons : 8 @ $5")
                                 TEXT-SIZE
                                 "yellow")
                           (text (string-append (blanks (* TAB 2)) "Numbers : 9 @ $5")
                                 TEXT-SIZE
                                 "yellow")
                           (text (string-append (blanks TAB) "Cord : 7 @ $5")
                                 TEXT-SIZE
                                 "yellow")
                           (text (string-append (blanks (* TAB 2)) "Wire : 3 @ $5")
                                 TEXT-SIZE
                                 "red")))



(check-expect (render Jewelry (λ (widget)
                                (cond [(< (widget-price widget) 7) "green"]
                                      [(> (widget-price widget) 25) "red"]
                                      [else TEXT-COLOR])))
              (above/align "left"
                           (text "Jewelry set : 4 @ $30" TEXT-SIZE "red")
                           (text (string-append (blanks TAB) "Rings : 15 @ $11")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks TAB) "Necklace : 10 @ $3")
                                 TEXT-SIZE
                                 "green")
                           (text (string-append (blanks (* TAB 2)) "Chain : 7 @ $1")
                                 TEXT-SIZE
                                 "green")
                           (text (string-append (blanks (* TAB 2)) "Pendant : 4 @ $1")
                                 TEXT-SIZE
                                 "green")
                           (text (string-append (blanks TAB) "Bracelet : 5 @ $5")
                                 TEXT-SIZE
                                 "green")
                           (text (string-append (blanks (* TAB 2)) "Beads : 25 @ $7")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks (* TAB 3)) "Glass : 6 @ $4")
                                 TEXT-SIZE
                                 "green")))


(check-expect (render Buttons (λ (widget) "purple")) 
              (above/align "left"
                           (text "Buttons : 8 @ $5" TEXT-SIZE "purple")
                           (text (string-append (blanks TAB) "Numbers : 9 @ $5")
                                 TEXT-SIZE
                                 "purple")))


(define (render widget color-fn)
  (local [(define (render-widgets widgets level)
            (cond
              [(empty? widgets) empty-image]
              [else (above/align
                     "left"
                     (text (string-append (blanks (* TAB level))
                                          (widget-name (first widgets))
                                          " : "
                                          (number->string (widget-quantity (first widgets)))
                                          " @ $"
                                          (number->string (widget-price (first widgets))))
                           TEXT-SIZE
                           (color-fn (first widgets)))
                     (render-widgets (widget-parts (first widgets)) (+ level 1))
                     (render-widgets (rest widgets) level))]))]
    (render-widgets (list widget) 0)))














(check-expect (simple-render2 Wire) 
              (text "Wire : 3 @ $5" TEXT-SIZE TEXT-COLOR))


(check-expect (simple-render2 Jewelry)
              (above/align "left"
                           (text "Jewelry set : 4 @ $30" TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks TAB) "Rings : 15 @ $11")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks TAB) "Necklace : 10 @ $3")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks (* TAB 2)) "Chain : 7 @ $1")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks (* TAB 2)) "Pendant : 4 @ $1")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks TAB) "Bracelet : 5 @ $5")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks (* TAB 2)) "Beads : 25 @ $7")
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (text (string-append (blanks (* TAB 3)) "Glass : 6 @ $4")
                                 TEXT-SIZE
                                 TEXT-COLOR)))


(check-expect (simple-render2 Buttons) 
              (above/align "left"
                           (text "Buttons : 8 @ $5" TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks TAB) "Numbers : 9 @ $5")
                                 TEXT-SIZE
                                 TEXT-COLOR)))


(define (simple-render2 widget)
  (render widget (λ (widget) TEXT-COLOR)))