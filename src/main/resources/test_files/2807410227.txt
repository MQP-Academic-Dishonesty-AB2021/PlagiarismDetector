

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

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")



 





(define (simple-render widget)
  (render widget (λ (widget) TEXT-COLOR)))

(check-expect
 (simple-render Jewelry) 
 
 (above/align
  "left"
  (text "Jewelry set : 4 @ $30" TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) "Rings : 15 @ $11")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) "Necklace : 10 @ $3")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) (blanks TAB) "Chain : 7 @ $1")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) (blanks TAB) "Pendant : 4 @ $1")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) "Bracelet : 5 @ $5")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) (blanks TAB) "Beads : 25 @ $7")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) (blanks TAB) (blanks TAB) "Glass : 6 @ $4")
        TEXT-SIZE TEXT-COLOR)))

(check-expect (simple-render Beads) 
              
              (above/align "left"
                           (text "Beads : 25 @ $7" TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks TAB) "Glass : 6 @ $4")
                                 TEXT-SIZE TEXT-COLOR)))

(check-expect (simple-render Glass) 
              (text "Glass : 6 @ $4" TEXT-SIZE TEXT-COLOR)) 






(define (render widget fn)
  (local
    [(define (describe-widget widget)
       (string-append
        (widget-name widget)
        " : "
        (number->string (widget-quantity widget))
        " @ $"
        (number->string (widget-price widget))))
     (define (render--inner widget)
       (above/align "left"
                    (text (describe-widget widget) TEXT-SIZE (fn widget))
                    (if (empty? (widget-parts widget)) empty-image
                        (beside
                         (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                         (render--low (widget-parts widget))))))
     (define (render--low low)
       (cond
         [(empty? low)
          empty-image]
         [else
          (above/align "left"
                       (render (first low) fn)
                       (render--low (rest low)))]))]
    (render--inner widget)))



(define (lowStock widget)
  (cond [(< (widget-quantity widget) 5) "red"]
        [(< (widget-quantity widget) 10) "yellow"]
        [else TEXT-COLOR]))



(define (extremePrice widget)
  (cond [(< (widget-price widget) 7) "green"]
        [(> (widget-price widget) 25) "red"]
        [else TEXT-COLOR]))




(check-expect
 (render Jewelry lowStock)
 (above/align
  "left"
  (text "Jewelry set : 4 @ $30" TEXT-SIZE "red")
  (text (string-append (blanks TAB) "Rings : 15 @ $11")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) "Necklace : 10 @ $3")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) (blanks TAB) "Chain : 7 @ $1")
        TEXT-SIZE "yellow")
  (text (string-append (blanks TAB) (blanks TAB) "Pendant : 4 @ $1")
        TEXT-SIZE "red")
  (text (string-append (blanks TAB) "Bracelet : 5 @ $5")
        TEXT-SIZE "yellow")
  (text (string-append (blanks TAB) (blanks TAB) "Beads : 25 @ $7")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) (blanks TAB) (blanks TAB) "Glass : 6 @ $4")
        TEXT-SIZE "yellow")))



(check-expect
 (render Jewelry extremePrice)
 (above/align
  "left"
  (text "Jewelry set : 4 @ $30" TEXT-SIZE "red")
  (text (string-append (blanks TAB) "Rings : 15 @ $11") TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) "Necklace : 10 @ $3") TEXT-SIZE "green")
  (text (string-append (blanks TAB) (blanks TAB) "Chain : 7 @ $1")
        TEXT-SIZE "green")
  (text (string-append (blanks TAB) (blanks TAB) "Pendant : 4 @ $1")
        TEXT-SIZE "green")
  (text (string-append (blanks TAB) "Bracelet : 5 @ $5") TEXT-SIZE "green")
  (text (string-append (blanks TAB) (blanks TAB) "Beads : 25 @ $7")
        TEXT-SIZE TEXT-COLOR)
  (text (string-append (blanks TAB) (blanks TAB) (blanks TAB) "Glass : 6 @ $4")
        TEXT-SIZE "green")))



(check-expect (render Beads lowStock)
              (above/align "left"
                           (text "Beads : 25 @ $7" TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks TAB) "Glass : 6 @ $4")
                                 TEXT-SIZE "yellow")))



(check-expect (render Glass extremePrice)
              (text "Glass : 6 @ $4" TEXT-SIZE "green")) 