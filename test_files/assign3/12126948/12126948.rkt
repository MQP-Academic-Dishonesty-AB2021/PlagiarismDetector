

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment3-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 
(define HEIGHT 400)
(define WIDTH 400)
(define MTS (empty-scene WIDTH HEIGHT))
(define X-POS (/ WIDTH 2))
(define Y-POS (/ HEIGHT 2))
(define COLON " : ")
(define AT-DOLLAR " @ $")





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")

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
                             
 

 





(define (widget-label w tabs) 
  (string-append
   (blanks (* tabs TAB))
   (widget-name w)
   COLON
   (number->string (widget-quantity w))
   AT-DOLLAR
   (number->string (widget-price w))))


(check-expect (widget-label Rings 0)
              "Rings : 15 @ $11")

(check-expect (widget-label Chain 1)
              (string-append (blanks TAB) "Chain : 7 @ $1"))

(check-expect (widget-label Glass 2)
              (string-append (blanks (* 2 TAB)) "Glass : 6 @ $4"))




(define (render w fn)
  (local [
          
          (define (render--widget w depth)
            (above/align
             "left"
             (text
              (widget-label w depth)
              TEXT-SIZE (fn w))
             (render--low (widget-parts w) (+ 1 depth))))
             
          
          (define (render--low low depth)
            (cond [(empty? low) empty-image]
                  [else
                   (above/align "left"
                                (render--widget (first low) depth)
                                (render--low (rest low) depth))]))]
    (render--widget w 0)))




(define (simple-render w)
  (render w (lambda (x) "black")))


(check-expect (simple-render Glass)
              (text (widget-label Glass 0) TEXT-SIZE "black"))


(check-expect (simple-render Necklace)
              (above/align "left"
                           (text (widget-label Necklace 0) TEXT-SIZE "black")
                           (text (widget-label Chain 1) TEXT-SIZE "black")
                           (text (widget-label Pendant 1) TEXT-SIZE "black")))


(check-expect (simple-render Bracelet)
              (above/align "left"
                           (text (widget-label Bracelet 0) TEXT-SIZE "black")
                           (text (widget-label Beads 1) TEXT-SIZE "black")
                           (text (widget-label Glass 2) TEXT-SIZE "black")))






(define (price-render w)
  (render w (lambda (x)
              (cond [(< (widget-price x) 7) "green"]
                    [(> (widget-price x) 25) "red"]
                    [else "black"]))))

(check-expect (price-render Wire)
              (text
               (widget-label Wire 0)
               TEXT-SIZE "green"))

(check-expect (price-render Bracelet)
              (above/align
               "left"
               (text
                (widget-label Bracelet 0)
                TEXT-SIZE "green")
               (text
                (widget-label Beads 1)
                TEXT-SIZE "black")
               (text
                (widget-label Glass 2)
                TEXT-SIZE "green")))




(check-expect (price-render Jewelry)
              (above/align
               "left"
               (text
                (widget-label Jewelry 0)
                TEXT-SIZE "red")
                (text
                (widget-label Rings 1)
                TEXT-SIZE "black")
                (text
                (widget-label Necklace 1)
                TEXT-SIZE "green")
                (text
                (widget-label Chain 2)
                TEXT-SIZE "green")
                (text
                (widget-label Pendant 2)
                TEXT-SIZE "green")
                (text
                (widget-label Bracelet 1)
                TEXT-SIZE "green")
               (text
                (widget-label Beads 2)
                TEXT-SIZE "black")
               (text
                (widget-label Glass 3)
                TEXT-SIZE "green")))













(define (stock-render w)
  (render w (lambda (x)
              (cond [(< (widget-quantity x) 5) "red"]
                    [(< (widget-quantity x) 10) "yellow"]
                    [else "black"]))))


(check-expect (stock-render Glass)
              (text (widget-label Glass 0) TEXT-SIZE "yellow"))


(check-expect (stock-render Necklace)
              (above/align "left"
                           (text (widget-label Necklace 0) TEXT-SIZE "black")
                           (text (widget-label Chain 1) TEXT-SIZE "yellow")
                           (text (widget-label Pendant 1) TEXT-SIZE "red")))


(check-expect (stock-render Bracelet)
              (above/align "left"
                           (text (widget-label Bracelet 0) TEXT-SIZE "yellow")
                           (text (widget-label Beads 1) TEXT-SIZE "black")
                           (text (widget-label Glass 2) TEXT-SIZE "yellow")))




(define (rainbow-render w)
  (render w (lambda (x) (make-color (random 255)
                                    (random 255)
                                    (random 255)))))