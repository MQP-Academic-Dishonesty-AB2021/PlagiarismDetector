

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  
                             
(define TAB 5) 





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



(define (fn-for-widget widget)
  (local [
          (define (fn-for-widget widget)
            (... (widget-name widget)  
                 (widget-quantity widget)
                 (widget-time widget)
                 (widget-price widget)
                 (fn-for-Low (widget-parts widget))
                 ))

          (define (fn-for-Low Low)
            (cond [(empty? Low) empty]
                  [else
                   (... (fn-for-widget (first Low))
                        (fn-for-Low (rest Low)))]))]

    (fn-for-widget widget)))






(check-expect (simple-render Wire) 
              (text (string-append (blanks 0) (widget-name Wire) " : " 
                 (number->string (widget-quantity Wire)) " @ "
                 (number->string (widget-price Wire))) TEXT-SIZE TEXT-COLOR)
                 )
(check-expect (simple-render Buttons) 
              (above/align "left" (text (string-append (blanks 0) (widget-name Buttons) " : " 
                 (number->string (widget-quantity Buttons)) " @ "
                 (number->string (widget-price Buttons))) TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks 5) (widget-name Numbers) " : " 
                 (number->string (widget-quantity Numbers)) " @ "
                 (number->string (widget-price Numbers))) TEXT-SIZE TEXT-COLOR)
                 ))


 





(check-expect (render Wire (lambda (Wire)
                  (if (< (widget-quantity Wire) 5)
                       "Red"
                       (if (< (widget-quantity Wire) 10)
                           "Yellow"
                           TEXT-COLOR))))
              (text (string-append (blanks 0) (widget-name Wire) " : " 
                 (number->string (widget-quantity Wire)) " @ "
                 (number->string (widget-price Wire))) TEXT-SIZE ((lambda (Wire)
                  (if (< (widget-quantity Wire) 5)
                       "Red"
                       (if (< (widget-quantity Wire) 10)
                           "Yellow"
                           TEXT-COLOR))) Wire)))


(check-expect (render Beads (lambda (Beads)
                  (if (< (widget-quantity Beads) 5)
                       "Red"
                       (if (< (widget-quantity Beads) 10)
                           "Yellow"
                           TEXT-COLOR))))
              (above/align "left"(text (string-append (blanks 0) (widget-name Beads) " : " 
                 (number->string (widget-quantity Beads)) " @ "
                 (number->string (widget-price Beads))) TEXT-SIZE ((lambda (Beads)
                  (if (< (widget-quantity Beads) 5)
                       "Red"
                       (if (< (widget-quantity Beads) 10)
                           "Yellow"
                           TEXT-COLOR))) Beads))
              (text (string-append (blanks 5) (widget-name Glass) " : " 
                 (number->string (widget-quantity Glass)) " @ "
                 (number->string (widget-price Glass))) TEXT-SIZE ((lambda (Glass)
                  (if (< (widget-quantity Glass) 5)
                       "Red"
                       (if (< (widget-quantity Glass) 10)
                           "Yellow"
                           TEXT-COLOR))) Glass))))


(define (render widget color-from-widget)
  (local [
          (define (render widget tabs)
             (above/align "left" (text (string-append (blanks (* tabs TAB)) (widget-name widget) " : " 
                 (number->string (widget-quantity widget)) " @ "
                 (number->string (widget-price widget))) TEXT-SIZE (color-from-widget widget))
                  (render-Low (widget-parts widget) (+ tabs 1))
                 ))

          (define (render-Low Low tabs)
            (cond [(empty? Low) (square 0 "solid" "white")]
                  [else
                   (above/align "left" (render (first Low) tabs)
                        (render-Low (rest Low) tabs))]))]

    (render widget 0)))


(define (simple-render widget)
   (render widget (lambda (widget) TEXT-COLOR))
)


(define (test1 widget)
(render widget (lambda (widget)
                  (if (< (widget-quantity widget) 5)
                       "Red"
                       (if (< (widget-quantity widget) 10)
                           "Yellow"
                           TEXT-COLOR)))))



(define (test2 widget)
(render widget (lambda (widget)
                  (if (> (widget-price widget) 20)
                       "Blue"
                       (if (< (widget-quantity widget) 10)
                           "Green"
                           TEXT-COLOR)))))
                 