

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







 


(define (simple-render awidget)
  (render awidget (λ (widget) TEXT-COLOR)))

(check-expect (simple-render Wire)
              (text (string-append
                     (widget-name Wire) " : "
                     (number->string (widget-quantity Wire)) " @ $"
                     (number->string (widget-price Wire)))
                    TEXT-SIZE TEXT-COLOR))       
(check-expect (simple-render Cord)
              (text (string-append
                     (widget-name Cord) " : "
                     (number->string (widget-quantity Cord)) " @ $"
                     (number->string (widget-price Cord))
                     "\n" (blanks TAB)
                     (widget-name Wire) " : "
                     (number->string (widget-quantity Wire)) " @ $"
                     (number->string (widget-price Wire)))
                    TEXT-SIZE TEXT-COLOR))        

(check-expect (simple-render Necklace)
              (text (string-append
                     (widget-name Necklace) " : "
                     (number->string (widget-quantity Necklace)) " @ $"
                     (number->string (widget-price Necklace))

                     "\n" (blanks TAB)
                     (widget-name Chain) " : "
                     (number->string (widget-quantity Chain)) " @ $"
                     (number->string (widget-price Chain))

                     "\n" (blanks TAB)
                     (widget-name Pendant) " : "
                     (number->string (widget-quantity Pendant)) " @ $"
                     (number->string (widget-price Pendant))
                     )
                    TEXT-SIZE TEXT-COLOR))       

(check-expect (simple-render Bracelet)
              (text (string-append
                     (widget-name Bracelet) " : "
                     (number->string (widget-quantity Bracelet)) " @ $"
                     (number->string (widget-price Bracelet))

                     "\n" (blanks TAB)
                     (widget-name Beads) " : "
                     (number->string (widget-quantity Beads)) " @ $"
                     (number->string (widget-price Beads))

                     "\n" (blanks (* 2 TAB))
                     (widget-name Glass) " : "
                     (number->string (widget-quantity Glass)) " @ $"
                     (number->string (widget-price Glass))
                     )
                    TEXT-SIZE TEXT-COLOR))       









(define (render awidget color-fn)
  (local [(define (fn-for-widget--widget awidget depth)
            (above/align "left"
                         (text (string-append
                                (blanks (* depth TAB))
                                (widget-name awidget)
                                " : "
                                (number->string (widget-quantity awidget))
                                " @ $"
                                (number->string (widget-price awidget)))
                               TEXT-SIZE (color-fn awidget))
                         (fn-for-widget--low (widget-parts awidget)
                                             (add1 depth))))

          (define (fn-for-widget--low alow depth)
            (cond [(empty? alow) (square 0 "solid" "white")]
                  [else (above/align "left"
                                     (fn-for-widget--widget (first alow) depth)
                                     (fn-for-widget--low (rest alow) depth))]))]
    (fn-for-widget--widget awidget 0)))


(check-expect (render Wire (λ (awidget) "red"))
              (text (string-append
                     (widget-name Wire) " : "
                     (number->string (widget-quantity Wire)) " @ $"
                     (number->string (widget-price Wire)))
                    TEXT-SIZE "red"))       

(check-expect (render Necklace (λ (awidget) (if (< (widget-quantity awidget) 5)
                                                "red"
                                                "white")))
              (above/align
               "left"
               (text (string-append
                      (widget-name Necklace) " : "
                      (number->string (widget-quantity Necklace)) " @ $"
                      (number->string (widget-price Necklace)))
                     TEXT-SIZE "white")
               (text (string-append
                      (blanks TAB)
                      (widget-name Chain) " : "
                      (number->string (widget-quantity Chain)) " @ $"
                      (number->string (widget-price Chain)))
                     TEXT-SIZE "white")
               (text (string-append
                      (blanks TAB)
                      (widget-name Pendant) " : "
                      (number->string (widget-quantity Pendant)) " @ $"
                      (number->string (widget-price Pendant)))
                     TEXT-SIZE "red")))     

(check-expect (render Bracelet (λ (awidget)
                                 (cond [(< (widget-price awidget) 5) "red"]
                                       [(> (widget-price awidget) 5) "green"]
                                       [else "white"])))
              (above/align
               "left"
               (text (string-append
                      (widget-name Bracelet) " : "
                      (number->string (widget-quantity Bracelet)) " @ $"
                      (number->string (widget-price Bracelet)))
                     TEXT-SIZE "white")
               (text (string-append
                      (blanks TAB)
                      (widget-name Beads) " : "
                      (number->string (widget-quantity Beads)) " @ $"
                      (number->string (widget-price Beads)))
                     TEXT-SIZE "green")
               (text (string-append
                      (blanks (* 2 TAB))
                      (widget-name Glass) " : "
                      (number->string (widget-quantity Glass)) " @ $"
                      (number->string (widget-price Glass)))
                     TEXT-SIZE "red")))       

(check-expect (render Bracelet (λ (awidget)
                                 (cond
                                   [(< (widget-quantity awidget) 5) "red"]
                                   [(< (widget-quantity awidget) 10) "yellow"]
                                   [else TEXT-COLOR])))
              (above/align
               "left"
               (text (string-append
                      (widget-name Bracelet) " : "
                      (number->string (widget-quantity Bracelet)) " @ $"
                      (number->string (widget-price Bracelet)))
                     TEXT-SIZE "yellow")
               (text (string-append
                      (blanks TAB)
                      (widget-name Beads) " : "
                      (number->string (widget-quantity Beads)) " @ $"
                      (number->string (widget-price Beads)))
                     TEXT-SIZE TEXT-COLOR)
               (text (string-append
                      (blanks (* 2 TAB))
                      (widget-name Glass) " : "
                      (number->string (widget-quantity Glass)) " @ $"
                      (number->string (widget-price Glass)))
                     TEXT-SIZE "yellow")))