

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)

(define-struct widget(name quantity time price parts))


(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))

(define Telephone (make-widget "Telephone" 5 20 15 (list Receiver Buttons Cord)))


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




(define (make-text widget)
  (text (string-append (widget-name widget) " : " (number->string(widget-quantity widget)) " @ $" (number->string(widget-price widget))) TEXT-SIZE TEXT-COLOR)
  )

(define (make-text-color widget color)
  (text (string-append (widget-name widget) " : " (number->string(widget-quantity widget)) " @ $" (number->string(widget-price widget))) TEXT-SIZE color)
  )





(define (simple-render wdgt)
  (local [(define (render--widget widget)
            (above/align "left" (make-text widget) (render--low (widget-parts widget))
                )
            )
          (define (render--low low)
            (cond
              [(empty? low) empty-image]
              [else (above/align "left" (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (render--widget (first low)))
                           (render--low (rest low)))]
              )
            )
          ]
    (render--widget wdgt)
    )
  )

(check-expect (simple-render Telephone) .
)






(define (render wdgt operation)
  (local [(define (render--widget widget)
            (above/align "left" (make-text-color widget (operation widget)) (render--low (widget-parts widget))
                )
            )
          (define (render--low low)
            (cond
              [(empty? low) empty-image]
              [else (above/align "left" (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (render--widget (first low))) (render--low (rest low)))]
              )
            )
          ]
    (render--widget wdgt)
    )
  )






(define (quantity-5-10 widget)
  (cond
    [(< (widget-quantity widget) 5) "Red"]
    [(< (widget-quantity widget) 10) "Yellow"]
    [else "Black"]
    )
  )






(define (price-7-25 widget)
  (cond
    [(< (widget-price widget) 7) "Green"]
    [(> (widget-price widget) 25) "Red"]
    [else "Black"]
    )
  )








(define (quantity-count widget)
  (cond
    [(< (widget-quantity widget) 5) "Red"]
    [(< (widget-quantity widget) 7) "Orange"]
    [(< (widget-quantity widget) 10) "Yellow"]
    [(< (widget-quantity widget) 16) "Black"]
    [else "Green"]
    )
  )


(check-expect (render Jewelry quantity-5-10).)
(check-expect (render Jewelry price-7-25) .)

(check-expect (render Jewelry quantity-count).)
