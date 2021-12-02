

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")

(require 2htdp/universe)




(define WIDTH  500)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))



(define-struct ws (mainWidget colorFunction))




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









(define fn (λ (widget) (cond
                            [(< (widget-price widget) 7) "green"]
                            [(> (widget-price widget) 25) "red"]
                            [else "black"]
                            )
                )
      )


(define (main mainWidget fn?) 
  (local [(define ws (make-ws mainWidget fn?))]
  (big-bang ws                   
    (to-draw setRender)   
    )))



(define (setRender ws)
  (render (ws-mainWidget ws) (ws-colorFunction ws))
  )











(check-expect (simple-render Jewelry).)

(check-expect (simple-render Telephone). )


(define (simple-render mainWidget)
  (render mainWidget (λ (n) "black"))
  )







(check-expect (render Jewelry (λ (widget) (cond
                            [(< (widget-price widget) 7) "green"]
                            [(> (widget-price widget) 25) "red"]
                            [else "black"]
                            )
                ))
              (place-image .
                           (/ WIDTH 2) (/ WIDTH 2) MTS)
              )


(check-expect (render Jewelry (λ (widget) (cond
                            [(< (widget-quantity widget) 5) "red"]
                            [(< (widget-quantity widget) 10) "yellow"]
                            [else "black"]
                            )
                ))
              (place-image .
                           (/ WIDTH 2) (/ WIDTH 2) MTS)
              )

(define (render mainWidget fn?)
  (place-image (generateColorTree mainWidget fn? 0) (/ WIDTH 2) (/ WIDTH 2) MTS)
  )





(check-expect (generateColorTree Jewelry (λ (widget) (cond
                            [(< (widget-price widget) 7) "green"]
                            [(> (widget-price widget) 25) "red"]
                            [else "black"]
                            )
                )0).)

(check-expect (generateColorTree Jewelry (λ (widget) (cond
                            [(< (widget-quantity widget) 5) "red"]
                            [(< (widget-quantity widget) 10) "yellow"]
                            [else "black"]
                            )
                )0) .)



(define (generateColorTree mainWidget fn? spaces)
  (local[
         (define (look-in-Widget widget spaces)
           (above/align "left"
            (text
             (string-append
              (blanks spaces)
              (widget-name widget)
              " : "
              (number->string(widget-quantity widget))
              " @ $"
              (number->string(widget-price widget))
              
              )
             24
             (fn? widget)
             )
            (look-in-subs (widget-parts widget)(+ spaces 4)))
           )
         (define (look-in-subs low spaces)
           (cond
             [(empty? low) (text "" 1 "white")]
             [else
              (above/align "left"
               (look-in-Widget(first low ) spaces)
               (look-in-subs (rest low )spaces))])
           )
         ]
    (look-in-Widget mainWidget spaces)
    )
  ) 


