






(require 2htdp/image)
(require 2htdp/universe)

(define-struct widget(name quantity time price parts))

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

(define WIDTH 600)
(define HEIGHT 800)
(define MTS (empty-scene WIDTH HEIGHT))
(define BLANK (square 0 "outline" "white"))
(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 

(define START Jewelry)





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")

(check-expect (main Glass) Glass)    
(check-expect (main Jewelry) Jewelry)

(define (main tree)
  (local [(define (render-text state) (render state color-fn))]
  (big-bang tree
    (to-draw render-text)
    )
  )
  )
















(check-expect (simple-render Glass)    
              (overlay
               (above/align
                "left"
                (text
                 (string-append
                  (blanks 0)
                  (widget-name Glass)
                  " : "
                  (number->string (widget-quantity Glass))
                  " @ $"
                  (number->string (widget-price Glass)))
                 TEXT-SIZE TEXT-COLOR)
                BLANK)
               MTS))

(check-expect (simple-render Beads) 
              (overlay
               (above/align
                "left"
                (text
                 (string-append
                  (blanks 0)
                  (widget-name Beads)
                  " : "
                  (number->string (widget-quantity Beads))
                  " @ $"
                  (number->string (widget-price Beads)))
                 TEXT-SIZE TEXT-COLOR)
                (above/align
                 "left"
                 (text
                  (string-append
                   (blanks 4)
                   (widget-name Glass)
                   " : "
                   (number->string (widget-quantity Glass))
                   " @ $"
                   (number->string (widget-price Glass)))
                  TEXT-SIZE TEXT-COLOR)
                 BLANK))
               MTS))



(define (simple-render widget)
  
  

  
  (overlay (get-text-list widget 0 (lambda (color) "black")) MTS)
)

(check-expect (get-text-list-simple Glass 0)    
              
              (above/align
               "left"
               (text
                (string-append
                 (blanks 0)
                 (widget-name Glass)
                 " : "
                 (number->string (widget-quantity Glass))
                 " @ $"
                 (number->string (widget-price Glass)))
                TEXT-SIZE TEXT-COLOR)
               BLANK)
              )


(check-expect (get-text-list-simple Beads 0) 
              
              (above/align
               "left"
               (text
                (string-append
                 (blanks 0)
                 (widget-name Beads)
                 " : "
                 (number->string (widget-quantity Beads))
                 " @ $"
                 (number->string (widget-price Beads)))
                TEXT-SIZE TEXT-COLOR)
               (above/align
                "left"
                (text
                 (string-append
                  (blanks 4)
                  (widget-name Glass)
                  " : "
                  (number->string (widget-quantity Glass))
                  " @ $"
                  (number->string (widget-price Glass)))
                 TEXT-SIZE TEXT-COLOR)
                BLANK)
               BLANK)
              )



(define (get-text-list-simple widget tabs)
  (local [(define (widget-subs widgets tabs)
            (cond
              [(empty? widgets) BLANK]
              [else
               (above/align "left" (get-text-list-simple (first widgets) tabs)
      
                            
                            (widget-subs (rest widgets) tabs))]))]
    (above/align "left"
                 (text (string-append (blanks tabs) (widget-name widget) " : " (number->string (widget-quantity widget)) " @ $" (number->string (widget-price widget))) TEXT-SIZE TEXT-COLOR)
                 (widget-subs (widget-parts widget) (+ tabs 4))
                 )
    )

  
  
  
  )

(check-expect (color-fn Beads) "black") 
(check-expect (color-fn Jewelry) "red") 
(check-expect (color-fn Glass) "green") 



(define (color-fn widget)
  (cond [(< (widget-price widget) 7) "green"]
        [(> (widget-price widget) 25) "red"]
        [else "black"]
  )
)

(check-expect (render Glass (lambda (widget)
                                       "black")) (overlay (get-text-list Glass 0 (lambda (widget)
                                       "black")) MTS)) 

(check-expect (render Jewelry (lambda (widget)
                                       "black")) (overlay (get-text-list Jewelry 0 (lambda (widget)
                                       "black")) MTS))

(check-expect (render Jewelry (lambda (widget)
                                       "black")) (overlay (get-text-list Jewelry 0 (lambda (widget)
                                       "black")) MTS))



(define (render widget fn)
  (overlay (get-text-list widget 0 fn) MTS)
)

(check-expect (get-text-list Glass 0 (lambda (widget)
                                       "black"))    
              
              (above/align
               "left"
               (text
                (string-append
                 (blanks 0)
                 (widget-name Glass)
                 " : "
                 (number->string (widget-quantity Glass))
                 " @ $"
                 (number->string (widget-price Glass)))
                TEXT-SIZE TEXT-COLOR)
               BLANK)
              )


(check-expect (get-text-list Beads 0 (lambda (widget)
                                       (if (< (widget-quantity widget) 7)
                                           "red"
                                           "black"))) 
              
              (above/align
               "left"
               (text
                (string-append
                 (blanks 0)
                 (widget-name Beads)
                 " : "
                 (number->string (widget-quantity Beads))
                 " @ $"
                 (number->string (widget-price Beads)))
                TEXT-SIZE "black")
               (above/align
                "left"
                (text
                 (string-append
                  (blanks 4)
                  (widget-name Glass)
                  " : "
                  (number->string (widget-quantity Glass))
                  " @ $"
                  (number->string (widget-price Glass)))
                 TEXT-SIZE "red")
                BLANK)
               BLANK)
              )



(define (get-text-list widget tabs fn)
(local [(define (widget-subs widgets tabs)
          (cond
            [(empty? widgets) BLANK]
            [else
              (above/align "left" (get-text-list (first widgets) tabs fn)
                          (widget-subs (rest widgets) tabs))]))]
  (above/align "left"
                (text (string-append (blanks tabs) (widget-name widget) " : " (number->string (widget-quantity widget)) " @ $" (number->string (widget-price widget))) TEXT-SIZE (fn widget))
                (widget-subs (widget-parts widget) (+ tabs 4))
                )
  )
)








