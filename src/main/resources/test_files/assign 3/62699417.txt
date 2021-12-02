

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3_DanielB_AlexM) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "BLACK")  
(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")

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




(check-expect(simple-render Jewelry) sample) 
(check-expect(simple-render Glass) (format-output Glass 0)) 

                                

 


(define (simple-render widget)
  (render widget (λ (widget) TEXT-COLOR)))

(define (format-output widget depth)
  (text (string-append (blanks (* depth TAB)) (widget-name widget) ": " (number->string (widget-quantity widget)) " @ $" (number->string (widget-price widget))) TEXT-SIZE TEXT-COLOR))
  

(define sample       (above/align "left"
                            (format-output Jewelry 0)
                            (format-output Rings 1)
                            (format-output Necklace 1)
                            (format-output Chain 2)
                            (format-output Pendant 2)
                            (format-output Bracelet 1)
                            (format-output Beads 2)
                            (format-output Glass 3)
                                 ))









(check-expect (render Jewelry costs-more-than-7) (above/align "left"
                                                              (format-output-colored Jewelry 0 (costs-more-than-7 Jewelry))
                                                              (format-output-colored Rings 1 (costs-more-than-7 Rings))
                                                              (format-output-colored Necklace 1 (costs-more-than-7 Necklace))
                                                              (format-output-colored Chain 2 (costs-more-than-7 Chain))
                                                              (format-output-colored Pendant 2 (costs-more-than-7 Pendant))
                                                              (format-output-colored Bracelet 1 (costs-more-than-7 Bracelet))
                                                              (format-output-colored Beads 2 (costs-more-than-7 Beads))
                                                              (format-output-colored Glass 3 (costs-more-than-7 Glass)))) 

(check-expect (render Glass combined) (format-output-colored Glass 0 (combined Glass))) 


(define (render widget fn)
  (local [
          (define (fn-for-list-of-widget low depth) 
            (cond [(empty? low) empty-image]
                  [else  (above/align "left" (format-output-colored (first low) depth (fn (first low)))
                                (fn-for-list-of-widget (widget-parts (first low)) (+ 1 depth))
                                (fn-for-list-of-widget (rest low) depth)
                              
                              )
                         ]
                  )
            )]
    (above/align "left" (format-output-colored widget 0 (fn widget)) (fn-for-list-of-widget (widget-parts widget) 1)
    )
  )
  )

(define (costs-more-than-7 widget)
  (if (> (widget-price widget) 7)
      "RED"
      "WHITE"
      )
  )
(define (fewer-than-6-in-stock widget)
  (if (< (widget-quantity widget) 6)
      "ORANGE"
      "WHITE"
      )
  )
(define (combined widget)
         (if (> (widget-price widget) 7)
      "RED"
      (if (< (widget-quantity widget) 6)
      "ORANGE"
      "WHITE"
      )
      )
  )

(define (format-output-colored widget depth color)
  (text (string-append (blanks (* depth TAB)) (widget-name widget) ": " (number->string(widget-quantity widget)) " @ $" (number->string(widget-price widget))) TEXT-SIZE color))

(define (render-test-color-based-on-quantity widget) 
  (render widget (λ (widget)
                   (cond [(<= (widget-quantity widget) 5) "RED"]
                         [(<= (widget-quantity widget) 10) "YELLOW"]
                         [else TEXT-COLOR]
                    )
                  )
          )
  )
(define (render-test-color-based-on-price widget) 
  (render widget (λ (widget)
                   (cond [(<= (widget-price widget) 5) "GREEN"]
                         [(>= (widget-price widget) 15) "RED"]
                         [else TEXT-COLOR]
                    )
                  )
          )
  )
(render-test-color-based-on-price Jewelry) 