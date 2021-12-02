

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 

(define WIDTH 800)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))

(define-struct widget(name quantity time price parts))











(define (fn-for-widget widget)
  ( ... (widget-name widget)
        (widget-quantity widget)
        (widget-time widget)
        (widget-price widget)
        (local [(define (fn-for-low low)
                  (cond [(empty? low) ...]
                        [else
                         ... (fn-for-widget (first low))
                         (fn-for-low (rest low))]))]
          (fn-for-low (widget-parts widget)))))



(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 10 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 9 empty))
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






(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")







(check-expect (simple-render Receiver)
              (place-image (text
                            (string-append
                             (widget-name Receiver) " : "
                             (number->string(widget-quantity Receiver)) " @ $"
                             (number->string(widget-price Receiver)))
                            TEXT-SIZE TEXT-COLOR)
                           200 200 MTS))


(check-expect (simple-render Buttons)
              (place-image
               (above/align "left"
                            (text
                             (string-append
                              (widget-name Buttons) " : "
                              (number->string(widget-quantity Buttons)) " @ $"
                              (number->string(widget-price Buttons))) TEXT-SIZE TEXT-COLOR)
                            (text
                             (string-append
                              (blanks TAB) (widget-name Numbers) " : "
                              (number->string(widget-quantity Numbers)) " @ $"
                              (number->string(widget-price Numbers)))
                             TEXT-SIZE TEXT-COLOR)) 200 200 MTS))




(define (simple-render0 widget)
  
  (local [(define (simple-render1 widget indent)
            (above
             (text 
              (widget->text widget indent) TEXT-SIZE TEXT-COLOR)
             (simple-render1-low (widget-parts widget) (add1 indent))))
                        
          
          (define (simple-render1-low low indent)
            (cond [(empty? low) (square 0 "outline" "white")]
                  [else
                   (above
                    (simple-render1 (first low) indent)
                    (simple-render1-low (rest low) (+ indent 1)))
                   ]))]

    
    (place-image (simple-render1 widget 0) 200 200 MTS)))


(define (simple-render widget)
  (render widget (λ (w) (identity "black"))))







(check-expect (widget->text Receiver 0) (string-append
                                         (widget-name Receiver) " : "
                                         (number->string(widget-quantity Receiver)) " @ $"
                                         (number->string(widget-price Receiver))))
(check-expect (widget->text Buttons 1) (string-append
                                        (blanks (* TAB 1))
                                        (widget-name Buttons) " : "
                                        (number->string(widget-quantity Buttons)) " @ $"
                                        (number->string(widget-price Buttons))))
(define (widget->text widget indent)
  (string-append (blanks (* TAB indent)) (widget-name widget) " : "
                 (number->string(widget-quantity widget)) " @ $"
                 (number->string(widget-price widget))))














(check-expect (render Receiver (λ (w) (if ( <(widget-price w) 10)
                                          "green"
                                          "black")))
              (place-image (text
                            (string-append
                             (widget-name Receiver) " : "
                             (number->string(widget-quantity Receiver)) " @ $"
                             (number->string(widget-price Receiver))) TEXT-SIZE "green")
                           200 200 MTS))


(check-expect (render Buttons (λ (w) (if (<=(widget-quantity w) 8)
                                         "green"
                                         "black")))
              (place-image (above/align "left" (text
                                                (string-append
                                                 (widget-name Buttons) " : "
                                                 (number->string
                                                  (widget-quantity Buttons))
                                                 " @ $"
                                                 (number->string
                                                  (widget-price Buttons)))
                                                TEXT-SIZE
                                                "green")
                                        (text
                                         (string-append
                                          (blanks TAB) (widget-name Numbers) " : "
                                          (number->string(widget-quantity Numbers)) " @ $"
                                          (number->string(widget-price Numbers)))
                                         TEXT-SIZE
                                         "black"))
                           200 200 MTS))
 


(check-expect (render Necklace (λ (w) (cond [(< (widget-quantity w) 5) "red"]
                                            [(< (widget-quantity w) 10) "yellow"]
                                            [else "black"])))
              (place-image (above/align "left" (text (widget->text Necklace 0) TEXT-SIZE "black")
                                        (text (widget->text Chain 1) TEXT-SIZE "yellow")
                                        (text (widget->text Pendant 1) TEXT-SIZE "red")) 200 200 MTS))



(define (render widget fn)
  
  (local [(define (render1 widget indent)
            (above/align "left"
                         (text 
                          (widget->text widget indent) TEXT-SIZE (fn widget))
                         
                         (render1-low (widget-parts widget) (add1 indent))))
                        
          
          (define (render1-low low indent)
            (cond [(empty? low) (square 0 "outline" "white")]
                  [else
                   (above/align "left"
                                (render1 (first low) indent)
                                (render1-low (rest low) indent))]))]

    
    (place-image (render1 widget 0) 200 200 MTS))) 
