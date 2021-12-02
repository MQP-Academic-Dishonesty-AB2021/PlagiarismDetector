

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)



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


(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))
(define Telephone (make-widget "Telephone" 5 20 15
                               (list Receiver Buttons Cord)))


(define YUMMY (make-widget "" 0 0 0 empty))


(define GIVEN_SIMPLE_RENDER_TEST .) 
(define GIVEN_RENDER_TEST .) 


(define SIMPLE_YUMMY_TEST .) 
(define SIMPLE_TELEPHONE_TEST .) 
(define SIMPLE_WIRE_TEST .) 


(define YUMMY_TEST .) 
(define TELEPHONE_TEST .) 
(define WIRE_TEST .) 


(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 







(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))


(check-expect (blanks 0) "") 
(check-expect (blanks TAB) "     ") 















(check-expect (simple-render Jewelry) GIVEN_SIMPLE_RENDER_TEST) 
(check-expect (simple-render YUMMY) SIMPLE_YUMMY_TEST) 
(check-expect (simple-render Telephone) SIMPLE_TELEPHONE_TEST) 
(check-expect (simple-render Wire) SIMPLE_WIRE_TEST) 


 


(define (simple-render w)
  (local [(define (simple-test w) TEXT-COLOR)]
    (render w simple-test)))


















(check-expect (render Jewelry joe-becks-could-test)
              GIVEN_RENDER_TEST) 
(check-expect (render YUMMY joe-becks-could-test)
              YUMMY_TEST) 
(check-expect (render Telephone required-stock-checking-test)
              TELEPHONE_TEST ) 
(check-expect (render Wire joe-becks-could-test)
              WIRE_TEST) 


 


(define (render w color-fn)
  (local [(define (place-text space w color-fn) 
            (text (string-append (blanks space)
                                 (widget-name w) " : "
                                 (number->string (widget-quantity w)) " @ $"
                                 (number->string (widget-price w)))
                  TEXT-SIZE (color-fn w)))
          
          (define (fn-for-widget space w color-fn)
            (above/align "left" (place-text space w color-fn)
                         (fn-for-children space (widget-parts w) color-fn)))

          (define (fn-for-children space low color-fn)
            (cond [(empty? low) empty-image]
                  [else
                   (above/align "left" (fn-for-widget (+ space TAB) (first low)
                                                      color-fn)
                                (fn-for-children space (rest low) color-fn))]))]

    (fn-for-widget 0 w color-fn)))











(check-expect (required-stock-checking-test Jewelry) "red") 
(check-expect (required-stock-checking-test Cord) "yellow") 
(check-expect (required-stock-checking-test Beads) "black") 


(define (required-stock-checking-test widget)
  (local [(define stock (widget-quantity widget))]
          (cond [(< stock 5) "red"]
                [(< stock 10) "yellow"]
                [else "black"])))














(check-expect (joe-becks-could-test Jewelry) "red") 
(check-expect (joe-becks-could-test Necklace) "green") 
(check-expect (joe-becks-could-test Beads) "black") 


(define (joe-becks-could-test widget)
  (local [(define cost (widget-price widget))]
          (cond [(< cost 7) "green"]
                [(> cost 25) "red"]
                [else "black"])))