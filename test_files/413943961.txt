

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















(define (fn-for-widget widget-0)
  (local [
          (define (fn-for-widget widget)
            (... 
             (widget-name widget) 
             (widget-quantity widget) 
             (widget-time widget) 
             (widget-price widget) 
             (fn-for-low(widget-parts widget)))) 

          (define (fn-for-low low)
            (cond [(empty? low) ...]
                  [else 
                   (...
                    (fn-for-widget (firt low))    
                    (fn-for-low(rest low)))])) ]  
    (fn-for-widget widget-0) ))



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








(check-expect (layersDown Jewelry Rings) 1) 
(check-expect (layersDown Telephone Telephone) 0) 
(check-expect (layersDown Telephone Wire) 2) 
(check-expect (layersDown Cord Wire) 1) 

  

(define (layersDown w1 w2)
  (local [(define (layersDown w_1 w_2 acc)
            (if (equal? w_1 w_2)
                acc
                (fn-for-lox (widget-parts w_1) w_2 (+ 1 acc))))

          (define (fn-for-lox lox w_2 acc)
            (cond [(empty? lox) false]
                  [else
                   (local [(define try (layersDown (first lox) w_2 acc))] 
                     (if (not (false? try))                     
                         try                                    
                         (fn-for-lox (rest lox) w_2 acc)))]))]          

    (layersDown w1 w2 0)))






(check-expect (simple-render Wire)
              (text (string-append
                     (blanks (* (layersDown Wire Wire) TAB)) "Wire" " : " "3" " @ $" "5") TEXT-SIZE TEXT-COLOR)) 

(check-expect (simple-render Necklace) 
              (above/align "left"
                           (text (string-append (blanks (* (layersDown Necklace Necklace) TAB)) "Necklace" " : " "10" " @ $" "3") TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks (* (layersDown Necklace Chain) TAB))    "Chain" " : " "7" " @ $" "1") TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks (* (layersDown Necklace Pendant) TAB))  "Pendant" " : " "4" " @ $" "1") TEXT-SIZE TEXT-COLOR)))



(check-expect (simple-render Jewelry) 
              (above/align "left"
                           (text (string-append (blanks (* (layersDown Jewelry Jewelry) TAB)) "Jewelry set" " : " "4" " @ $" "30") TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks (* (layersDown Jewelry Rings) TAB)) "Rings" " : " "15" " @ $" "11") TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks (* (layersDown Jewelry Necklace) TAB)) "Necklace" " : " "10" " @ $" "3") TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks (* (layersDown Jewelry Chain) TAB))    "Chain" " : " "7" " @ $" "1") TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks (* (layersDown Jewelry Pendant) TAB))  "Pendant" " : " "4" " @ $" "1") TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks (* (layersDown Jewelry Bracelet) TAB))  "Bracelet" " : " "5" " @ $" "5") TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks (* (layersDown Jewelry Beads) TAB))  "Beads" " : " "25" " @ $" "7") TEXT-SIZE TEXT-COLOR)
                           (text (string-append (blanks (* (layersDown Jewelry Glass) TAB))  "Glass" " : " "6" " @ $" "4") TEXT-SIZE TEXT-COLOR)))


(check-expect (simple-render Jewelry) 
              (above/align "left"
                           (text (string-append (blanks (* (layersDown Jewelry Jewelry) TAB)) "Jewelry set" " : " "4" " @ $" "30") TEXT-SIZE TEXT-COLOR)
                           (above/align "left"
                                        (text (string-append (blanks (* (layersDown Jewelry Rings) TAB)) "Rings" " : " "15" " @ $" "11") TEXT-SIZE TEXT-COLOR)
                                        (square 0 "solid" "black"))
                           (above/align "left"
                                        (text (string-append (blanks (* (layersDown Jewelry Necklace) TAB)) "Necklace" " : " "10" " @ $" "3") TEXT-SIZE TEXT-COLOR)
                                        (above/align "left"
                                                     (text (string-append (blanks (* (layersDown Jewelry Chain) TAB))    "Chain" " : " "7" " @ $" "1") TEXT-SIZE TEXT-COLOR)
                                                     (square 0 "solid" "black"))
                                        (above/align "left"
                                                     (text (string-append (blanks (* (layersDown Jewelry Pendant) TAB))  "Pendant" " : " "4" " @ $" "1") TEXT-SIZE TEXT-COLOR)
                                                     (square 0 "solid" "black"))
                                        (square 0 "solid" "black"))
                           (above/align "left"
                                        (text (string-append (blanks (* (layersDown Jewelry Bracelet) TAB))  "Bracelet" " : " "5" " @ $" "5") TEXT-SIZE TEXT-COLOR)
                                        (above/align "left"
                                                     (text (string-append (blanks (* (layersDown Jewelry Beads) TAB))  "Beads" " : " "25" " @ $" "7") TEXT-SIZE TEXT-COLOR)
                                                     (above/align "left"
                                                                  (text (string-append (blanks (* (layersDown Jewelry Glass) TAB))  "Glass" " : " "6" " @ $" "4") TEXT-SIZE TEXT-COLOR)
                                                                  (square 0 "solid" "black"))))))




 












(define (render widget0 fn)
  (local [
          (define (simple-render widget)
            (above/align "left"
                         (text (string-append
                                (blanks (* (layersDown widget0 widget) TAB))  (widget-name widget) " : " (number->string (widget-quantity widget)) " @ $" (number->string (widget-price widget)))
                               TEXT-SIZE (fn widget))
                         (generateText(widget-parts widget)))) 

          (define (generateText low)
            (cond [(empty? low) (square 0 "solid" "black")]
                  [else 
                   (above/align "left"
                                (simple-render (first low))    
                                (generateText(rest low)))])) ]  
    (simple-render widget0) ))





(define (simple-render widget0)
  (render widget0  (lambda (w) TEXT-COLOR)))







(check-expect (stockColoring Jewelry)
              "Red") 
(check-expect (stockColoring Receiver)
              TEXT-COLOR) 
(check-expect (stockColoring Chain)
              "Yellow") 
(check-expect (stockColoring Beads)
              TEXT-COLOR) 

(define (stockColoring widget)
  (cond [(< (widget-quantity widget) 5) "Red"]
        [(< (widget-quantity widget) 10) "Yellow"]
        [else TEXT-COLOR]))

(render Jewelry stockColoring)
(render Telephone stockColoring)





(check-expect (priceColorcode Bracelet)
              "green") 
(check-expect (priceColorcode Jewelry)
              "red") 
(check-expect (priceColorcode Telephone)
              TEXT-COLOR) 
(check-expect (priceColorcode Rings)
              TEXT-COLOR) 




(define (priceColorcode widget)
	(cond 
  [(< (widget-price widget) 7) "green"]
  [(< (widget-price widget) 25) TEXT-COLOR]
  [else "red"]))
  
(render Jewelry priceColorcode)
(render Telephone priceColorcode)
