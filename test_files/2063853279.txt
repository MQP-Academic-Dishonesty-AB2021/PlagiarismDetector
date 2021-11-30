

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

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

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")






 










(define (render widget fn)
  (local [(define (get-text widget)                                    
            (string-append (widget-name widget)
                           ": "
                           (number->string (widget-quantity widget))
                           " @ $"
                           (number->string (widget-price widget))))


          (define (get-text-color widget)                  
            (cond [(string? (fn widget)) (fn widget)]
                  [else
                   TEXT-COLOR]))

          
          (define (get-text--widget widget sub)         
            (cons
             (text      
              (string-append (blanks (* TAB sub)) (get-text widget))
              TEXT-SIZE
              (get-text-color widget))
             (get-text--low (widget-parts widget) (+ sub 1))))

          
          (define (get-text--low low sub)               
            (cond [(empty? low) empty]
                  [else
                   (append
                    (get-text--widget (first low) sub)
                    (get-text--low (rest low) sub))]))

          (define (draw-images loi)                   
            (if (empty? loi)
                empty-image
                (above/align "left" (first loi) (draw-images (rest loi)))))]
    
    (draw-images (get-text--widget widget 0))))
                    







(define (simple-render widget)
  (render widget (λ (wid) false)))

(simple-render Jewelry) 










(define (compare-text-color fn? num color widget-field)
  (λ(widget)
    (cond [(fn? (widget-field widget) num)
           color]
          [else
           false])))


(define less-than-5 (compare-text-color < 5 "red" widget-quantity))
(check-expect (less-than-5 Wire) "red")
(check-expect (less-than-5 Beads) false)
(check-expect (less-than-5 Receiver) false)


(define less-than-10 (compare-text-color < 10 "yellow" widget-quantity))
(check-expect (less-than-10 Pendant) "yellow")
(check-expect (less-than-10 Beads) false)
(check-expect (less-than-10 Receiver) false)


(define less-than-7-price (compare-text-color < 7 "green" widget-price))
(check-expect (less-than-7-price Pendant) "green")
(check-expect (less-than-7-price Beads) false)
(check-expect (less-than-7-price Receiver) false)
(check-expect (less-than-7-price Necklace) "green")

(define over-25-price (compare-text-color > 25 "red" widget-price))

(define less-than-10-or-5
  (λ(widget)
    (cond [(not (false? (less-than-5 widget)))
           (less-than-5 widget)]
          [(not (false? (less-than-10 widget)))
           (less-than-10 widget)]
          [else
           false])))
(check-expect (less-than-10-or-5 Wire) "red")
(check-expect (less-than-10-or-5 Pendant) "red")
(check-expect (less-than-10-or-5 Numbers) "yellow")
(check-expect (less-than-10-or-5 Beads) false)


(define over-25-price-less-than-7-price
  (λ(widget)
    (cond[(not (false? (over-25-price widget)))
          (less-than-5 widget)]
         [(not (false? (less-than-7-price widget)))
          (less-than-7-price widget)]
         [else
          false])))


(render Necklace less-than-10-or-5)
(render Jewelry over-25-price-less-than-7-price)
(render Telephone less-than-10-or-5)