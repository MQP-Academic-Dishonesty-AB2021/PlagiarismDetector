

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


 


 





























(define (render-widget w n color)
  (text (string-append
         (blanks n)
         (widget-name w)
         ": "
         (number->string (widget-quantity w))
         " @ $"
         (number->string (widget-price w)))
        TEXT-SIZE
        color))




(define (render w fn)
  (local [(define (fn-for-widget w n)
            (if (not (empty? (widget-parts w)))
                (above/align "left" (fn w n)
                             (fn-for-low (widget-parts w) (+ n TAB)))
                (fn w n)))
          (define (fn-for-low low n)
            (cond
              [(empty? low) empty-image]
              [else
               (if (not (empty? (widget-parts (first low))))
                   (above/align "left"
                                (fn-for-widget (first low) n)
                                (fn-for-low (rest low) n))
                   (above/align "left"
                                (fn (first low) n)
                                (fn-for-low (rest low) n)))]))]
    (fn-for-widget w 0)))





(define (price-color w n)
  (cond
    [(< (widget-price w) 7) (render-widget w n "green")]
    [(> (widget-price w) 25) (render-widget w n "red")]
    [else (render-widget w n "black")]))



(define (quantity-color w n)
  (cond
    [(< (widget-quantity w) 5) (render-widget w n "red")]
    [(< (widget-quantity w) 10) (render-widget w n "yellow")]
    [else (render-widget w n "black")]))