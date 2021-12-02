

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



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

(define TEXT-COLOR "white") 

(define TAB 5) 






(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")






(check-expect (simple-render Jewelry)
              .)
(check-expect (simple-render Telephone)
              .)
              
(define (simple-render e)
  (render e (λ (e) TEXT-COLOR)))






(check-expect (render Jewelry affordable)
              .)

(check-expect (render Telephone low-stock)
              .)

(define (render e fn)
  (local
    [(define (fn-for-element e acc)
       (above/align "left" (text
                            (string-append
                             (blanks (* acc TAB))
                             (widget-name e) " : "
                             (number->string (widget-quantity e)) " @ $"
                             (number->string (widget-price e))) TEXT-SIZE (fn e))
                    (fn-for-low (widget-parts e) (add1 acc))))

     (define (fn-for-low low acc)
       (cond
         [(empty? low) empty-image]
         [else
          (above/align "left" (fn-for-element (first low) acc)
                       (fn-for-low (rest low) acc))]))]
    (fn-for-element e 0)))






(check-expect (low-stock Beads) TEXT-COLOR)
(check-expect (low-stock Wire) "red")
(check-expect (low-stock Chain) "yellow")

(define (low-stock wid)
  (if
   (< (widget-quantity wid) 10)
   (if
    (< (widget-quantity wid) 5)
    "red"
    "yellow")
   TEXT-COLOR))






(check-expect (affordable Telephone) TEXT-COLOR)
(check-expect (affordable Wire) "green")
(check-expect (affordable Jewelry) "red")

(define (affordable wid)
  (cond
    [(< (widget-price wid) 7)
     "green"]
    [(> (widget-price wid) 25)
     "red"]
    [else
     TEXT-COLOR]))

       

