

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

(define MTS (empty-scene 0 0))

 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")





(check-expect (simple-render Rings)
              (text (get-widget-info Rings) TEXT-SIZE TEXT-COLOR))

(check-expect (simple-render Beads)
              (above/align "left"
              (text (get-widget-info Beads) TEXT-SIZE TEXT-COLOR)
              (text (string-append "     " (get-widget-info Glass))
                    TEXT-SIZE TEXT-COLOR)))

 


(define (simple-render wid)
  (render wid (λ (wid) "black")))






(check-expect (get-widget-info Chain)
              (string-append (widget-name Chain)
                             " : "
                             (number->string (widget-quantity Chain))
                             " @ $"
                             (number->string (widget-price Chain)))) 
(check-expect (get-widget-info Jewelry)
              (string-append (widget-name Jewelry)
                             " : "
                             (number->string (widget-quantity Jewelry))
                             " @ $"
                             (number->string (widget-price Jewelry)))) 

(define (get-widget-info wid)
  (string-append (widget-name wid)
                 " : "
                 (number->string (widget-quantity wid))
                 " @ $"
                 (number->string (widget-price wid))))



(check-expect (render Beads red-green-price)
              (above/align "left"
                           (text (get-widget-info Beads)
                                 TEXT-SIZE
                                 (red-green-price Beads))
                           (text (string-append "     " (get-widget-info Glass))
                                 TEXT-SIZE
                                 (red-green-price Glass))))

(check-expect (render Cord yellow-red-quantity)
              (above/align "left"
                           (text (get-widget-info Cord)
                                 TEXT-SIZE
                                 (yellow-red-quantity Cord))
                           (text (string-append "     " (get-widget-info Wire))
                                 TEXT-SIZE
                                 (yellow-red-quantity Wire))))

                           

(define (render wid fn)
  (local[(define (fn-for-widget wid depth)
           (above/align "left" (text (string-append (blanks (* depth TAB))
                                       (get-widget-info wid))
                        TEXT-SIZE (fn wid))
                  (fn-for-low (widget-parts wid) (add1 depth))))
         (define (fn-for-low low depth)
           (cond
             [(empty? low) MTS]
             [else
              (above/align "left" (fn-for-widget (first low) depth)
                     (fn-for-low (rest low) depth))]))]
    (fn-for-widget wid 0)))










(check-expect (yellow-red-quantity Jewelry) "red")
(check-expect (yellow-red-quantity Bracelet) "yellow")
(check-expect (yellow-red-quantity Rings) "black")


(define (yellow-red-quantity wid)
  (cond [(< (widget-quantity wid) 5)
         "red"]
        [(< (widget-quantity wid) 10)
         "yellow"]
        [else
         "black"]))









(check-expect (red-green-price Jewelry) "red")
(check-expect (red-green-price Bracelet) "green")
(check-expect (red-green-price Rings) "black")


(define (red-green-price wid)
  (cond [(< (widget-price wid) 7)
         "green"]
        [(> (widget-price wid) 25)
         "red"]
        [else
         "black"]))



