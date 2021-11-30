

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)



(define TEXT-SIZE 24)    
(define TEXT-COLOR "white")


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









 















(check-expect (test-case-text-gen Telephone 0 "white")
              (text (string-append (blanks (* 0 TAB)) 
                                   "Telephone" 
                                   ": "
                                   (number->string 5)
                                   " @ "
                                   (number->string 15))
                    TEXT-SIZE
                    "white"))

(check-expect (test-case-text-gen Cord 3 "green")
              (text (string-append (blanks (* 3 TAB)) 
                                   "Cord" 
                                   ": "
                                   (number->string 7)
                                   " @ "
                                   (number->string 5))
                    TEXT-SIZE
                    "green"))
 
(define (test-case-text-gen widget tab color)
  (text (string-append (blanks (* tab TAB))
                       (widget-name widget)
                       ": "
                       (number->string (widget-quantity widget))
                       " @ "
                       (number->string (widget-price widget)))
        TEXT-SIZE
        color))







(check-expect (simple-render Telephone)
              (above/align "left"
                           (test-case-text-gen Telephone 0 TEXT-COLOR)
                           (test-case-text-gen Receiver 1 TEXT-COLOR)
                           (test-case-text-gen Buttons 1 TEXT-COLOR)
                           (test-case-text-gen Numbers 2 TEXT-COLOR)
                           (test-case-text-gen Cord 1 TEXT-COLOR)
                           (test-case-text-gen Wire 2 TEXT-COLOR)))

(check-expect (simple-render Necklace)
              (above/align "left"
                           (test-case-text-gen Necklace 0 TEXT-COLOR)
                           (test-case-text-gen Chain 1 TEXT-COLOR)
                           (test-case-text-gen Pendant 1 TEXT-COLOR)))

(check-expect (simple-render Wire)
              (test-case-text-gen Wire 0 TEXT-COLOR))



 








 

 

 










(define (simple-render widget)
  ((render widget (λ (n) TEXT-COLOR)) widget))










(define (render widget fn)
  (local [(define (simple-render widget)
            (local [(define (fn-for-widget prefix widget)
                      (above/align "left"
                                   (make-text prefix widget)
                                   (fn-for-low (+ prefix 1)
                                               (widget-parts widget))))
                    (define (fn-for-low  prefix low)
                      (cond [(empty? low) (square 0 "solid" TEXT-COLOR)]
                            [else
                             (above/align "left"
                                          (fn-for-widget prefix (first low))
                                          (fn-for-low prefix (rest low)))]))]
              (fn-for-widget 0 widget)))
          (define (make-text prefix widget)
            (text (string-append (blanks (* prefix TAB))
                                 (widget-name widget)
                                 ": "
                                 (number->string (widget-quantity widget))
                                 " @ "
                                 (number->string (widget-price widget)))
                  TEXT-SIZE
                  (fn widget)))]
    simple-render))












(check-expect (test1 Necklace)
              (above/align "left"
                           (test-case-text-gen Necklace 0 TEXT-COLOR)
                           (test-case-text-gen Chain 1 "yellow")
                           (test-case-text-gen Pendant 1 "red")))
(check-expect (test1 Telephone)
              (above/align "left"
                           (test-case-text-gen Telephone 0 "yellow")
                           (test-case-text-gen Receiver 1 TEXT-COLOR)
                           (test-case-text-gen Buttons 1 "yellow")
                           (test-case-text-gen Numbers 2 "yellow")
                           (test-case-text-gen Cord 1 "yellow")
                           (test-case-text-gen Wire 2 "red")))
(check-expect (test2 Jewelry)
              (above/align "left"
                           (test-case-text-gen Jewelry 0 "yellow")
                                (test-case-text-gen Rings 1 "yellow")
                                (test-case-text-gen Necklace 1 "green")
                                     (test-case-text-gen Chain 2 "green")
                                     (test-case-text-gen Pendant 2 "green")
                                (test-case-text-gen Bracelet 1 "green")
                                     (test-case-text-gen Beads 2 TEXT-COLOR)
                                     (test-case-text-gen Glass 3 "green")))

(define (test1 widget)
  ((render widget
           (λ (n) (cond [(< (widget-quantity n) 5) "red"]
                        [(< (widget-quantity n) 10) "yellow"]
                        [else TEXT-COLOR]))) widget))












(check-expect (test2 Telephone)
              (above/align "left"
                           (test-case-text-gen Telephone 0 "yellow")
                           (test-case-text-gen Receiver 1 TEXT-COLOR)
                           (test-case-text-gen Buttons 1 "green")
                           (test-case-text-gen Numbers 2 "green")
                           (test-case-text-gen Cord 1 "green")
                           (test-case-text-gen Wire 2 "green")))

(check-expect (test2 Necklace)
              (above/align "left"
                           (test-case-text-gen Necklace 0 "green")
                           (test-case-text-gen Chain 1 "green")
                           (test-case-text-gen Pendant 1 "green")))

(check-expect (test2 Jewelry)
              (above/align "left"
                           (test-case-text-gen Jewelry 0 "yellow")
                                (test-case-text-gen Rings 1 "yellow")
                                (test-case-text-gen Necklace 1 "green")
                                     (test-case-text-gen Chain 2 "green")
                                     (test-case-text-gen Pendant 2 "green")
                                (test-case-text-gen Bracelet 1 "green")
                                     (test-case-text-gen Beads 2 TEXT-COLOR)
                                     (test-case-text-gen Glass 3 "green")))

(define (test2 widget)
  ((render widget
           (λ (n) (cond [(< (widget-price n) 7) "green"]
                        [(> (widget-price n) 10) "yellow"]
                        [else TEXT-COLOR]))) widget))

                           