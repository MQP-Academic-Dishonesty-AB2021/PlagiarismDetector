

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")
(define TAB 5)

(define-struct widget(name quantity time price parts))



(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers  " 9 5 5 empty))
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




(check-expect (simple-render Jewelry) .)



(define (simple-render wid)
  (render wid (λ (wid) TEXT-COLOR))) 





(check-expect (render Jewelry in-stock?) .)
(check-expect (render Telephone affordable?) .)



(define (render wid fn)   
  (local
    [(define
       (simple-render--widget wid tab-acc)
       (above/align
        "left"
        (text (string-append
               (blanks
                (* TAB tab-acc)) 
               (widget-name wid)
               " : "
               (number->string (widget-quantity wid))
               " @ $"
               (number->string (widget-price wid)))
              TEXT-SIZE (fn wid))   
        (simple-render--list (widget-parts wid) (add1 tab-acc))))
            

     (define (simple-render--list low tab-acc)
       (cond [(empty? low) empty-image]
             [else
              (above/align "left" (simple-render--widget (first low) tab-acc)
                           (simple-render--list (rest low) tab-acc))]))]
     
    (simple-render--widget wid 0))) 








(check-expect (in-stock? Jewelry) "red")
(check-expect (in-stock? Chain) "yellow")
(check-expect (in-stock? Necklace) TEXT-COLOR)



(define (in-stock? wid)
  (cond [(< (widget-quantity wid) 5) "red"]
        [(< (widget-quantity wid) 10) "yellow"]
        [else TEXT-COLOR]))







(check-expect (affordable? Jewelry) "red")
(check-expect (affordable? Wire) "green")
(check-expect (affordable? Rings) TEXT-COLOR)



(define (affordable? wid)
  (cond [(< (widget-price wid) 7) "green"]
        [(> (widget-price wid) 25) "red"]
        [else TEXT-COLOR]))
