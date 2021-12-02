

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab3P3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(require 2htdp/image)
(require 2htdp/universe)

(define MTS (empty-scene 800 600))
(define XCON 150)
(define YCON 10)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 






(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")



(define (simple-render WIDGET)
  (big-bang WIDGET
    (to-draw render-text)))



(define (place-text w)
  (local     
     [(define (print widge)
       (text (string-append (widget-name widge) " : " (number->string(widget-quantity widge)) " @ $" (number->string(widget-price widge))) TEXT-SIZE TEXT-COLOR))

     (define (fn-one w)
       (cons w (fn-low (widget-parts w))))

     (define (fn-low low)
       (cond
         [(empty? low) empty]
         [else
          (append (fn-one (first low))
                  (fn-low (rest low)))]))
        
     ]
    
    (map (λ(n) (print n))(fn-one w))))




(define (addCoords n)
  (cond
    [(= n 0) empty]
    [else
      (cons (make-posn XCON (+ YCON (* n 25))) (addCoords (- n 1)))]))



(define (reverseList list)
  (foldl cons '() list))




(define (render-text w)
  (place-images
   (reverseList (place-text w))
   (addCoords (length(place-text w)))
   MTS))







(define (cost-more widge num)
  (if (> (widget-price  widge) num)
      true
      false))



(define (quan-less widge num)
  (if (< (widget-quantity widge) num)
      true
      false))



(define (text-color fn color)
  (cond
    [(fn)
     color]
    [else
     TEXT-COLOR]))
