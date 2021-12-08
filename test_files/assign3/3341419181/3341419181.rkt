

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)

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


(define (fn-for-widget widg)
  (... (widget-name widg)
       (widget-quanity widg)
       (widget-time widg)
       (widget-price widg)
       (widget-parts widg)))


 


(define COLOR "black") 
(define HEIGHT 600)
(define WIDTH 600)
(define MTS (empty-scene WIDTH HEIGHT))

(define TEXT-SIZE 24)
(define SPACE (+ TEXT-SIZE 5))    
(define TAB 5)                    

(define LOW-STOCK 10)             
(define UNDERSTOCK 5)             
(define LOW-STOCK-COLOR "yellow") 
(define UNDERSTOCK-COLOR "red")   
(define TIME 10)                  
(define TIME-COLOR "blue")        





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")



(define-struct world-state(widg fn?))


(define (fn-for-ws state)
  (... (world-state-widg state)
       (world-state-fn? state)))




(define (main ws)
  (big-bang ws
    (on-draw simple-render))) 




(check-expect (default Jewelry) COLOR)

(define (default widg) COLOR)






(define (simple-render ws)
  (if (not (boolean? (world-state-fn? ws)))  
      (render (world-state-widg ws) (world-state-fn? ws)) 
      (place-images/align
          (gen-widgets-images (world-state-widg ws) default)
          (gen-widgets-posn (full-low (world-state-widg ws)))
          "left"
          "top"
          MTS)))






(define (render widg fn?)
  (place-images/align
       (gen-widgets-images widg fn?)
       (gen-widgets-posn (full-low widg))
          "left"
          "top"
          MTS))





(check-expect (stock-status Rings) LOW-STOCK-COLOR)
(check-expect (stock-status Jewelry) UNDERSTOCK-COLOR)
(check-expect (stock-status Necklace) COLOR)

(define (stock-status widg)
        (cond
          [(< (widget-quantity widg) UNDERSTOCK)
           UNDERSTOCK-COLOR]
          [(> (widget-quantity widg) LOW-STOCK)
           LOW-STOCK-COLOR]
          [else
           COLOR]))





(check-expect (timely? Telephone) TIME-COLOR)
(check-expect (timely? Wire) COLOR)

(define (timely? widg)
  (if (> (widget-time widg) TIME)
      TIME-COLOR
      COLOR))

  


(check-expect (create-widget-text Telephone "" default)
              .) 
                                  
(check-expect (create-widget-text Rings (blanks TAB) stock-status)
              .)
(check-expect (create-widget-text Jewelry (blanks TAB) timely?)
              .)


(define (create-widget-text widg indent fn?)
  (local
    ([define widget-text
       (string-append
        indent
        (widget-name widg)
       ": "
       (number->string (widget-quantity widg))
       " @ $"
       (number->string (widget-price widg)))])
      (text widget-text TEXT-SIZE (fn? widg))))
  





(check-expect (gen-widgets-images Beads default)
              (list
               .
               .)) 
(check-expect (gen-widgets-images Rings stock-status)
              (list .))


               

(define (gen-widgets-images widg color-fn)
    (local [(define (select widg num)
              (cons
              (create-widget-text widg (blanks num) color-fn)
              (cycle (widget-parts widg) num)))
            (define (cycle low num)
              (cond [(empty? low) empty]
                    [else
                     (append
                     (select (first low) (+ num TAB))
                     (cycle (rest low) num))]))]
      (select widg 0)))





(check-expect (full-low Rings) (list Rings))
(check-expect (full-low Cord) (list Cord Wire))

(define (full-low widg)
  (local
    ([define (make-low widg)
           (cons widg
                 (add-to-low (widget-parts widg)))]        
     [define (add-to-low low)
       (cond [(empty? low) empty]
             [else
              (append
               (make-low (first low))
               (add-to-low (rest low)))])])
    (make-low widg)))





(check-expect (gen-widgets-posn (list Necklace Chain Pendant))
              (list (make-posn (/ WIDTH 4) (+ 1 SPACE))
                    (make-posn (/ WIDTH 4) (+ 1 SPACE SPACE))
                    (make-posn (/ WIDTH 4) (+ 1 SPACE SPACE SPACE))))
(check-expect (gen-widgets-posn (list Wire))
              (list (make-posn (/ WIDTH 4) (+ 1 SPACE))))

(define (gen-widgets-posn low)
  (local [(define (posn-create low num)
            (cond
              [(empty? low) empty]
              [else
               (cons
                (make-posn (/ WIDTH 4) (+ SPACE num))
                (posn-create (rest low) (+ SPACE num)))]))]
          (posn-create low 1)))


 

(main (make-world-state Telephone timely?))
    