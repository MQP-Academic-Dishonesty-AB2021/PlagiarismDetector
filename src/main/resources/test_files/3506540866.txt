

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Part3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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





 


(define (helper wdg)
  (string-append (widget-name wdg) ": " (number->string (widget-quantity wdg)) " @ $" (number->string (widget-price wdg))))



(check-expect (simple-render Glass)  (text (helper Glass) TEXT-SIZE TEXT-COLOR))  
(check-expect (simple-render Necklace) (above/align
                                        "left"
                                        (text (helper Necklace) TEXT-SIZE TEXT-COLOR)
                                        (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Chain) TEXT-SIZE TEXT-COLOR))
                                        (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Pendant) TEXT-SIZE TEXT-COLOR)))) 
(check-expect (simple-render Necklace) .)   


 









(check-expect (render (lambda (wdg)(cond
                                     [(> (widget-price wdg) 25) "red"]
                                     [(< (widget-price wdg) 7) "green"]
                                     [else TEXT-COLOR]))
                      Jewelry)
              (above/align
               "left"
               (text (helper Jewelry) TEXT-SIZE "red")
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Rings) TEXT-SIZE TEXT-COLOR))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Necklace) TEXT-SIZE "green"))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Chain) TEXT-SIZE "green"))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Pendant) TEXT-SIZE "green"))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Bracelet) TEXT-SIZE "green"))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Beads) TEXT-SIZE TEXT-COLOR))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Glass) TEXT-SIZE "green"))
               )) 


(check-expect (render (lambda (wdg) (cond
                                      [(< (widget-quantity wdg) 5) "red"]
                                      [(< (widget-quantity wdg) 10) "yellow"]
                                      [else TEXT-COLOR]))
                      Jewelry)
              (above/align
               "left"
               (text (helper Jewelry) TEXT-SIZE "red")
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Rings) TEXT-SIZE TEXT-COLOR))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Necklace) TEXT-SIZE TEXT-COLOR))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Chain) TEXT-SIZE "yellow"))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Pendant) TEXT-SIZE "red"))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Bracelet) TEXT-SIZE "yellow"))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Beads) TEXT-SIZE TEXT-COLOR))
               (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Glass) TEXT-SIZE "yellow"))
              ))

(define (render fn wdg)
  (local[
         (define (fn-for-widget wdg)
           (above/align
            "left"
            (text (helper wdg) TEXT-SIZE (fn wdg))
            (if (equal? (fn-for-LOW (widget-parts wdg)) empty-image)
                (fn-for-LOW (widget-parts wdg))
                (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (fn-for-LOW (widget-parts wdg))))))    
           

           
         
         (define (fn-for-LOW LOW)
           (cond [(empty? LOW) empty-image]
                 [else
                  (above/align
                   "left"
                   (fn-for-widget (first LOW))   
                   (fn-for-LOW (rest LOW)))]))]         
  
    (fn-for-widget wdg)))






(check-expect (simple-render Glass)  (text (helper Glass) TEXT-SIZE TEXT-COLOR))  
(check-expect (simple-render Necklace) (above/align
                                        "left"
                                        (text (helper Necklace) TEXT-SIZE TEXT-COLOR)
                                        (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Chain) TEXT-SIZE TEXT-COLOR))
                                        (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR) (text (helper Pendant) TEXT-SIZE TEXT-COLOR)))) 
(check-expect (simple-render Necklace) .) 


(define (simple-render wdg) (render (lambda (wdg) TEXT-COLOR) wdg))