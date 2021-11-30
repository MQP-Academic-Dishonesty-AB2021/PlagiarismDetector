

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(require 2htdp/image)
(require 2htdp/universe)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  

(define TAB 5) 





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))
(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")



(define WIDTH 400)
(define HEIGHT 300)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 2)) 
(define MTS (empty-scene WIDTH HEIGHT))





(define-struct widget (name quantity time price parts))









 




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








(define (render widget fn)
  (local [
          
          
          (define (get-color-list fn widget)
            (local [ (define (flatten widget)
                       (cond
                         [(empty? widget) empty]
                         [(list? widget) (append (flatten (first widget))
                                                 (flatten (rest widget)))] 
                         [(empty? (widget-parts widget)) (list widget)] 
                         [else
                          (append (list widget) (flatten (widget-parts widget)))]))]
              (map fn (flatten widget))))

          (define (form-widget-list widget)
            (local
              
              
              [(define (widget->string widget indents)
                 (cons (string-append (blanks (* TAB indents)) (widget-name widget) " : "
                                      (number->string (widget-quantity widget))
                                      " @" " $" (number->string (widget-price widget)))
                       (widget->los (widget-parts widget) (+ indents 1))))

               
               
               
               (define (widget->los low indents)
                 (cond
                   [(empty? low) empty]
                   [else (append (widget->string (first low) indents)
                                 (widget->los (rest low) indents))]))]
              (widget->string widget 0)))




          (define (render-text los loc)
            (cond [(empty? los) (text " " 1 "white")]
                  [else (above/align "left"
                                     (text (first los) TEXT-SIZE (first loc))
                                     (render-text (rest los) (rest loc)))]))]
    
      (place-images (list (render-text (form-widget-list widget) (get-color-list fn widget)))
                (list (make-posn X-POS Y-POS))
                MTS))

  )




(check-expect (render Wire lowstock) (place-images (list (above 
                                      (text "Wire : 3 @ $5" TEXT-SIZE "Red")
                                      (text "" 1 "white")))  (list (make-posn X-POS Y-POS))
                MTS))


(check-expect (render Wire expensive) (place-images (list (above 
                                       (text "Wire : 3 @ $5" TEXT-SIZE "Green")
                                       (text "" 1 "white")))  (list (make-posn X-POS Y-POS))
                MTS))


(check-expect (render Cord lowstock)
              (place-images (list (above/align "left"
               (text "Cord : 7 @ $5" TEXT-SIZE "Yellow")
               (text "     Wire : 3 @ $5" TEXT-SIZE "Red")
               (text " " 1 "white")))  (list (make-posn X-POS Y-POS))
                MTS))


(check-expect (render Cord expensive)
              (place-images (list (above/align "left"
               (text "Cord : 7 @ $5" TEXT-SIZE "Green")
               (text "     Wire : 3 @ $5" TEXT-SIZE "Green")
               (text " " 1 "white")))  (list (make-posn X-POS Y-POS))
                MTS))


(check-expect (render Jewelry lowstock)  (place-images (list (above/align "left"
                                                      (text "Jewelry set : 4 @ $30" TEXT-SIZE "Red")
                                                      (text "     Rings : 15 @ $11" TEXT-SIZE "Black")
                                                      (text "     Necklace : 10 @ $3" TEXT-SIZE "Black")
                                                      (text "          Chain : 7 @ $1" TEXT-SIZE "Yellow")
                                                      (text "          Pendant : 4 @ $1" TEXT-SIZE "Red")
                                                      (text "     Bracelet : 5 @ $5" TEXT-SIZE "Yellow")
                                                      (text "          Beads : 25 @ $7" TEXT-SIZE "Black")
                                                      (text "               Glass : 6 @ $4" TEXT-SIZE "Yellow")
                                                      (text " " 1 "white")))   (list (make-posn X-POS Y-POS))
                MTS))


(check-expect (render Jewelry expensive)  (place-images (list (above/align "left"
                                                       (text "Jewelry set : 4 @ $30" TEXT-SIZE "Red")
                                                       (text "     Rings : 15 @ $11" TEXT-SIZE "Yellow")
                                                       (text "     Necklace : 10 @ $3" TEXT-SIZE "Green")
                                                       (text "          Chain : 7 @ $1" TEXT-SIZE "Green")
                                                       (text "          Pendant : 4 @ $1" TEXT-SIZE "Green")
                                                       (text "     Bracelet : 5 @ $5" TEXT-SIZE "Green")
                                                       (text "          Beads : 25 @ $7" TEXT-SIZE "Green")
                                                       (text "               Glass : 6 @ $4" TEXT-SIZE "Green")
                                                       (text " " 1 "white")))
                                                         (list (make-posn X-POS Y-POS))
                MTS))




(check-expect (lowstock Jewelry) "Red")
(check-expect (lowstock Rings) "Black")
(check-expect (lowstock Glass) "Yellow")

(define (lowstock widget)
  (cond
    [(< (widget-quantity widget) 5) "Red"]
    [(< (widget-quantity widget) 10) "Yellow" ]
    [else "Black"]))







(check-expect (expensive Jewelry) "Red")
(check-expect (expensive Rings) "Yellow")
(check-expect (expensive Chain) "Green")

 (define (expensive widget)
  (cond
    [(> (widget-price widget) 20) "Red"]
    [(> (widget-price widget) 10) "Yellow"]
    [else "Green"]))


(define (simple-render widget)
  (render widget (λ (widget) TEXT-COLOR)))






 

(check-expect (simple-render Cord)
              (place-images (list (above/align "left"
               (text "Cord : 7 @ $5" TEXT-SIZE TEXT-COLOR)
               (text "     Wire : 3 @ $5" TEXT-SIZE TEXT-COLOR)
               (text " " 1 "white")))  (list (make-posn X-POS Y-POS))
                MTS))


(check-expect (simple-render Jewelry)  (place-images (list (above/align "left"
                                                       (text "Jewelry set : 4 @ $30" TEXT-SIZE TEXT-COLOR)
                                                       (text "     Rings : 15 @ $11" TEXT-SIZE TEXT-COLOR)
                                                       (text "     Necklace : 10 @ $3" TEXT-SIZE TEXT-COLOR)
                                                       (text "          Chain : 7 @ $1" TEXT-SIZE TEXT-COLOR)
                                                       (text "          Pendant : 4 @ $1" TEXT-SIZE TEXT-COLOR)
                                                       (text "     Bracelet : 5 @ $5" TEXT-SIZE TEXT-COLOR)
                                                       (text "          Beads : 25 @ $7" TEXT-SIZE TEXT-COLOR)
                                                       (text "               Glass : 6 @ $4" TEXT-SIZE TEXT-COLOR)
                                                       (text " " 1 "white")))
                                                         (list (make-posn X-POS Y-POS))
                MTS))

(check-expect (simple-render Telephone) (place-images(list (above/align "left"
                                                       (text "Telephone : 5 @ $15" TEXT-SIZE TEXT-COLOR)
                                                       (text "     Receiver : 10 @ $7" TEXT-SIZE TEXT-COLOR)
                                                       (text "     Buttons : 8 @ $5" TEXT-SIZE TEXT-COLOR)
                                                       (text "          Numbers : 9 @ $5" TEXT-SIZE TEXT-COLOR)
                                                       (text "     Cord : 7 @ $5" TEXT-SIZE TEXT-COLOR)
                                                       (text "          Wire : 3 @ $5" TEXT-SIZE TEXT-COLOR)
                                                       (text " " 1 "white")))
                                                         (list (make-posn X-POS Y-POS))
                MTS))

 




(check-expect (render-text (form-widget-list Cord))
              (above/align "left"
                           (text "Cord : 7 @ $5" TEXT-SIZE TEXT-COLOR)
                           (text "     Wire : 3 @ $5" TEXT-SIZE TEXT-COLOR)
                           (text " " 1 "white")))

(check-expect (render-text (form-widget-list Jewelry))
              (above/align "left"
                           (text "Jewelry set : 4 @ $30" TEXT-SIZE TEXT-COLOR)
                           (text "     Rings : 15 @ $11" TEXT-SIZE TEXT-COLOR)
                           (text "     Necklace : 10 @ $3" TEXT-SIZE TEXT-COLOR)
                           (text "          Chain : 7 @ $1" TEXT-SIZE TEXT-COLOR)
                           (text "          Pendant : 4 @ $1" TEXT-SIZE TEXT-COLOR)
                           (text "     Bracelet : 5 @ $5" TEXT-SIZE TEXT-COLOR)
                           (text "          Beads : 25 @ $7" TEXT-SIZE TEXT-COLOR)
                           (text "               Glass : 6 @ $4" TEXT-SIZE TEXT-COLOR)
                           (text " " 1 "white")))
(check-expect (render-text (form-widget-list Bracelet))
              (above/align "left"
                           (text "Bracelet : 5 @ $5" TEXT-SIZE TEXT-COLOR)
                           (text "     Beads : 25 @ $7" TEXT-SIZE TEXT-COLOR)
                           (text "          Glass : 6 @ $4" TEXT-SIZE TEXT-COLOR)
                           (text " " 1 "white")))

(define (render-text los)
  (cond [(empty? los) (text " " 1 "white")]
        [else (above/align "left"
                           (text (first los) TEXT-SIZE TEXT-COLOR)
                           (render-text (rest los)))]))







(check-expect (form-widget-list Cord) (list "Cord : 7 @ $5"
                                            "     Wire : 3 @ $5"))
(check-expect (form-widget-list Telephone) (list "Telephone : 5 @ $15"
                                                 "     Receiver : 10 @ $7"
                                                 "     Buttons : 8 @ $5"
                                                 "          Numbers : 9 @ $5"
                                                 "     Cord : 7 @ $5"
                                                 "          Wire : 3 @ $5"))
(check-expect (form-widget-list Bracelet) (list "Bracelet : 5 @ $5"
                                                "     Beads : 25 @ $7"
                                                "          Glass : 6 @ $4"))
(check-expect (form-widget-list Jewelry) (list "Jewelry set : 4 @ $30"
                                               "     Rings : 15 @ $11"
                                               "     Necklace : 10 @ $3"
                                               "          Chain : 7 @ $1"
                                               "          Pendant : 4 @ $1"
                                               "     Bracelet : 5 @ $5"
                                               "          Beads : 25 @ $7"
                                               "               Glass : 6 @ $4"))

(define (form-widget-list widget)
  (local
    
    
    [(define (widget->string widget indents)
       (cons (string-append (blanks (* TAB indents)) (widget-name widget) " : "
                            (number->string (widget-quantity widget))
                            " @" " $" (number->string (widget-price widget)))
             (widget->los (widget-parts widget) (+ indents 1))))

     
     
     
     (define (widget->los low indents)
       (cond
         [(empty? low) empty]
         [else (append (widget->string (first low) indents)
                       (widget->los (rest low) indents))]))]
    (widget->string widget 0)))
