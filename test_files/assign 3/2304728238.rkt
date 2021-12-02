

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


(define BASE (make-widget "" 0 0 0 empty))

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")
(define TAB 5)





(define (blanks n)
  (list->string (build-list n (λ(x) #\ ))))

(check-expect (blanks 0) "")

(check-expect (blanks TAB) "     ")

(check-expect (blanks 10) "          ")

(check-expect (blanks 15) "               ")



(define (widget-text wid)
  (string-append (widget-name wid) 
     " : "
         (number->string (widget-quantity wid))
         " @ $"
         (number->string (widget-price wid))))
           
           



(check-expect (widget-text Telephone) 
      (string-append 
       (widget-name Telephone)
       " : "
       (number->string (widget-quantity Telephone))
       " @ $" 
       (number->string (widget-price Telephone))))



(check-expect (widget-text (make-widget "n/a" 0 0 0 empty)) 
      "n/a : 0 @ $0")


(check-expect (widget-text Wire) 
      (string-append 
       (widget-name Wire)
       " : "
       (number->string (widget-quantity Wire))
       " @ $" 
       (number->string (widget-price Wire))))     


(check-expect (widget-text Beads) 
      (string-append 
       (widget-name Beads)
       " : "
       (number->string (widget-quantity Beads))
       " @ $" 
       (number->string (widget-price Beads))))  





 


(define (simple-render wid)
  (render wid 
          (lambda (w) 
          TEXT-COLOR)))
  



(check-expect (simple-render BASE) 
      (above/align "left"
               (text (widget-text BASE) TEXT-SIZE TEXT-COLOR)
               (square 0 "solid" "white")))



(check-expect (simple-render Buttons)
      (above/align "left" 
               (text (widget-text Buttons) TEXT-SIZE TEXT-COLOR)
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text (widget-text Numbers) TEXT-SIZE TEXT-COLOR)
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))



(check-expect (simple-render Cord)
      (above/align "left" 
               (text (widget-text Cord) TEXT-SIZE TEXT-COLOR)
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text (widget-text Wire) TEXT-SIZE TEXT-COLOR)
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))
  


(check-expect (simple-render Telephone)
      (above/align "left" 
               (text (widget-text Telephone) TEXT-SIZE TEXT-COLOR)
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text (widget-text Receiver) TEXT-SIZE TEXT-COLOR)
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))
               (above/align "left"
                    (beside
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                     (above/align "left"
                          (text (widget-text Buttons) TEXT-SIZE TEXT-COLOR)
                          (above/align "left"
                               (beside
                                (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                                (above/align "left"
                                     (text (widget-text Numbers)
                                           TEXT-SIZE TEXT-COLOR)
                                     (square 0 "solid" "white")))
                               (square 0 "solid" "white"))
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))
               (above/align "left"
                    (beside
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                     (above/align "left" 
                          (text (widget-text Cord) TEXT-SIZE TEXT-COLOR)
                          (above/align "left" 
                               (beside 
                                (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                                (above/align "left"
                                     (text (widget-text Wire)
                                           TEXT-SIZE TEXT-COLOR)
                                     (square 0 "solid" "white")))
                               (square 0 "solid" "white"))
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))





(define (render wid fn)
  (local [(define (render--wid wid) 
        (above/align "left"
             (text (widget-text wid) TEXT-SIZE (fn wid))
             (render--low (widget-parts wid))))
           
      (define (render--low low) 
        (cond [(empty? low) (square 0 "solid" "white")]
          [else (above/align "left"
                 (beside
                  (text (blanks TAB) TEXT-SIZE (fn (first low))) 
                  (render--wid (first low)))
                 (render--low (rest low)))]))]
             
    (render--wid wid)))







(define (low-stock wid)
  (cond [(< (widget-quantity wid) 5) "red"]
    [(< (widget-quantity wid) 10) "yellow"]
    [else
     TEXT-COLOR]))


(check-expect (low-stock BASE) "red")


(check-expect (low-stock (make-widget "n/a" 9 9 9 empty)) "yellow")


(check-expect (low-stock (make-widget "n/a" 5 5 5 empty)) "yellow")


(check-expect (low-stock (make-widget "n/a" 10 10 10 empty)) TEXT-COLOR)
            






(define (helper2 wid)
  (cond [(< (widget-price wid) 6) "red"]
    [(> (widget-price wid) 8) "green"]
    [else
     TEXT-COLOR]))


(check-expect (helper2 BASE) "red")


(check-expect (helper2 (make-widget "n/a" 9 9 9 empty)) "green")


(check-expect (helper2 (make-widget "n/a" 6 6 6 empty)) TEXT-COLOR)


(check-expect (helper2 (make-widget "n/a" 8 8 8 empty)) TEXT-COLOR)



(check-expect (render BASE low-stock) 
      (above/align "left"
               (text (widget-text BASE) TEXT-SIZE "red")
               (square 0 "solid" "white")))


(check-expect (render (make-widget "n/a1" 5 5 5 
                          (list (make-widget "n/a2" 10 10 10 empty))) 
                      low-stock)
      (above/align "left" 
               (text "n/a1 : 5 @ $5" TEXT-SIZE "yellow")
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text "n/a2 : 10 @ $10" TEXT-SIZE TEXT-COLOR)
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))


(check-expect (render Cord low-stock)
      (above/align "left" 
               (text (widget-text Cord) TEXT-SIZE "yellow")
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text (widget-text Wire) TEXT-SIZE "red")
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))


(check-expect (render Buttons low-stock)
      (above/align "left" 
               (text (widget-text Buttons) TEXT-SIZE "yellow")
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text (widget-text Numbers) TEXT-SIZE "yellow")
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))



(check-expect (render Telephone low-stock)
      (above/align "left" 
               (text (widget-text Telephone) TEXT-SIZE "yellow")
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text (widget-text Receiver) TEXT-SIZE TEXT-COLOR)
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))
               (above/align "left"
                    (beside
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                     (above/align "left"
                          (text (widget-text Buttons) TEXT-SIZE "yellow")
                          (above/align "left"
                               (beside
                                (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                                (above/align "left"
                                     (text (widget-text Numbers)
                                           TEXT-SIZE "yellow")
                                     (square 0 "solid" "white")))
                               (square 0 "solid" "white"))
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))
               (above/align "left"
                    (beside
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                     (above/align "left" 
                          (text (widget-text Cord) TEXT-SIZE "yellow")
                          (above/align "left" 
                               (beside 
                                (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                                (above/align "left"
                                     (text (widget-text Wire) TEXT-SIZE "red")
                                     (square 0 "solid" "white")))
                               (square 0 "solid" "white"))
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))



(check-expect (render BASE helper2) 
          (above/align "left"
                           (text (widget-text BASE) TEXT-SIZE "red")
                           (square 0 "solid" "white")))


(check-expect (render (make-widget "n/a1" 6 6 6
                          (list (make-widget "n/a2" 8 8 8 empty))) 
                      helper2)
      (above/align "left" 
               (text "n/a1 : 6 @ $6" TEXT-SIZE TEXT-COLOR)
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text "n/a2 : 8 @ $8" TEXT-SIZE TEXT-COLOR)
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))


(check-expect (render Cord helper2)
              (above/align "left" 
                           (text (widget-text Cord) TEXT-SIZE "red")
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text (widget-text Wire) TEXT-SIZE "red")
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))



(check-expect (render Buttons helper2)
      (above/align "left" 
               (text (widget-text Buttons) TEXT-SIZE "red")
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text (widget-text Numbers) TEXT-SIZE "red")
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))



(check-expect (render Telephone helper2)
      (above/align "left" 
               (text (widget-text Telephone) TEXT-SIZE "green")
               (above/align "left" 
                    (beside 
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                     (above/align "left"
                          (text (widget-text Receiver) TEXT-SIZE TEXT-COLOR)
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))
               (above/align "left"
                    (beside
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                     (above/align "left"
                          (text (widget-text Buttons) TEXT-SIZE "red")
                          (above/align "left"
                               (beside
                                (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                                (above/align "left"
                                     (text (widget-text Numbers)
                                           TEXT-SIZE "red")
                                     (square 0 "solid" "white")))
                               (square 0 "solid" "white"))
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))
               (above/align "left"
                    (beside
                     (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                     (above/align "left" 
                          (text (widget-text Cord) TEXT-SIZE "red")
                          (above/align "left" 
                               (beside 
                                    (text (blanks TAB) TEXT-SIZE TEXT-COLOR) 
                                (above/align "left"
                                     (text (widget-text Wire) TEXT-SIZE "red")
                                     (square 0 "solid" "white")))
                               (square 0 "solid" "white"))
                          (square 0 "solid" "white")))
                    (square 0 "solid" "white"))))