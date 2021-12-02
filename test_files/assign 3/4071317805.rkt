

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





(define (simple-render wid)
  (local [(define (fn-widget wid)
            (above/align "left" (text (format-widget wid) TEXT-SIZE TEXT-COLOR) (tab (fn-low (widget-parts wid)))))
          (define (fn-low low)
            (if (empty? low) (square 0 "solid" "white")
                (above/align "left" (fn-widget (first low)) (fn-low (rest low)))))
          (define (tab img) (if (zero? (image-width img)) img (beside (text (make-string TAB #\space) TEXT-SIZE TEXT-COLOR) img)))]
    (fn-widget wid)))

(check-expect (simple-render Jewelry)
              (above/align "left"
                           (text "Jewelry set : 4 @ $30"
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (above/align "left"
                                        (text
                                         "     Rings : 15 @ $11"
                                         TEXT-SIZE
                                         TEXT-COLOR)
                                        (above/align "left"
                                                     (text
                                                      "     Necklace : 10 @ $3"
                                                      TEXT-SIZE
                                                      TEXT-COLOR)
                                                     (above/align "left"
                                                                  (text
                                                                   "          Chain : 7 @ $1"
                                                                   TEXT-SIZE
                                                                   TEXT-COLOR)
                                                                  (above/align "left"
                                                                               (text
                                                                                "          Pendant : 4 @ $1"
                                                                                TEXT-SIZE
                                                                                TEXT-COLOR)
                                                                               (above/align "left"
                                                                                            (text
                                                                                             "     Bracelet : 5 @ $5"
                                                                                             TEXT-SIZE
                                                                                             TEXT-COLOR)
                                                                                            (above/align "left"
                                                                                                         (text
                                                                                                          "          Beads : 25 @ $7"
                                                                                                          TEXT-SIZE
                                                                                                          TEXT-COLOR)
                                                                                                         (above/align "left"
                                                                                                                      (text
                                                                                                                       "               Glass : 6 @ $4"
                                                                                                                       TEXT-SIZE
                                                                                                                       TEXT-COLOR)(square 0 "solid" "white"))))))))))


(define (format-widget wid) (string-append (widget-name wid) " : " (number->string (widget-quantity wid)) " @ $" (number->string (widget-price wid))))
(check-expect (format-widget Wire) "Wire : 3 @ $5")
(check-expect (format-widget Jewelry) "Jewelry set : 4 @ $30")

















(define (BASE-COLOR-SELECTOR wid) TEXT-COLOR)




(define (colorer los)
  (local [(define (fn-los los wid)
            (let
                ([try ((first los) wid)])
              (if (image-color? try) try (fn-los (rest los) wid))))]
    (λ (wid) (fn-los (append los (list BASE-COLOR-SELECTOR)) wid))))



(define (color-selector selector fn? value color) (λ (wid) (if (fn? (selector wid) value) color false)))



(define cost-selector (colorer (list (color-selector widget-price < 7 "green") (color-selector widget-price > 25 "red"))))

(check-expect (cost-selector Wire) "green")
(check-expect (cost-selector Jewelry) "red")
(check-expect (cost-selector Beads) TEXT-COLOR)



(define quantity-selector (colorer (list (color-selector widget-quantity < 5 "red") (color-selector widget-quantity < 10 "yellow"))))

(check-expect (quantity-selector Buttons) "yellow")
(check-expect (quantity-selector Wire) "red")
(check-expect (quantity-selector Necklace) TEXT-COLOR)






(define time-selector (colorer (list (color-selector widget-time < 3 "green") (color-selector widget-time < 5 "yellow")
                                     (color-selector widget-time < 7 "orange") (color-selector widget-time < 10 "red"))))

(check-expect (time-selector Chain) "green")
(check-expect (time-selector Bracelet) "yellow")
(check-expect (time-selector Wire) "orange")
(check-expect (time-selector Necklace) "red")
(check-expect (time-selector  Jewelry) TEXT-COLOR)




(define (render fn wid)
  (local [(define (fn-widget wid)
            (above/align "left" (text (format-widget wid) TEXT-SIZE (fn wid)) (tab (fn-low (widget-parts wid)))))
          (define (fn-low low)
            (if (empty? low) (square 0 "solid" "white")
                (above/align "left" (fn-widget (first low)) (fn-low (rest low)))))
          (define (tab img) (if (zero? (image-width img)) img (beside (text (make-string TAB #\space) TEXT-SIZE TEXT-COLOR) img)))]
    (fn-widget wid)))
 
(check-expect (render cost-selector Jewelry)
              (above/align "left"
                           (text "Jewelry set : 4 @ $30"
                                 TEXT-SIZE
                                 "red")
                           (above/align "left"
                                        (text
                                         "     Rings : 15 @ $11"
                                         TEXT-SIZE
                                         TEXT-COLOR)
                                        (above/align "left"
                                                     (text
                                                      "     Necklace : 10 @ $3"
                                                      TEXT-SIZE
                                                      "green")
                                                     (above/align "left"
                                                                  (text
                                                                   "          Chain : 7 @ $1"
                                                                   TEXT-SIZE
                                                                   "green")
                                                                  (above/align "left"
                                                                               (text
                                                                                "          Pendant : 4 @ $1"
                                                                                TEXT-SIZE
                                                                                "green")
                                                                               (above/align "left"
                                                                                            (text
                                                                                             "     Bracelet : 5 @ $5"
                                                                                             TEXT-SIZE
                                                                                             "green")
                                                                                            (above/align "left"
                                                                                                         (text
                                                                                                          "          Beads : 25 @ $7"
                                                                                                          TEXT-SIZE
                                                                                                          TEXT-COLOR)
                                                                                                         (above/align "left"
                                                                                                                      (text
                                                                                                                       "               Glass : 6 @ $4"
                                                                                                                       TEXT-SIZE
                                                                                                                       "green")(square 0 "solid" "white"))))))))))
                           

(define (simple-render2 wid) (render BASE-COLOR-SELECTOR wid))
(check-expect (simple-render2 Jewelry)
              (above/align "left"
                           (text "Jewelry set : 4 @ $30"
                                 TEXT-SIZE
                                 TEXT-COLOR)
                           (above/align "left"
                                        (text
                                         "     Rings : 15 @ $11"
                                         TEXT-SIZE
                                         TEXT-COLOR)
                                        (above/align "left"
                                                     (text
                                                      "     Necklace : 10 @ $3"
                                                      TEXT-SIZE
                                                      TEXT-COLOR)
                                                     (above/align "left"
                                                                  (text
                                                                   "          Chain : 7 @ $1"
                                                                   TEXT-SIZE
                                                                   TEXT-COLOR)
                                                                  (above/align "left"
                                                                               (text
                                                                                "          Pendant : 4 @ $1"
                                                                                TEXT-SIZE
                                                                                TEXT-COLOR)
                                                                               (above/align "left"
                                                                                            (text
                                                                                             "     Bracelet : 5 @ $5"
                                                                                             TEXT-SIZE
                                                                                             TEXT-COLOR)
                                                                                            (above/align "left"
                                                                                                         (text
                                                                                                          "          Beads : 25 @ $7"
                                                                                                          TEXT-SIZE
                                                                                                          TEXT-COLOR)
                                                                                                         (above/align "left"
                                                                                                                      (text
                                                                                                                       "               Glass : 6 @ $4"
                                                                                                                       TEXT-SIZE
                                                                                                                       TEXT-COLOR)(square 0 "solid" "white"))))))))))