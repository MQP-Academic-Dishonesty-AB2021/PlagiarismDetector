

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct widget (name quantity time price parts))









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



(define (test-function-1 widget)
  (cond [(< (widget-price widget) 5) "green"]
        [(> (widget-price widget) 6) "red"]
        [else TEXT-COLOR]))



(define (test-function-2 widget)
  (cond [(< (widget-quantity widget) 5) "red"]
        [(< (widget-quantity widget) 10) "yellow"]
        [else TEXT-COLOR]))

(check-expect (render Bracelet test-function-1)
              (above/align "left"
                           (text (string-append (widget-name Bracelet)
                                                " : "
                                                (number->string (widget-quantity Bracelet))
                                                " @ $"
                                                (number->string (widget-price Bracelet)))
                                 TEXT-SIZE
                                 (test-function-1 Bracelet))
                           (text (string-append (blanks TAB)
                                                (widget-name Beads)
                                                " : "
                                                (number->string (widget-quantity Beads))
                                                " @ $"
                                                (number->string (widget-price Beads)))
                                 TEXT-SIZE
                                 (test-function-1 Beads))
                           (text (string-append (blanks (* 2 TAB))
                                                (widget-name Glass)
                                                " : "
                                                (number->string (widget-quantity Glass))
                                                " @ $"
                                                (number->string (widget-price Glass)))
                                 TEXT-SIZE
                                 (test-function-1 Glass))))
(check-expect (render Necklace test-function-2)
              (above/align "left"
                           (text (string-append (widget-name Necklace)
                                                " : "
                                                (number->string (widget-quantity Necklace))
                                                " @ $"
                                                (number->string (widget-price Necklace)))
                                 TEXT-SIZE
                                 (test-function-2 Necklace))
                           (text (string-append (blanks TAB)
                                                (widget-name Chain)
                                                " : "
                                                (number->string (widget-quantity Chain))
                                                " @ $"
                                                (number->string (widget-price Chain)))
                                 TEXT-SIZE
                                 (test-function-2 Chain))
                           (text (string-append (blanks TAB)
                                                (widget-name Pendant)
                                                " : "
                                                (number->string (widget-quantity Pendant))
                                                " @ $"
                                                (number->string (widget-price Pendant)))
                                 TEXT-SIZE
                                 (test-function-2 Pendant))))




(define (render widget fn)
  (local [(define (render widget) 
            (above/align "left"
                         (text (string-append (widget-name widget)
                                              " : "
                                              (number->string (widget-quantity widget))
                                              " @ $"
                                              (number->string (widget-price widget)))
                               TEXT-SIZE
                               (fn widget))
                         (render--low (widget-parts widget))))
          (define (render--low low) 
            (cond [(empty? low) empty-image]
                  [else
                   (above/align "left"
                                (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                                        (render (first low)))
                                (render--low (rest low)))]))]
    (render widget)))

(check-expect (simple-render-1 Bracelet test-function-1)
              (above/align "left"
                           (text (string-append (widget-name Bracelet)
                                                " : "
                                                (number->string (widget-quantity Bracelet))
                                                " @ $"
                                                (number->string (widget-price Bracelet)))
                                 TEXT-SIZE
                                 (test-function-1 Bracelet))
                           (text (string-append (blanks TAB)
                                                (widget-name Beads)
                                                " : "
                                                (number->string (widget-quantity Beads))
                                                " @ $"
                                                (number->string (widget-price Beads)))
                                 TEXT-SIZE
                                 (test-function-1 Beads))
                           (text (string-append (blanks (* 2 TAB))
                                                (widget-name Glass)
                                                " : "
                                                (number->string (widget-quantity Glass))
                                                " @ $"
                                                (number->string (widget-price Glass)))
                                 TEXT-SIZE
                                 (test-function-1 Glass))))

(check-expect (simple-render-2 Necklace test-function-2)
              (above/align "left"
                           (text (string-append (widget-name Necklace)
                                                " : "
                                                (number->string (widget-quantity Necklace))
                                                " @ $"
                                                (number->string (widget-price Necklace)))
                                 TEXT-SIZE
                                 (test-function-2 Necklace))
                           (text (string-append (blanks TAB)
                                                (widget-name Chain)
                                                " : "
                                                (number->string (widget-quantity Chain))
                                                " @ $"
                                                (number->string (widget-price Chain)))
                                 TEXT-SIZE
                                 (test-function-2 Chain))
                           (text (string-append (blanks TAB)
                                                (widget-name Pendant)
                                                " : "
                                                (number->string (widget-quantity Pendant))
                                                " @ $"
                                                (number->string (widget-price Pendant)))
                                 TEXT-SIZE
                                 (test-function-2 Pendant))))




(define (simple-render widget)
  (local [(define (simple-render widget) 
            (above/align "left"
                         (text (string-append (widget-name widget)
                                              " : "
                                              (number->string (widget-quantity widget))
                                              " @ $"
                                              (number->string (widget-price widget)))
                               TEXT-SIZE
                               TEXT-COLOR)
                         (simple-render--low (widget-parts widget))))
          (define (simple-render--low low) 
            (cond [(empty? low) empty-image]
                  [else
                   (above/align "left"
                                (beside (text (blanks TAB) TEXT-SIZE TEXT-COLOR)
                                        (simple-render (first low)))
                                (simple-render--low (rest low)))]))]
    render))

(define simple-render-1 (simple-render Bracelet))
(define simple-render-2 (simple-render Necklace))