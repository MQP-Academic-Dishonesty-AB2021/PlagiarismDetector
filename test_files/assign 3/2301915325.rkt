

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







(define-struct widget (name quantity time price parts))







(define W1 (make-widget "" 0 0 0 empty))

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

 











(define (find-widgets w fn)
  (local [(define (find--widget w)
            (if (fn w)
                (cons w (find--low (widget-parts w)))
                (find--low (widget-parts w))))
          
          (define (find--low low)
            (cond [(empty? low) empty]
                  [else
                   (append (find--widget (first low))
                           (find--low (rest low)))]))]
    (find--widget w)))





(check-expect (find-widget-name-longer-than W1 0)
              empty) 
(check-expect (find-widget-name-longer-than Telephone 7) 
              (list Telephone Receiver))
(check-expect (find-widget-name-longer-than Jewelry 7) 
              (list Jewelry Necklace Bracelet))



(define (find-widget-name-longer-than w num)
  (local [(define (fn? w)
            (> (string-length (widget-name w)) num))]
    (find-widgets w fn?)))





(check-expect (find-widget-quantity-over W1 0)
              empty) 
(check-expect (find-widget-quantity-over Telephone 5) 
              (list Receiver Buttons Numbers Cord))
(check-expect (find-widget-quantity-over Jewelry 7) 
              (list Rings Necklace Beads))



(define (find-widget-quantity-over w num)
  (local [(define (fn? w)
            (> (widget-quantity w) num))]
    (find-widgets w fn?)))





(check-expect (find-widgets-cheaper-than W1 0)
              empty) 
(check-expect (find-widgets-cheaper-than Telephone 6) 
              (list Buttons Numbers Cord Wire))
(check-expect (find-widgets-cheaper-than Jewelry 10) 
              (list Necklace Chain Pendant Bracelet Beads Glass))



(define (find-widgets-cheaper-than w num)
  (local [(define (fn? w)
            (< (widget-price w) num))]
    (find-widgets w fn?)))






(check-expect (find-widget-hard-make W1 0 0)
              empty) 
(check-expect (find-widget-hard-make Telephone 4 6) 
              (list Telephone Receiver Wire))
(check-expect (find-widget-hard-make Jewelry 7 10) 
              (list Jewelry Rings Pendant Bracelet Glass))



(define (find-widget-hard-make w num1 num2)
  (local [(define (fn? w)
            (or (< (widget-quantity w) num1)
                (> (widget-price w) num2)))]
    (find-widgets w fn?)))









(define (qsort fn wField)
  (local [(define (make-sort w)
            (local [(define (sort low)
                      (cond [(empty? low) empty]
                            [else
                             (local [(define pivot (first low))
                                     (define (compare? w)
                                       (fn (wField w) (wField pivot)))]
                               (append (sort (filter compare?
                                                     (rest low)))
                                       (list pivot)
                                       (sort (filter (λ (w)
                                                       (not (compare? w)))
                                                     (rest low)))))]))
                    
                    (define (compile-list w)
                      (local [(define (compile-list--widget w)
                                (cons w (compile-list--low (widget-parts w))))
                              
                              (define (compile-list--low low)
                                (cond [(empty? low) empty]
                                      [else
                                       (append (compile-list--widget (first low))
                                               (compile-list--low (rest low)))]))]
                        (compile-list--widget w)))]
              (sort (compile-list w))))]
    make-sort))





(check-expect (map widget-name (sort-strings W1))
              (list "")) 
(check-expect (map widget-name (sort-strings Telephone)) 
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))
(check-expect (map widget-name (sort-strings Jewelry)) 
              (list "Beads" "Bracelet" "Chain" "Glass"
                    "Jewelry set" "Necklace" "Pendant" "Rings"))



(define sort-strings
  (qsort string<? widget-name))





(check-expect (map widget-name (sort-overstocked W1))
              (list "")) 
(check-expect (map widget-name (sort-overstocked Buttons)) 
              (list "Numbers" "Buttons"))
(check-expect (map widget-name (sort-overstocked Necklace)) 
              (list "Necklace" "Chain" "Pendant"))



(define sort-overstocked
  (qsort > widget-quantity))