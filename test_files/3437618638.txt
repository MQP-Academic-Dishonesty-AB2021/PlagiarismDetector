

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))














(define-struct widget (name quantity time price parts))

 


(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))
(define Telephone (make-widget "Telephone" 5 20 15
                               (list Receiver
                                     Buttons
                                     Cord)))


(define Glass (make-widget "Glass" 6 9 4 empty))
(define Beads (make-widget "Beads" 25 12 7 (list Glass)))
(define Bracelet (make-widget "Bracelet" 5 3 5 (list Beads)))
(define Chain (make-widget "Chain" 7 2 1 empty))
(define Pendant (make-widget "Pendant" 4 3 1 empty))
(define Necklace (make-widget "Necklace" 10 7 3
                              (list Chain
                                    Pendant)))
(define Rings (make-widget "Rings" 15 8 11 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30
                             (list Rings
                                   Necklace
                                   Bracelet)))





(define (filter-widget fn? widget)
    (local
        [(define (filter-widget--widget widget)
            (if (fn? widget)
                (cons
                    widget
                    (filter-widget--low (widget-parts widget)))
                (filter-widget--low (widget-parts widget))))
        (define (filter-widget--low low)
            (cond
                [(empty? low) empty]
                [else
                    (append (filter-widget--widget (first low))
                        (filter-widget--low (rest low)))]))]
    (filter-widget--widget widget)))








(check-expect (filter-widget (name-longer-than 6) Telephone) 
    (list Telephone
          Receiver
          Buttons
          Numbers))


(check-expect (filter-widget (name-longer-than 5) Glass) empty)


(check-expect (filter-widget (name-longer-than 5) Bracelet) 
    (list Bracelet))


(check-expect ((name-longer-than 5) Glass) false)


(check-expect ((name-longer-than 4) Telephone) true)

(define (name-longer-than len)
    (lambda (widget)
        (> (string-length (widget-name widget)) len)))









(check-expect (filter-widget (quantity-over 4) Telephone)
    (list Telephone
          Receiver
          Buttons
          Numbers
          Cord))


(check-expect (filter-widget (quantity-over 8) Jewelry)
    (list Rings
          Necklace
          Beads))


(check-expect (filter-widget (quantity-over 8) Glass) empty)


(check-expect ((quantity-over 4) Telephone) true)


(check-expect ((quantity-over 8) Jewelry) false)

(define (quantity-over quantity)
    (lambda (widget)
        (> (widget-quantity widget) quantity)))









(check-expect (filter-widget (cheaper-than 16) Telephone)
    (list Telephone
          Receiver
          Buttons
          Numbers
          Cord
          Wire))


(check-expect (filter-widget (cheaper-than 11) Jewelry)
    (list Necklace
          Chain
          Pendant
          Bracelet
          Beads
          Glass))


(check-expect (filter-widget (cheaper-than 3) Glass) empty)


(check-expect ((cheaper-than 5) Cord) false)


(check-expect ((cheaper-than 8) Cord) true)

(define (cheaper-than price)
    (lambda (widget)
        (< (widget-price widget) price)))










(check-expect (filter-widget (hard-make 4 10) Telephone)
    (list Telephone
          Wire))


(check-expect (filter-widget (hard-make 7 11) Jewelry)
    (list Jewelry
          Pendant
          Bracelet
          Glass))


(check-expect (filter-widget (hard-make 10 5) Glass)
    (list Glass))


(check-expect ((hard-make 2 5) Telephone) true) 


(check-expect ((hard-make 4 8) Glass) false) 

(define (hard-make quantity price)
    (lambda (widget)
        (or (< (widget-quantity widget) quantity) (> (widget-price widget)
                                                     price))))











(check-expect (map widget-name ((qsort string<? widget-name) Wire)) 
    (list "Wire"))


(check-expect (map widget-name ((qsort > widget-time) Necklace))
    (list "Necklace"
          "Pendant"
          "Chain"))



(check-expect (map widget-name ((qsort string<? widget-name) Telephone))
    (list "Buttons"
          "Cord"
          "Numbers"
          "Receiver"
          "Telephone"
          "Wire"))


(check-expect (map widget-name ((qsort < widget-price) Jewelry))
    (list "Chain"
          "Pendant"
          "Necklace"
          "Glass"
          "Bracelet"
          "Beads"
          "Rings"
          "Jewelry set"))

(define (qsort comparison? field)
  (lambda (widget)
    (local [(define (qsort--low comparison? field low)
                (cond
                    [(empty? low) empty]
                    [else
                        (local [(define pivot (first low))
                         (define (should-swap? wid1)
                            (comparison? (field wid1) (field pivot)))]
                        (append
                            (qsort--low comparison? field (filter should-swap?
                                                                  (rest low)))
                            (list pivot)
                            (qsort--low comparison? field
                                (filter (lambda (wid1) 
                                            (not (should-swap? wid1)))
                                        (rest low)))))]))]
    
    (qsort--low comparison? field (filter-widget widget? widget))))) 