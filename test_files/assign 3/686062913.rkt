

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))














(define-struct widget (name quantity time price parts))

 


(define (fn-for--low low)
    (cond
        [(empty? low) ..]
        [else
            (... (fn-for--widget (first low))
                 (fn-for-low (rest low)))]))


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










(check-expect (find-widget-name-longer-than Telephone 6) 
    (list Telephone
          Receiver
          Buttons
          Numbers))


(check-expect (find-widget-name-longer-than Glass 5) empty)


(check-expect (find-widget-name-longer-than Bracelet 5) 
    (list Bracelet))


(check-expect (find-widget-name-longer-than--low (widget-parts Telephone) 6)
    (list Receiver
          Buttons
          Numbers))

(define (find-widget-name-longer-than widget len) 
    (if (> (string-length (widget-name widget)) len)
        (cons widget (find-widget-name-longer-than--low (widget-parts widget)
                                                        len))
        (find-widget-name-longer-than--low (widget-parts widget) len)))

(define (find-widget-name-longer-than--low low len)
    (cond
        [(empty? low) empty]
        [else
            (append (find-widget-name-longer-than (first low) len)
                (find-widget-name-longer-than--low (rest low) len))]))










(check-expect (find-widget-quantity-over Telephone 4)
    (list Telephone
          Receiver
          Buttons
          Numbers
          Cord))


(check-expect (find-widget-quantity-over Jewelry 8)
    (list Rings
          Necklace
          Beads))


(check-expect (find-widget-quantity-over Glass 8) empty)


(check-expect (find-widget-quantity-over--low (widget-parts Telephone) 4)
    (list Receiver
          Buttons
          Numbers
          Cord))

(define (find-widget-quantity-over widget quantity)
    (if (> (widget-quantity widget) quantity)
        (cons widget
            (find-widget-quantity-over--low (widget-parts widget) quantity))
        (find-widget-quantity-over--low (widget-parts widget) quantity)))

(define (find-widget-quantity-over--low low quantity)
    (cond
        [(empty? low) empty]
        [else
            (append (find-widget-quantity-over (first low) quantity)
                (find-widget-quantity-over--low (rest low) quantity))]))










(check-expect (find-widgets-cheaper-than Telephone 16)
    (list Telephone
          Receiver
          Buttons
          Numbers
          Cord
          Wire))


(check-expect (find-widgets-cheaper-than Jewelry 11)
    (list Necklace
          Chain
          Pendant
          Bracelet
          Beads
          Glass))


(check-expect (find-widgets-cheaper-than Glass 3) empty)


(check-expect (find-widgets-cheaper-than--low (widget-parts Telephone) 16)
    (list Receiver
          Buttons
          Numbers
          Cord
          Wire))

(define (find-widgets-cheaper-than widget price)
    (if (< (widget-price widget) price)
        (cons widget
            (find-widgets-cheaper-than--low (widget-parts widget) price))
        (find-widgets-cheaper-than--low (widget-parts widget) price)))

(define (find-widgets-cheaper-than--low low price)
    (cond
        [(empty? low) empty]
        [else
            (append (find-widgets-cheaper-than (first low) price)
                (find-widgets-cheaper-than--low (rest low) price))]))










(check-expect (is-hard-to-make? Telephone 6 20)
    true)



(check-expect (is-hard-to-make? Buttons 6 2)
    true)



(check-expect (is-hard-to-make? Jewelry 4 31)
    false)



(check-expect (is-hard-to-make? Glass 5 4) 
    false)

(define (is-hard-to-make? widget quantity price) 
    (or (< (widget-quantity widget) quantity) (> (widget-price widget) price)))









(check-expect (find-widget-hard-make Telephone 4 10)
    (list Telephone
          Wire))
    

(check-expect (find-widget-hard-make Jewelry 7 11)
    (list Jewelry
          Pendant
          Bracelet
          Glass))
    

(check-expect (find-widget-hard-make Glass 10 5) 
    (list Glass))


(check-expect (find-widget-hard-make--low (widget-parts Telephone) 2 30)
    empty)

(define (find-widget-hard-make widget quantity price)
    (if (is-hard-to-make? widget quantity price)
        (cons widget
            (find-widget-hard-make--low (widget-parts widget) quantity price))
        (find-widget-hard-make--low (widget-parts widget) quantity price)))

(define (find-widget-hard-make--low low quantity price)
    (cond
        [(empty? low) empty]
        [else
            (append (find-widget-hard-make (first low) quantity price)
                (find-widget-hard-make--low (rest low) quantity price))]))