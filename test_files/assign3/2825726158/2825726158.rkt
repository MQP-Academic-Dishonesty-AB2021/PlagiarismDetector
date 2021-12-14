

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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




(define (fn-for-widget widget)
  (... (widget-name widget)  
       (widget-quantity widget)
       (widget-time widget)
       (widget-price widget)
       (fn-for-Low (widget-parts widget))
       ))



(define (fn-for-Low Low)
  (cond [(empty? Low) empty]
        [else
         (... (fn-for-widget (first Low))
              (fn-for-Low (rest Low)))]))








(check-expect (find-widget-name-longer-than Beads 4) (list Beads Glass))
(check-expect (find-widget-name-longer-than Glass 10) empty)
(check-expect (find-widget-name-longer-than Jewelry 6) (list Jewelry Necklace Pendant Bracelet))
(check-expect (find-widget-name-longer-than Jewelry 10) (list Jewelry))

(define (find-widget-name-longer-than widget nat)
    (cond [(> (string-length (widget-name widget)) nat)
           (cons widget (name-longer-Low (widget-parts widget) nat))]
          [else
           (name-longer-Low (widget-parts widget) nat)]))
       





(check-expect (name-longer-Low empty 5) empty)
(check-expect (name-longer-Low (widget-parts Jewelry) 10) empty)
(check-expect (name-longer-Low (widget-parts Jewelry) 6) (list Necklace Pendant Bracelet))

(define (name-longer-Low Low nat)
    (cond [(empty? Low) empty]
        [else
         (append (find-widget-name-longer-than (first Low) nat)
              (name-longer-Low (rest Low) nat))]))







(check-expect (find-widget-quantity-over Beads 4) (list Beads Glass))
(check-expect (find-widget-quantity-over Glass 10) empty)
(check-expect (find-widget-quantity-over Jewelry 6) (list Rings Necklace Chain Beads))
(check-expect (find-widget-quantity-over Jewelry 10) (list Rings Beads))


(define (find-widget-quantity-over widget nat)
      (cond [(> (widget-quantity widget) nat)
           (cons widget (quantity-bigger-Low (widget-parts widget) nat))]
          [else
           (quantity-bigger-Low (widget-parts widget) nat)]))





(check-expect (quantity-bigger-Low empty 5) empty)
(check-expect (quantity-bigger-Low (widget-parts Jewelry) 10) (list Rings Beads))
(check-expect (quantity-bigger-Low (widget-parts Jewelry) 6) (list Rings Necklace Chain Beads))

(define (quantity-bigger-Low Low nat)
    (cond [(empty? Low) empty]
        [else
         (append (find-widget-quantity-over (first Low) nat)
              (quantity-bigger-Low (rest Low) nat))]))








(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))
(check-expect (find-widgets-cheaper-than Glass 2) empty)
(check-expect (find-widgets-cheaper-than Buttons 6) (list Buttons Numbers))
(check-expect (find-widgets-cheaper-than Jewelry 10) (list Necklace Chain Pendant Bracelet Beads Glass))


(define (find-widgets-cheaper-than widget nat)
      (cond [(< (widget-price widget) nat)
           (cons widget (price-cheaper-Low (widget-parts widget) nat))]
          [else
           (price-cheaper-Low (widget-parts widget) nat)]))





(check-expect (price-cheaper-Low empty 5) empty)
(check-expect (price-cheaper-Low (widget-parts Buttons) 10) (list Numbers))
(check-expect (price-cheaper-Low (widget-parts Telephone) 6) (list Buttons Numbers Cord Wire))

(define (price-cheaper-Low Low nat)
    (cond [(empty? Low) empty]
        [else
         (append (find-widgets-cheaper-than (first Low) nat)
              (price-cheaper-Low (rest Low) nat))]))








(check-expect (find-widget-hard-make Wire 6) (list Wire))
(check-expect (find-widget-hard-make Glass 2) (list Glass))
(check-expect (find-widget-hard-make Buttons 6) empty)
(check-expect (find-widget-hard-make Jewelry 10) (list Jewelry Rings Chain Pendant Bracelet Glass))


(define (find-widget-hard-make widget nat)
      (cond [(or (< (widget-quantity widget) nat) (> (widget-price widget) nat))
           (cons widget (hard-Low (widget-parts widget) nat))]
          [else
           (hard-Low (widget-parts widget) nat)]))





(check-expect (hard-Low empty 5) empty)
(check-expect (hard-Low (widget-parts Buttons) 10) (list Numbers))
(check-expect (hard-Low (widget-parts Telephone) 6) (list Receiver Wire))

(define (hard-Low Low nat)
    (cond [(empty? Low) empty]
        [else
         (append (find-widget-hard-make (first Low) nat)
              (hard-Low (rest Low) nat))]))