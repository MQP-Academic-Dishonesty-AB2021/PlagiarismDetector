

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


 


 







   


  (check-expect (find-widget-name-longer-than Wire 6) empty)

  (check-expect (find-widget-name-longer-than Cord 2) (list Cord Wire))

  (check-expect (find-widget-name-longer-than Telephone 8) (list Telephone))


(define (find-widget-name-longer-than w length)
  (cond [(> (string-length (widget-name w)) length)
         (cons w
               (fn-for-low (widget-parts w) length))]
        [else (fn-for-low (widget-parts w) length)]))




(define (fn-for-low low length)
  (cond
    [(empty? low) empty]
    [else
     (append
     (find-widget-name-longer-than (first low) length)
     (fn-for-low (rest low) length))]))







   


  (check-expect (find-widget-quantity-over Wire 4) empty)

  (check-expect (find-widget-quantity-over Cord 2) (list Cord Wire))

  (check-expect (find-widget-quantity-over Telephone 9) (list Receiver))

(define (find-widget-quantity-over w amount)
  (cond [(> (widget-quantity w) amount)
         (cons w
               (fn-for-low-quantity (widget-parts w) amount))]
        [else (fn-for-low-quantity (widget-parts w) amount)]))




(define (fn-for-low-quantity low amount)
  (cond
    [(empty? low) empty]
    [else
     (append
     (find-widget-quantity-over (first low) amount)
     (fn-for-low-quantity (rest low) amount))]))







   


  (check-expect (find-widget-cheaper-than Wire 4) empty)

  (check-expect (find-widget-cheaper-than Cord 10) (list Cord Wire))

  (check-expect (find-widget-cheaper-than Buttons 10) (list Buttons Numbers))

  (check-expect (find-widget-cheaper-than Telephone 10)
                (list Receiver Buttons Numbers Cord Wire))

(define (find-widget-cheaper-than w price)
  (cond [(< (widget-price w) price)
         (cons w
               (fn-for-low-cheaper (widget-parts w) price))]
        [else (fn-for-low-cheaper (widget-parts w) price)]))




(define (fn-for-low-cheaper low price)
  (cond
    [(empty? low) empty]
    [else
    (append
     (find-widget-cheaper-than (first low) price)
     (fn-for-low-cheaper (rest low) price))]))








   


  (check-expect (find-widget-hard-make Wire 3 5) empty)

  (check-expect (find-widget-hard-make Cord 8 6) (list Cord Wire))

  (check-expect (find-widget-hard-make Buttons 10 4) (list Buttons Numbers))

  (check-expect (find-widget-hard-make Telephone 4 16) (list Wire))


(define (find-widget-hard-make w stock cost)
  (cond [(hard-to-make? w stock cost)
         (cons w
               (fn-for-low-hard (widget-parts w) stock cost))]
        [else (fn-for-low-hard (widget-parts w) stock cost)]))




(define (fn-for-low-hard low stock cost)
  (cond
    [(empty? low) empty]
    [else
    (append
     (find-widget-hard-make (first low) stock cost)
     (fn-for-low-hard (rest low) stock cost))]))







   


  (check-expect (hard-to-make Wire 3 5) false)

  (check-expect (hard-to-make Wire 2 5) true)

  (check-expect (hard-to-make Wire 3 7) true)

(define (hard-to-make? w stock cost)
  (or (< (widget-quantity w) stock) (> (widget-price w) cost)))
