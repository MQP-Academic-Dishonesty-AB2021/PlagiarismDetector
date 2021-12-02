

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







(define (find-widget-name-longer-than awidget length)
  (if (> (string-length (widget-name awidget)) length)
      (cons awidget
           (find-widget-name-longer-than--low (widget-parts awidget) length))
      (find-widget-name-longer-than--low (widget-parts awidget) length)))

(define (find-widget-name-longer-than--low alow length)
  (cond [(empty? alow) empty]
        [else (append (find-widget-name-longer-than (first alow) length)
               (find-widget-name-longer-than--low (rest alow) length))]))

(check-expect (find-widget-name-longer-than Wire 10) empty)

(check-expect (find-widget-name-longer-than Wire 2) (list Wire))

(check-expect (find-widget-name-longer-than Wire 4) empty)


(check-expect (find-widget-name-longer-than Cord 2) (list Cord Wire))

(check-expect (find-widget-name-longer-than Telephone 4)
              (list Telephone Receiver Buttons Numbers))
(check-expect (find-widget-name-longer-than Telephone 3)
              (list Telephone Receiver Buttons Numbers Cord Wire))







(check-expect (find-widget-quantity-over Wire 10) empty)

(check-expect (find-widget-quantity-over Wire 2) (list Wire))

(check-expect (find-widget-quantity-over Wire 3) empty)


(check-expect (find-widget-quantity-over Cord 1) (list Cord Wire))

(check-expect (find-widget-quantity-over Telephone 1)
              (list Telephone Receiver Buttons Numbers Cord Wire))


(define (find-widget-quantity-over awidget quant)
  (if (> (widget-quantity awidget) quant)
      (cons awidget
           (find-widget-quantity-over--low (widget-parts awidget) quant))
      (find-widget-quantity-over--low (widget-parts awidget) quant)))

(define (find-widget-quantity-over--low alow quant)
  (cond [(empty? alow) empty]
        [else (append (find-widget-quantity-over (first alow) quant)
               (find-widget-quantity-over--low (rest alow) quant))]))








(check-expect (find-widgets-cheaper-than Wire 2) empty)

(check-expect (find-widgets-cheaper-than Wire 10) (list Wire))

(check-expect (find-widgets-cheaper-than Wire 5) empty)


(check-expect (find-widgets-cheaper-than Cord 10) (list Cord Wire))

(check-expect (find-widgets-cheaper-than Telephone 10)
              (list Receiver Buttons Numbers Cord Wire))



(define (find-widgets-cheaper-than awidget cost)
  (if (< (widget-price awidget) cost)
      (cons awidget
           (find-widgets-cheaper-than--low (widget-parts awidget) cost))
      (find-widgets-cheaper-than--low (widget-parts awidget) cost)))

(define (find-widgets-cheaper-than--low alow cost)
  (cond [(empty? alow) empty]
        [else (append (find-widgets-cheaper-than (first alow) cost)
               (find-widgets-cheaper-than--low (rest alow) cost))]))








(check-expect (find-widget-hard-make Wire 1 10) empty)

(check-expect (find-widget-hard-make Wire 20 10) (list Wire))

(check-expect (find-widget-hard-make Wire 1 1) (list Wire))

(check-expect (find-widget-hard-make Wire 3 10) empty)

(check-expect (find-widget-hard-make Wire 1 5) empty)

(check-expect (find-widget-hard-make Wire 3 5) empty)


(check-expect (find-widget-hard-make Telephone 7 6)
              (list Telephone Receiver Wire))

(check-expect (find-widget-hard-make Telephone 20 0)
              (list Telephone Receiver Buttons Numbers Cord Wire))

(check-expect (find-widget-hard-make Telephone 0 20) empty)



(define (find-widget-hard-make awidget quant cost)
  (if (include-widget awidget quant cost)
      (cons awidget
            (find-widget-hard-make--low (widget-parts awidget) quant cost))
      (find-widget-hard-make--low (widget-parts awidget) quant cost)
      ))

(define (find-widget-hard-make--low alow quant cost)
  (cond [(empty? alow) empty]
        [else (append (find-widget-hard-make (first alow) quant cost)
               (find-widget-hard-make--low (rest alow) quant cost))]))










(check-expect (include-widget Pendant 2 4) false)
(check-expect (include-widget Pendant 5 4) true)  
(check-expect (include-widget Pendant 2 0) true)  

(define (include-widget awidget quant cost)
  (or (< (widget-quantity awidget) quant) (> (widget-price awidget) cost)))