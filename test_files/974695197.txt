

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |starter part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





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















(check-expect (fn-for-low-subs (widget-parts Wire) 1) empty)
(check-expect (fn-for-low-subs (widget-parts Bracelet) 7) empty)
(check-expect (fn-for-low-subs (widget-parts Bracelet) 4) (list Beads Glass))

(define (fn-for-low-subs los length)
  (cond [(empty? los) empty]
        [else 
          (append (find-widget-name-longer-than (first los) length)
            (fn-for-low-subs (rest los) length)
          )
        ]
  )
)

(check-expect (find-widget-name-longer-than Wire 1) (list Wire))          
(check-expect (find-widget-name-longer-than Bracelet 7) (list Bracelet))  
(check-expect (find-widget-name-longer-than Bracelet 4) (list Bracelet Beads Glass)) 



(define (find-widget-name-longer-than widget length)
  (if (> (string-length (widget-name widget)) length)
         (cons widget (fn-for-low-subs (widget-parts widget) length))
         (fn-for-low-subs (widget-parts widget) length)
  )
)

(define (find-widget-quantity-over-subs los quantity)
  (cond [(empty? los) empty]
        [else 
          (append (find-widget-quantity-over (first los) quantity)
            (find-widget-quantity-over-subs (rest los) quantity)
          )
        ]
  )
)

(check-expect (find-widget-quantity-over Wire 1) (list Wire)) 
(check-expect (find-widget-quantity-over Bracelet 7) (list Beads)) 
(check-expect (find-widget-quantity-over Bracelet 4) (list Bracelet Beads Glass)) 




(define (find-widget-quantity-over widget quantity)
  (if (> (widget-quantity widget) quantity)
         (cons widget (find-widget-quantity-over-subs (widget-parts widget) quantity))
         (find-widget-quantity-over-subs (widget-parts widget) quantity)
  )
)

(check-expect (find-widget-cheaper-subs (list Beads) 20) (list Beads Glass)) 
(check-expect (find-widget-cheaper-subs (widget-parts Wire) 0) empty) 
(check-expect (find-widget-cheaper-subs (list Rings Necklace Bracelet) 6) (list Necklace Chain Pendant Bracelet Glass)) 

(define (find-widget-cheaper-subs los amount)
  (cond [(empty? los) empty]
        [else 
          (append (find-widgets-cheaper-than (first los) amount)
            (find-widget-cheaper-subs (rest los) amount)
          )
        ]
  )
)

(check-expect (find-widgets-cheaper-than Bracelet 6) (list Bracelet Glass))
(check-expect (find-widgets-cheaper-than Bracelet 0) empty)
(check-expect (find-widgets-cheaper-than Jewelry 6) (list Necklace Chain Pendant Bracelet Glass))



(define (find-widgets-cheaper-than widget amount) 
  (if (< (widget-price widget) amount)
         (cons widget (find-widget-cheaper-subs (widget-parts widget) amount))
         (find-widget-cheaper-subs (widget-parts widget) amount)
  )
)

(check-expect (find-widget-hard-make-subs (list Receiver Buttons Cord) 5 5) (list Receiver Wire)) 
(check-expect (find-widget-hard-make-subs empty 0 999) empty) 
(check-expect (find-widget-hard-make-subs (list Chain Pendant) 20 2) (list Chain Pendant)) 

(define (find-widget-hard-make-subs los quantity amount)
  (cond [(empty? los) empty]
        [else 
          (append (find-widget-hard-make (first los) quantity amount)
            (find-widget-hard-make-subs (rest los) quantity amount)
          )
        ]
  )
)

(check-expect (find-widget-hard-make Telephone 5 5) (list Telephone Receiver Wire))
(check-expect (find-widget-hard-make Wire 0 999) empty) 
(check-expect (find-widget-hard-make Necklace 20 2) (list Necklace Chain Pendant))



(define (find-widget-hard-make widget quantity amount) 
    (if (or (< (widget-quantity widget) quantity) (> (widget-price widget) amount))
         (cons widget (find-widget-hard-make-subs (widget-parts widget) quantity amount))
         (find-widget-hard-make-subs (widget-parts widget) quantity amount)
    ))

