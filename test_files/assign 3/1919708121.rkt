

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(define-struct widget(name quantity time price parts))


  (define Wire (make-widget "Wire" 3 5 5 empty))
  (define Cord (make-widget "Cord" 7 5 5 (list Wire)))
  (define Numbers (make-widget "Numbers" 9 5 5 empty))
  (define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
  (define Receiver (make-widget "Receiver" 10 5 7 empty))

(define Telephone (make-widget "Telephone" 5 20 15 (list Receiver Buttons Cord)))



 
 





(define (find-widget-name-longer-than widget cut)
  (if (> (string-length (widget-name widget)) cut)
                (cons widget (name-longer-low (widget-parts widget) cut))
                (name-longer-low (widget-parts widget) cut)
                )
            )
(define (name-longer-low low cut)
            (cond
              [(empty? low) empty]
              [else (append (find-widget-name-longer-than (first low) cut) (name-longer-low (rest low) cut))]
              )
  )

(check-expect (find-widget-name-longer-than Telephone 0) (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-name-longer-than Numbers 0) (list Numbers))
(check-expect (find-widget-name-longer-than Numbers 3) (list Numbers))
(check-expect (find-widget-name-longer-than Telephone 5) (list Telephone Receiver Buttons Numbers))





(define (find-widget-quantity-over widget cut)
  (if (> (widget-quantity widget) cut)
                (cons widget (quantity-over-low (widget-parts widget) cut))
                (quantity-over-low (widget-parts widget) cut)
                )
            )
(define (quantity-over-low low cut)
            (cond
              [(empty? low) empty]
              [else (append (find-widget-quantity-over (first low) cut) (quantity-over-low (rest low) cut))]
              )
  )

(check-expect (find-widget-quantity-over Telephone 0) (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-quantity-over Numbers 0) (list Numbers))
(check-expect (find-widget-quantity-over Numbers 3) (list Numbers))
(check-expect (find-widget-quantity-over Telephone 5) (list Receiver Buttons Numbers Cord))





(define (find-widgets-cheaper-than widget cut)
  (if (< (widget-price widget) cut)
                (cons widget (cheaper-than-low (widget-parts widget) cut))
                (cheaper-than-low (widget-parts widget) cut)
                )
            )
(define (cheaper-than-low low cut)
            (cond
              [(empty? low) empty]
              [else (append (find-widgets-cheaper-than (first low) cut) (cheaper-than-low (rest low) cut))]
              )
  )

(check-expect (find-widgets-cheaper-than Telephone 6) (list Buttons Numbers Cord Wire))
(check-expect (find-widgets-cheaper-than Numbers 10) (list Numbers))
(check-expect (find-widgets-cheaper-than Numbers 99) (list Numbers))
(check-expect (find-widgets-cheaper-than Numbers 0) empty)
(check-expect (find-widgets-cheaper-than Telephone 16) (list Telephone Receiver Buttons Numbers Cord Wire))





(define (find-widget-hard-make widget cut cut2)
  (if (or (< (widget-quantity widget) cut) (> (widget-price widget) cut2))
                (cons widget (hard-make-low (widget-parts widget) cut cut2))
                (hard-make-low (widget-parts widget) cut cut2)
                )
  )
(define (hard-make-low low cut cut2)
            (cond
              [(empty? low) empty]
              [else (append (find-widget-hard-make (first low) cut cut2) (hard-make-low (rest low) cut cut2))]
              )
  )

(check-expect (find-widget-hard-make Wire 4 4) (list Wire))
(check-expect (find-widget-hard-make Wire 3 4) (list Wire))
(check-expect (find-widget-hard-make Wire 4 5) (list Wire))
(check-expect (find-widget-hard-make Wire 3 5) empty)
(check-expect (find-widget-hard-make Telephone 9 8) (list Telephone Buttons Cord Wire))
(check-expect (find-widget-hard-make Telephone 99 0) (list Telephone Receiver Buttons Numbers Cord Wire))
(check-expect (find-widget-hard-make Telephone 0 99) empty) 