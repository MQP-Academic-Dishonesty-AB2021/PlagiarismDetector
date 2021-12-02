

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment3-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





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









(define (get-subwidgets w)
  (local [
          (define (get-subwidgets--widget w)
            (cons w (get-subwidgets--low (widget-parts w))))
  
          (define (get-subwidgets--low low)
            (cond [(empty? low) empty]
                  [else
                   (append (get-subwidgets--widget (first low))
                           (get-subwidgets--low (rest low)))]))]
    (get-subwidgets--widget w)))









(define (find-widgets w condition?)
  (filter condition? (get-subwidgets w)))



(define (find-widget-name-longer-than w num)
  (find-widgets w (lambda (x)
                    (> (string-length (widget-name x)) num))))


(check-expect (find-widget-name-longer-than Wire 3) (list Wire))

(check-expect (find-widget-name-longer-than Wire 4) empty)

(check-expect (find-widget-name-longer-than Cord 3) (list Cord Wire))

(check-expect (find-widget-name-longer-than Cord 4) empty)

(check-expect (find-widget-name-longer-than Necklace 5) (list Necklace Pendant))

(check-expect (find-widget-name-longer-than Bracelet 4) (list Bracelet Beads Glass))
  


(define (find-widget-quantity-over w num)
  (find-widgets w (lambda (x)
                    (> (widget-quantity x) num))))
                
(check-expect (find-widget-quantity-over Wire 0) (list Wire))

(check-expect (find-widget-quantity-over Cord 10) empty)

(check-expect (find-widget-quantity-over Buttons 6) (list Buttons Numbers))

(check-expect (find-widget-quantity-over Chain 10) empty)

(check-expect (find-widget-quantity-over Bracelet 20) (list Beads))

(check-expect (find-widget-quantity-over Jewelry 12) (list Rings Beads))

         
         


(define (find-widgets-cheaper-than w num)
  (find-widgets w (lambda (x)
                    (< (widget-price x) num))))
         

(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))

(check-expect (find-widgets-cheaper-than Wire 5) empty)

(check-expect (find-widgets-cheaper-than Cord 6) (list Cord Wire))

(check-expect (find-widgets-cheaper-than Cord 5) empty)

(check-expect (find-widgets-cheaper-than Necklace 2) (list Chain Pendant))

(check-expect (find-widgets-cheaper-than Bracelet 6) (list Bracelet Glass))





(define (find-widget-hard-make w ntl num)
  (find-widgets w (lambda (x)
                    (ishard? x ntl num))))

(check-expect (find-widget-hard-make Wire 0 0) empty)

(check-expect (find-widget-hard-make Cord 5 5) empty)

(check-expect (find-widget-hard-make Telephone 10 4)
              (list Telephone Buttons Numbers Cord Wire))

(check-expect (find-widget-hard-make Glass 0 0) empty)

(check-expect (find-widget-hard-make Necklace 30 30) empty)

            



(define (ishard? w ntl num)
  (and (< (widget-quantity w) ntl)
       (> (widget-price w) num)))

(check-expect (ishard? Wire 0 0) false)

(check-expect (ishard? Cord 5 5) false)

(check-expect (ishard? Buttons 10 10) false)

(check-expect (ishard? Telephone 7 10) true)








(define (qsort-helper arr lt?)
  (cond [(<= (length arr) 1) arr]
        [else (local [(define pivot (first arr))]
                (append
                 (qsort-helper (filter
                                (lambda (x) (lt? x pivot))
                                (rest arr))
                               lt?)
                 (list pivot)
                 (qsort-helper (filter
                                (lambda (x) (not (lt? x pivot)))
                                (rest arr))
                               lt?)))]))

(check-expect (qsort-helper empty <) empty)
(check-expect (qsort-helper (list 1 3 2 4) <) (list 1 2 3 4))
(check-expect (qsort-helper (list 1 3 2 4) >) (list 4 3 2 1))





(define (qsort lt? field)
  (lambda (x)
    (qsort-helper
     (get-subwidgets x)
     (lambda (w1 w2) (lt? (field w1)
                          (field w2))))))





(define sort-strings (qsort string<? widget-name))

(check-expect (map widget-name (sort-strings Telephone))
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))
(check-expect (map widget-name (sort-strings Bracelet))
              (list "Beads" "Bracelet" "Glass"))
(check-expect (map widget-name (sort-strings Wire))
              (list "Wire"))