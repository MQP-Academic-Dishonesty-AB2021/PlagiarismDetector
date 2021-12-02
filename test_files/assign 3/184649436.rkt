

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
  (local [
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
                        (fn-for-Low (rest Low)))]))]

    (fn-for-widget widget)))





(define (filter-widgets widget nat fn-for-widget?)
  (local [
          (define (filter-widget widget)
            (if (fn-for-widget? widget nat)
            ( cons widget (filter-Low(widget-parts widget)))
            (filter-Low(widget-parts widget))))

          (define (filter-Low Low)
            (cond [(empty? Low) empty]
                  [else
                   (append (filter-widget (first Low))
                        (filter-Low (rest Low)))]))]

    (filter-widget widget)))





(define (find-widget-name-longer-than widget nat)
  (filter-widgets widget
                  nat
                  (lambda(widget nat)
                    (> (string-length (widget-name widget)) nat))))





(define (find-widget-quantity-over widget nat)
  (filter-widgets widget
                  nat
                  (lambda(widget nat)
                    (> (widget-quantity widget) nat))))




(define (find-widgets-cheaper-than widget nat)
  (filter-widgets widget
                  nat
                  (lambda(widget nat)
                    (< (widget-price widget) nat))))




(define (find-widget-hard-make widget nat)
  (filter-widgets widget
                  nat
                  (lambda(widget nat)
                    (or (< (widget-quantity widget) nat) (> (widget-price widget) nat)))))

(check-expect (find-widget-name-longer-than Beads 4) (list Beads Glass))
(check-expect (find-widget-name-longer-than Glass 10) empty)
(check-expect (find-widget-name-longer-than Jewelry 6) (list Jewelry Necklace Pendant Bracelet))
(check-expect (find-widget-name-longer-than Jewelry 10) (list Jewelry))

(check-expect (find-widget-quantity-over Beads 4) (list Beads Glass))
(check-expect (find-widget-quantity-over Glass 10) empty)
(check-expect (find-widget-quantity-over Jewelry 6) (list Rings Necklace Chain Beads))
(check-expect (find-widget-quantity-over Jewelry 10) (list Rings Beads))

(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))
(check-expect (find-widgets-cheaper-than Glass 2) empty)
(check-expect (find-widgets-cheaper-than Buttons 6) (list Buttons Numbers))
(check-expect (find-widgets-cheaper-than Jewelry 10) (list Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (find-widget-hard-make Wire 6) (list Wire))
(check-expect (find-widget-hard-make Glass 2) (list Glass))
(check-expect (find-widget-hard-make Buttons 6) empty)
(check-expect (find-widget-hard-make Jewelry 10) (list Jewelry Rings Chain Pendant Bracelet Glass))




(define (qsort comparator property)
  (lambda (topwidget)
       (local [
          (define (get-widget-parts widget)
            (cons widget
                 (widget-parts-from-loW (widget-parts widget))
                 ))

          (define (widget-parts-from-loW Low)
            (cond [(empty? Low) empty]
                  [else
                   (append (get-widget-parts (first Low))
                        (widget-parts-from-loW (rest Low)))]))
          
          (define (qsort loW)
            (if (empty? loW) empty
            (local[(define pivot (first loW))
                   (define closed-compare (lambda(widget) (comparator (property widget) (property pivot))))]
              (append (qsort (filter closed-compare (rest loW)))
                      (list pivot)
                      (qsort (filter (lambda(widget) (not (closed-compare widget))) (rest loW)) ))
           )))]

   (qsort (get-widget-parts topwidget)))))
                   


(define sort-strings (qsort string<? widget-name))

(check-expect (map widget-name (sort-strings Telephone))
(list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))




(define sort-overstocked (qsort > widget-quantity))

(check-expect (map widget-name (sort-overstocked Necklace))
(list "Necklace" "Chain" "Pendant"))


