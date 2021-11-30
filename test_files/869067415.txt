

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment3_part_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(define-struct widget(name quantity time price parts))



(define (fn-for-widget widget)
  (local [(define (fn-for-low low)
            (...
             (cond
               [(empty? low) ...]
               [else
                (... (fn-for-widget (first low))
                     (fn-for-low (rest low)))])))]
    (...
     (widget-name widget)
     (widget-quantity widget)
     (widget-time widget)
     (widget-price widget)
     (fn-for-low (widget-parts widget)))))
      


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







(define (get-widgets widget fn? param)
  (local [(define (get-widgets-list low fn? param)
            (cond
              [(empty? low) empty]
              [else
               (append (get-widgets (first low) fn? param)
                       (get-widgets-list (rest low) fn? param))]))]
    (if (fn? widget param)
        (cons widget (get-widgets-list (widget-parts widget) fn? param))
        (get-widgets-list (widget-parts widget) fn? param)
        )))





  



(check-expect (find-widget-name-longer-than Wire 10) empty)


(check-expect (find-widget-name-longer-than Wire 4) empty)


(check-expect (find-widget-name-longer-than Telephone 5)
              (list Telephone Receiver Buttons Numbers))


(check-expect (find-widget-name-longer-than Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

(define (find-widget-name-longer-than widget len)
  (local [(define (longer-than widget len)
            (> (string-length (widget-name widget)) len))]
    (get-widgets widget longer-than len)))






(check-expect (find-widget-quantity-over Wire 2) (list Wire))


(check-expect (find-widget-quantity-over Wire 3) empty)


(check-expect (find-widget-quantity-over Telephone 4)
              (list Telephone Receiver Buttons Numbers Cord))


(check-expect (find-widget-quantity-over Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

  

(define (find-widget-quantity-over widget quantity)
  (local [(define (quantity-over widget quantity) 
            (> (widget-quantity widget) quantity))]
    (get-widgets widget quantity-over quantity)))










(check-expect (find-widgets-cheaper-than Wire 2) empty)


(check-expect (find-widgets-cheaper-than Wire 5) empty)


(check-expect (find-widgets-cheaper-than Wire 6) (list Wire))


(check-expect (find-widgets-cheaper-than Telephone 10)
              (list Receiver Buttons Numbers Cord Wire))


(check-expect (find-widgets-cheaper-than Jewelry 50)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

  

(define (find-widgets-cheaper-than widget price)
  (local [(define (cheaper-than widget price) 
            (< (widget-price widget) price))]
    (get-widgets widget cheaper-than price)))






(check-expect (find-widget-hard-make Wire 2) (list Wire))


(check-expect (find-widget-hard-make Wire 6) (list Wire))


(check-expect (find-widget-hard-make Telephone 10)
              (list Telephone Buttons Numbers Cord Wire))


(check-expect (find-widget-hard-make Jewelry 50)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))


(check-expect (find-widget-hard-make Jewelry 0)
              (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

  

(define (find-widget-hard-make widget hard)
  (local [(define (hard-make widget hard)
            (or (< (widget-quantity widget) hard)
                (> (widget-price widget) hard)))]
    (get-widgets widget hard-make hard)))














(check-expect (larger-than < widget-quantity Jewelry
                           (list Wire Telephone Pendant))
              (list Telephone))

(check-expect (larger-than > widget-quantity Jewelry
                           (list Wire Telephone Pendant))
              (list Wire))

(check-expect (larger-than < widget-time Wire empty) empty)

(define (larger-than comp field pivot low)
  (local [(define (comp2 widg)
            (comp (field pivot) (field widg)))]
    (filter comp2 low)))







  


(check-expect (smaller-than < image-height (square 20 "solid" "white")
                            (list (square 10 "solid" "white")
                                  (square 15 "solid" "white")
                                  (square 20 "solid" "white")
                                  (square 25 "solid" "white")))
              (list (square 10 "solid" "white") (square 15 "solid" "white")))

(check-expect (smaller-than < widget-quantity Jewelry
                            (list Wire Telephone Pendant))
              (list Wire))

(check-expect (smaller-than > widget-quantity Jewelry
                            (list Wire Telephone Pendant))
              (list Telephone))

(check-expect (smaller-than < widget-quantity Jewelry empty) empty)


(define (smaller-than comp field pivot low)
  (local [(define (comp2 widg)
            (comp (field widg) (field pivot)))]
    (filter comp2 low)))







  


(check-expect (equal-to < widget-quantity Jewelry
                        (list Wire Telephone Pendant)) (list Pendant))

(check-expect (equal-to > widget-quantity Jewelry
                        (list Wire Telephone Pendant)) (list Pendant))

(check-expect (equal-to > widget-time Cord (list Wire Numbers))
              (list Wire Numbers))

(check-expect (equal-to > widget-quantity Cord (list Wire Numbers)) empty)

(check-expect (equal-to > widget-quantity empty empty) empty)

(define (equal-to comp field pivot low)
  (cond [(empty? low) empty]
        [(not (or (comp (field pivot) (field (first low)))
                  (comp (field (first low)) (field pivot))))
         (cons (first low) (equal-to comp field pivot (rest low)))]
        [else (equal-to comp field pivot (rest low))]))





  


(check-expect (all-parts Telephone)
              (list Telephone Receiver Buttons Numbers Cord Wire))

(check-expect (all-parts (widget-parts Jewelry))
              (list Rings Necklace Chain Pendant Bracelet Beads Glass))

(check-expect (all-parts Wire) (list Wire))

(check-expect (all-parts empty) empty)

(define (all-parts widgetOrList)
  (cond [(empty? widgetOrList) empty]
        [(list? widgetOrList)
         (append (all-parts (first widgetOrList))
                 (all-parts (rest widgetOrList)))]
        [else (cons widgetOrList
                    (all-parts (widget-parts widgetOrList)))]))







  


(check-expect ((qsort < widget-price) Jewelry)
              (list Chain Pendant Necklace Glass Bracelet Beads Rings Jewelry))

(check-expect ((qsort > widget-quantity) Telephone)
              (list Receiver Numbers Buttons Cord Telephone Wire))

(check-expect ((qsort > widget-quantity) Necklace)
              (list Necklace Chain Pendant))

(check-expect ((qsort string<? widget-name) Telephone)
              (list Buttons Cord Numbers Receiver Telephone Wire))


(define (qsort comp field)
  (local [(define (qs-temp widgetOrList)
            (cond [(widget? widgetOrList)
                   (append
                    (qs-temp (smaller-than comp field widgetOrList
                                           (all-parts
                                            (widget-parts widgetOrList))))
                    (list widgetOrList)
                    (equal-to comp field widgetOrList
                              (all-parts (widget-parts widgetOrList)))
                    (qs-temp (larger-than comp field widgetOrList
                                          (all-parts
                                           (widget-parts widgetOrList)))))]
                  [(empty? widgetOrList) empty]
                  [else
                   (append
                    (qs-temp (smaller-than comp field (first widgetOrList)
                                           (rest widgetOrList)))
                    (list (first widgetOrList))
                    (equal-to comp field (first widgetOrList)
                              (rest widgetOrList))
                    (qs-temp (larger-than comp field (first widgetOrList)
                                          (rest widgetOrList))))]))]
    qs-temp))



(define sort-strings (qsort string<? widget-name))
(map widget-name (sort-strings Telephone))



(define sort-overstocked (qsort > widget-quantity))
(map widget-name (sort-overstocked Necklace))