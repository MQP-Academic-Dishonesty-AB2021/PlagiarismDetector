

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


  
          
  






[check-expect [find-widget-name-longer-than Wire 5] empty]

[check-expect [find-widget-name-longer-than Wire 3] [list Wire]]
[check-expect [find-widget-name-longer-than Buttons 4]
              [list Buttons Numbers]]
[check-expect [find-widget-name-longer-than Telephone 4]
              [list Telephone Receiver Buttons Numbers]]




[define [find-widget-name-longer-than wid length]
  [cond [[> [string-length [widget-name wid]] length]
         [cons wid [find-widget-name-longer-than-low
                    [widget-parts wid] length]]]
        [else [find-widget-name-longer-than-low
               [widget-parts wid] length]]]]




[check-expect [find-widget-name-longer-than-low empty 4] empty]

[check-expect [find-widget-name-longer-than-low [list Wire] 3] [list Wire]]
[check-expect [find-widget-name-longer-than-low [list Wire Buttons] 3]
              [list Wire Buttons Numbers]]



[define [find-widget-name-longer-than-low low length]
  [cond [[empty? low] empty]
        [else
         [append [find-widget-name-longer-than
                  [first low] length] 
                 [find-widget-name-longer-than-low
                  [rest low] length]]]]] 








[check-expect [find-widget-quantity-over Wire 4] empty]

[check-expect [find-widget-quantity-over Wire 2] [list Wire]]
[check-expect [find-widget-quantity-over Telephone 2]
              [list Telephone Receiver Buttons Numbers Cord Wire]]
[check-expect [find-widget-quantity-over Telephone 8]
              [list Receiver Numbers]]



[define [find-widget-quantity-over wid quant]
  [cond [[> [widget-quantity wid] quant]
         [cons wid [find-widget-quantity-over-low
                    [widget-parts wid] quant]]]
        [else [find-widget-quantity-over-low [widget-parts wid] quant]]]]





[check-expect [find-widget-quantity-over-low empty 4] empty]

[check-expect [find-widget-quantity-over-low [list Wire] 2] [list Wire]]
[check-expect [find-widget-quantity-over-low [list Wire Buttons] 3]
              [list Buttons Numbers]]



[define [find-widget-quantity-over-low low quant]
  [cond [[empty? low] empty]
        [else
         [append [find-widget-quantity-over
                  [first low] quant] 
                 [find-widget-quantity-over-low
                  [rest low] quant]]]]] 








[check-expect [find-widgets-cheaper-than Wire 4] empty]

[check-expect [find-widgets-cheaper-than Wire 6] [list Wire]]
[check-expect [find-widgets-cheaper-than Telephone 16]
              [list Telephone Receiver Buttons Numbers Cord Wire]]
[check-expect [find-widgets-cheaper-than Telephone 6]
              [list Buttons Numbers Cord Wire]]



[define [find-widgets-cheaper-than wid price]
  [cond [[< [widget-price wid] price]
         [cons wid [find-widgets-cheaper-than-low
                    [widget-parts wid] price]]]
        [else [find-widgets-cheaper-than-low
               [widget-parts wid] price]]]]





[check-expect [find-widgets-cheaper-than-low empty 4] empty]

[check-expect [find-widgets-cheaper-than-low [list Wire] 6]
              [list Wire]]
[check-expect [find-widgets-cheaper-than-low [list Wire Buttons] 6]
              [list Wire Buttons Numbers]]
[check-expect [find-widgets-cheaper-than-low [list Receiver Buttons] 6]
              [list Buttons Numbers]]



[define [find-widgets-cheaper-than-low low price]
  [cond [[empty? low] empty]
        [else
         [append [find-widgets-cheaper-than
                  [first low] price] 
                 [find-widgets-cheaper-than-low
                  [rest low] price]]]]] 







[check-expect [find-widget-hard-make Wire 0 10] empty]


[check-expect [find-widget-hard-make Wire 5 7] [list Wire]]
[check-expect [find-widget-hard-make Telephone 2 10] [list Telephone]]

[check-expect [find-widget-hard-make  Telephone 5 4]
              [list Telephone Receiver Buttons Numbers Cord Wire]]

[check-expect [find-widget-hard-make  Telephone 6 6]
              [list Telephone Receiver Wire]]




[define [find-widget-hard-make wid Natural Number]
  [cond [[or [< [widget-quantity wid] Natural]
             [> [widget-price wid] Number]]
         [cons wid [find-widget-hard-make-low
                    [widget-parts wid] Natural Number]]]
        [else [find-widget-hard-make-low
               [widget-parts wid]  Natural Number]]]]




[check-expect [find-widget-hard-make-low empty 2 3] empty]

[check-expect [find-widget-hard-make-low [list Wire] 3 5] empty] 
[check-expect [find-widget-hard-make-low [list Wire Buttons] 5 2]
              [list Wire Buttons Numbers]]



[define [find-widget-hard-make-low low Natural Number]
  [cond [[empty? low] empty]
        [else
         [append [find-widget-hard-make
                  [first low]  Natural Number] 
                 [find-widget-hard-make-low
                  [rest low]  Natural Number]]]]] 