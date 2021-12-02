

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


 





 


[define [fn-for-widget fn? wid]
  [local [[define [fn-for--widget wid]
            [cond [[fn? wid] [cons wid [fn-for--low [widget-parts wid]]]]
                  [else [fn-for--low [widget-parts wid]]]]]
          
          [define [fn-for--low low]
            [cond [[empty? low] empty]
                  [else
                   [append [fn-for--widget [first low]] 
                           [fn-for--low [rest low]]]]]]] 
    [fn-for--widget wid]]]






[check-expect [find-widget-name-longer-than Wire 5] empty]

[check-expect [find-widget-name-longer-than Wire 3]
              [list Wire]]
[check-expect [find-widget-name-longer-than Buttons 4]
              [list Buttons Numbers]]
[check-expect [find-widget-name-longer-than Telephone 4]
              [list Telephone Receiver Buttons Numbers]]



[define [find-widget-name-longer-than wid num]
  [fn-for-widget [λ [wid] [> [string-length [widget-name wid]] num]] wid]]








[check-expect [find-widget-quantity-over Wire 4] empty]

[check-expect [find-widget-quantity-over Wire 2] [list Wire]]
[check-expect [find-widget-quantity-over Telephone 2]
              [list Telephone Receiver Buttons Numbers Cord Wire]]
[check-expect [find-widget-quantity-over Telephone 8]
              [list Receiver Numbers]]



[define [find-widget-quantity-over wid num]
  [fn-for-widget [λ [wid] [> [widget-quantity wid] num]] wid]]







[check-expect [find-widgets-cheaper-than Wire 4] empty]

[check-expect [find-widgets-cheaper-than Wire 6] [list Wire]]
[check-expect [find-widgets-cheaper-than Telephone 16]
              [list Telephone Receiver Buttons Numbers Cord Wire]]
[check-expect [find-widgets-cheaper-than Telephone 6]
              [list Buttons Numbers Cord Wire]]



[define [find-widgets-cheaper-than wid num]
  [fn-for-widget [λ [wid] [< [widget-price wid] num]] wid]]






[check-expect [find-widget-hard-make Wire 0 10] empty]


[check-expect [find-widget-hard-make Wire 5 7] [list Wire]] 
[check-expect [find-widget-hard-make Telephone 2 10]
              [list Telephone]]

[check-expect [find-widget-hard-make  Telephone 5 4]
              [list Telephone Receiver Buttons Numbers Cord Wire]]

[check-expect [find-widget-hard-make  Telephone 6 6]
              [list Telephone Receiver Wire]]




[define [find-widget-hard-make wid num num2]
  [fn-for-widget [λ [wid]
                   [or [< [widget-quantity wid] num]
                       [> [widget-price wid] num2]]] wid]]
































[define [qsort fn? property]
  [local
    [[define [qsort wid]
       [local [[define listW [genList wid]]
               [define [qsort-inner low]
                 [cond
                   [[empty? low] empty]
                   [else
                    [local
                      [[define pivot [first low]]
                       [define [compare wid]
                         [fn? [property wid] [property pivot]]]]
                      
                      [append
                       [qsort-inner [filter compare [rest low]]]
                       [list pivot]
                       [qsort-inner
                        [filter [λ[wid]
                                  [not [compare wid]]]
                                
                                [rest low]]]]]]]]]
         [qsort-inner listW]]]]
    qsort]]



[check-expect [genList Wire] [list Wire]]

[check-expect [genList Buttons] [list Buttons Numbers]]
[check-expect [genList Telephone]
              [list Telephone Receiver Buttons Numbers Cord Wire]]

[define [genList wid]
  [local
    [[define [fn-for--widget wid]
       [cons wid
             [fn-for--low [widget-parts wid]]]] 
          
     [define [fn-for--low low]
       [cond [[empty? low] empty]
             [else
              [append [fn-for--widget [first low]] 
                      [fn-for--low [rest low]]]]]] 
     ]
    [fn-for--widget wid]]]          


[define sort-strings [qsort string<? widget-name]]

[check-expect [map widget-name [sort-strings Wire]]
              [list "Wire"]]

(check-expect [map widget-name (sort-strings Buttons)]
              (list "Buttons" "Numbers"))
[check-expect [map widget-name [sort-strings Telephone]]
              (list "Buttons" "Cord" "Numbers" "Receiver"
                    "Telephone" "Wire")]



[define sort-overstocked [qsort > widget-quantity]]

[check-expect [map widget-name [sort-overstocked Numbers]]
              [list "Numbers"]]

[check-expect [map widget-name [sort-overstocked Necklace]]
              [list "Necklace" "Chain" "Pendant"]]
[check-expect [map widget-name [sort-overstocked Jewelry]]
              [list "Beads" "Rings" "Necklace" "Chain" "Glass"
                    "Bracelet" "Jewelry set" "Pendant"]]