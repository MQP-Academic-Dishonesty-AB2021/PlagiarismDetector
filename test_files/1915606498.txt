

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(define-struct widget(name quantity time price parts))









(define Wire (make-widget "Wire" 3 5 3 empty))
(define Cord (make-widget "Cord" 7 5 2 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 4 (list Numbers)))
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


(define 1our (make-widget "1our" 3 5 3 empty))
(define 2our (make-widget "2our" 7 5 5 (list 1our)))
(define 3our (make-widget "3our" 9 5 5 empty))
(define Four (make-widget "Four" 8 5 5 (list 3our)))
(define 5our (make-widget "5our" 10 5 7 empty))
(define 6our (make-widget "6our" 5 20 15
                          (list 5our Four 2our)))
(define 7our (make-widget "7our" 5 20 10
                          (list 1our 3our)))
(define 8our (make-widget "8our" 7 20 10
                          (list 7our)))

(define eee (make-widget "eee" 5 20 3
                         (list 1our)))
(define thr (make-widget "thr" 5 20 3
                         (list eee)))


(define BASE (make-widget "" 0 0 0 empty))





 
      






(define (abstract-find-widget wid0 fn)
  (local
    [(define (fn-for-widget wid)
       (if (fn wid)
           (cons wid (fn-for-low (widget-parts wid)))
           (fn-for-low (widget-parts wid))))
           
     (define (fn-for-low low)
       (cond [(empty? low) empty]
             [else 
              (append (fn-for-widget (first low)) 
                      (fn-for-low (rest low)))]))] 
        
    (fn-for-widget wid0)))
      
      



(define (find-widget-hard-make wid stock cost)
  (abstract-find-widget wid 
                        (lambda (w) (or 
                                     (< (widget-quantity w) stock)
                                     (> (widget-price w) cost)))))



(define (find-widget-name-longer-than wid len)
  (abstract-find-widget wid 
                        (lambda (w) (> (string-length (widget-name w)) len))))



(define (find-widget-quantity-over wid stock)
  (abstract-find-widget wid 
                        (lambda (w) (> (widget-quantity w) stock))))



(define (find-widgets-cheaper-than wid cost)
  (abstract-find-widget wid 
                        (lambda (w) (< (widget-price w) cost))))





(check-expect (find-widget-name-longer-than eee 3) (list 1our))



(check-expect (find-widget-name-longer-than thr 3) (list 1our))



(check-expect (find-widget-name-longer-than Wire 5) empty)



(check-expect (find-widget-name-longer-than 7our 5) empty)
  


(check-expect (find-widget-name-longer-than 6our 5) empty)
  

(check-expect (find-widget-name-longer-than Wire 3) 
              (list Wire))
  


(check-expect (find-widget-name-longer-than 7our 3) (list 7our 1our 3our))



(check-expect (find-widget-name-longer-than Telephone 3) 
              (list Telephone Receiver Buttons Numbers Cord Wire))
  

(check-expect (find-widget-name-longer-than Wire 4) empty)



(check-expect (find-widget-name-longer-than Cord 4) empty)
  


(check-expect (find-widget-name-longer-than 6our 4) empty)


(check-expect (find-widget-name-longer-than BASE 1) empty)
  

  


(check-expect (find-widget-quantity-over Wire 100) empty)
  


(check-expect (find-widget-quantity-over Cord 100) empty)
  


(check-expect (find-widget-quantity-over Telephone 100) empty)
  

(check-expect (find-widget-quantity-over Wire 1) 
              (list Wire))



(check-expect (find-widget-quantity-over Cord 1) 
              (list Cord Wire))
  


(check-expect (find-widget-quantity-over Telephone 1) 
              (list Telephone Receiver Buttons Numbers Cord Wire))
  




(check-expect (find-widget-quantity-over Telephone 5) 
              (list Receiver Buttons Numbers Cord))
  




(check-expect (find-widget-quantity-over Necklace 7) 
              (list Necklace))
  
  

  
  


(check-expect (find-widgets-cheaper-than BASE 0) empty)



(check-expect (find-widgets-cheaper-than Four 5) empty)



(check-expect (find-widgets-cheaper-than thr 3) empty)



(check-expect (find-widgets-cheaper-than 7our 4) (list 1our))



(check-expect (find-widgets-cheaper-than 8our 4) (list 1our))

  





(check-expect (find-widget-hard-make BASE 0 0) empty)



(check-expect (find-widget-hard-make
               (make-widget "lv0" 5 5 5
                            (list (make-widget "lv1" 5 5 5 empty))) 5 5) empty)



(check-expect (find-widget-hard-make
               (make-widget "lv0" 5 5 5
                   (list (make-widget "lv1" 5 5 5
                        (list (make-widget "lv2" 5 5 5 empty))))) 5 5) empty)
                                          


(check-expect (find-widget-hard-make
               (make-widget "lv0" 5 5 5
                            (list (make-widget "lv1" 3 5 7 empty))) 5 5)
              (list (make-widget "lv1" 3 5 7 empty)))
                                                       


(check-expect (find-widget-hard-make
               (make-widget "lv0" 5 5 5
                   (list (make-widget "lv1" 5 5 5
                        (list (make-widget "lv2" 3 5 7 empty))))) 5 5)
              (list (make-widget "lv2" 3 5 7 empty)))









(define (qsort fn? widget-field)
        (local [
          
          
          (define (tree-to-list widget)
            (cons 
             widget
             (tree-to-list--list (widget-parts widget))))
                             
          
          
          (define (tree-to-list--list low)
            (cond [(empty? low) empty]
                  [else 
                   (append 
                    (tree-to-list (first low))
                    (tree-to-list--list (rest low)))]))]
                  
   (lambda (wid)
            (local [(define lox0 (tree-to-list wid))
                    
                    
                    (define (qsort-deep lox)
                      (cond [(empty? lox) empty]
                            [else 
                             (local
                               [(define pivot (first lox))
                                
                                (define (smaller? n)
                                  (fn? (widget-field n) (widget-field pivot)))]
                               (append
                                (qsort-deep (filter smaller? (rest lox)))               
                                (list pivot)
                                (qsort-deep
                                 (filter (λ(n) 
                                           (not (smaller? n)))
                                         (rest lox)))))]))]
              (qsort-deep lox0)))))


             


(define sort-strings (qsort string<? widget-name))     
(check-expect (map widget-name (sort-strings Telephone))
              (list "Buttons" "Cord" "Numbers" "Receiver" "Telephone" "Wire"))



(define sort-overstocked (qsort > widget-quantity))
(check-expect (map widget-name (sort-overstocked Necklace))
              (list "Necklace" "Chain" "Pendant"))


(check-expect (map widget-name ((qsort < widget-price) Telephone))
              (list "Cord" "Wire" "Buttons" "Numbers" "Receiver" "Telephone"))
                    

(check-expect (map widget-name ((qsort > widget-time) Cord))
              (list "Cord" "Wire"))