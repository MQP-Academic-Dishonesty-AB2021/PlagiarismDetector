

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |traffic signal starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))

(define TICKS-SECOND 28) 






(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 

  

(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))










(define-struct trafficLight(timePassed counter lightLit))

 






 
                      


(define L0 (make-trafficLight 0 0 2))
(define L1 (make-trafficLight 0 (* 5 TICKS-SECOND) 1))
(define L2 (make-trafficLight 0 (* 8 TICKS-SECOND) 0))
(define L3 (make-trafficLight 0 (* 10 TICKS-SECOND) 0))






(define-struct world (tlight listOfAutos))

 
  

(define (main w)
  (big-bang w
    (on-tick alter-world)    
    (to-draw render)))       

(define startLight(make-trafficLight 0 0 2))
(define START (make-world startLight empty))







(define (alter-world w)
  (make-world
   (change-light (world-tlight w))
   (produce-auto w (random 100))))





(check-expect (render START) (place-images(append (list (render-light (world-tlight START)) (text-display (world-tlight START)))
                                                  (render-ImgList (world-listOfAutos START))) 
                                          (append (list (make-posn X-POS Y-POS) (make-posn (+ 75 X-POS) Y-POS)) (render-PosList (world-listOfAutos START)))
                                          MTS))

(define (render w) 
  (place-images(append (list (render-light (world-tlight w)) (text-display (world-tlight w)))
                       (render-ImgList (world-listOfAutos w)))
               (append (list (make-posn X-POS Y-POS) (make-posn (+ 75 X-POS) Y-POS)) (render-PosList (world-listOfAutos w)))
               MTS)) 

(define (text-display tlight)
  (text (number->string(ticks->seconds(trafficLight-timePassed tlight))) 16 "black"))








(check-expect (change-light L0) (make-trafficLight (+ 1 (trafficLight-timePassed L0)) (+ 1 (trafficLight-counter L0)) (trafficLight-lightLit L0)))
(check-expect (change-light L1) (make-trafficLight (+ 1 (trafficLight-timePassed L1)) (+ 1 (trafficLight-counter L1)) (trafficLight-lightLit L1)))
(check-expect (change-light L2) (make-trafficLight (+ 1 (trafficLight-timePassed L2)) (+ 1 (trafficLight-counter L2)) (trafficLight-lightLit L2)))
(check-expect (change-light L3) (make-trafficLight (+ 1 (trafficLight-timePassed L3)) (+ 1 (trafficLight-counter L3)) (trafficLight-lightLit L3)))

  
(define (change-light tlight)
  (cond [(and(<= 0 (ticks->seconds(trafficLight-counter tlight)))
             (> (+ 0 GREEN-LENGTH) (ticks->seconds(trafficLight-counter tlight))))
         (make-trafficLight (+ 1 (trafficLight-timePassed tlight)) (+ 1 (trafficLight-counter tlight)) 2)]
        
        [(and(<= YELLOW-LENGTH (ticks->seconds (trafficLight-counter tlight)))
             (> (+ YELLOW-LENGTH GREEN-LENGTH) (ticks->seconds (trafficLight-counter tlight))
                )) 
         (make-trafficLight (+ 1 (trafficLight-timePassed tlight)) (+ 1 (trafficLight-counter tlight)) 1)]
        
        [(and(<= (+ YELLOW-LENGTH GREEN-LENGTH)(ticks->seconds (trafficLight-counter tlight)))
             (> (+ RED-LENGTH YELLOW-LENGTH GREEN-LENGTH) (ticks->seconds (trafficLight-counter tlight))
                ))
         (make-trafficLight (+ 1 (trafficLight-timePassed tlight)) (+ 1 (trafficLight-counter tlight)) 0)]
        [else
         (make-trafficLight (+ 1 (trafficLight-timePassed tlight)) 0 2 )] 
        ))
 
 

(define RED-LIGHT
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))

(define YELLOW-LIGHT
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green"))) 

(define GREEN-LIGHT 
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

(define listOfImages empty)
(define listOfPos empty)






(check-expect (render-light L0)  GREEN-LIGHT)
(check-expect (render-light L1)  YELLOW-LIGHT)
(check-expect (render-light L2)  RED-LIGHT)
(check-expect (render-light L3)  RED-LIGHT)            

(define (render-light tlight)
  (cond [(= 0 (trafficLight-lightLit tlight))   
         RED-LIGHT]         
        [(= 1 (trafficLight-lightLit tlight))    
         YELLOW-LIGHT]
        [(= 2 (trafficLight-lightLit tlight))    
         GREEN-LIGHT]))


 






(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)

(define YCAP (+ Y-POS (* 3 LIGHT-RADIUS)))
(define SPEED-CAP 10)

 





(define-struct auto (x y speed maxSpeed img))

 






(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)





(define (pick-image val)
  (cond 
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)
 



(define (produce-auto w random-num)  
  (if (and(<= 0 random-num) (> 5 random-num))
      (cons (make-auto 0
                       (+ (random (- HEIGHT YCAP)) YCAP)
                       0
                       (+ 1 (random SPEED-CAP))
                       (pick-image (random 3))) (world-listOfAutos w))
      (move-auto (world-tlight w) (world-listOfAutos w))))



(define auto0 (make-auto 0 505 0 10 AUTO-IMAGE1))
(define auto1 (make-auto (- X-POS 5) 500 0 9 AUTO-IMAGE1))
(define auto2 (make-auto X-POS 400 0 7 AUTO-IMAGE2))
(define auto3 (make-auto (+ X-POS 5) 450 0 4 AUTO-IMAGE3))

(check-expect (render-ImgList empty) empty) 
(check-expect (render-ImgList (list auto0 auto1 auto2)) (list (auto-img auto0) (auto-img auto1) (auto-img auto2)))
(check-expect (render-ImgList (list auto1 auto3 auto2)) (list (auto-img auto1) (auto-img auto3) (auto-img auto2)))

(define (render-ImgList listOfAuto)
  (cond[(empty? listOfAuto) empty]
       [else
        (cons (auto-img (first listOfAuto)) (render-ImgList (rest listOfAuto)))]))



(check-expect (render-PosList empty) empty)
(check-expect (render-PosList (list auto0 auto1 auto2)) (list (make-posn (auto-x auto0) (auto-y auto0))
                                                              (make-posn (auto-x auto1) (auto-y auto1))
                                                              (make-posn (auto-x auto2) (auto-y auto2))))
(check-expect (render-PosList (list auto1 auto3 auto2)) (list (make-posn (auto-x auto1) (auto-y auto1))
                                                              (make-posn (auto-x auto3) (auto-y auto3))
                                                              (make-posn (auto-x auto2) (auto-y auto2))))

(define (render-PosList listOfAuto)
  (cond[(empty? listOfAuto) empty]
       [else
        (cons(make-posn (auto-x (first listOfAuto)) (auto-y (first listOfAuto)))
             (render-PosList (rest listOfAuto)))]))
  
 



(check-expect (move-auto L3 (list auto1 auto2 auto3))
              (list auto1 auto2 (make-auto (auto-x auto3)
                                           (auto-y auto3)
                                           (auto-maxSpeed auto3)
                                           (auto-maxSpeed auto3)
                                           (auto-img auto3))))
(check-expect (move-auto L0 (list auto0 auto1 auto2))
              (list (make-auto (auto-x auto0)
                               (auto-y auto0)
                               (auto-maxSpeed auto0)
                               (auto-maxSpeed auto0)
                               (auto-img auto0))
                    (make-auto (auto-x auto1)
                               (auto-y auto1)
                               (auto-maxSpeed auto1)
                               (auto-maxSpeed auto1)
                               (auto-img auto1))
                    (make-auto (auto-x auto2)
                               (auto-y auto2)
                               (auto-maxSpeed auto2)
                               (auto-maxSpeed auto2)
                               (auto-img auto2))))

 
(define (move-auto tlight listOfAutos)
  (cond [(empty? listOfAutos) empty]
        [else 
         (cons (make-auto (+ (auto-x (first listOfAutos)) (auto-speed (first listOfAutos)))
                          (auto-y (first listOfAutos))
                          (auto-speed (change-speed tlight (first listOfAutos)))
                          (auto-maxSpeed (first listOfAutos))
                          (auto-img (first listOfAutos)))
               (move-auto tlight (rest listOfAutos)))]))
    
 







(check-expect (change-speed L3 auto0) (make-auto (auto-x auto0) (auto-y auto0) (* (auto-maxSpeed auto0) .5) (auto-maxSpeed auto0) (auto-img auto0)))
(check-expect (change-speed L0 auto1) (make-auto (auto-x auto1) (auto-y auto1) (auto-maxSpeed auto1) (auto-maxSpeed auto1) (auto-img auto1)))
(check-expect (change-speed L1 auto0) (make-auto (auto-x auto0) (auto-y auto0) (* (auto-maxSpeed auto0) .75) (auto-maxSpeed auto0) (auto-img auto0)))

(define (change-speed tlight a) 
  (if (<= (auto-x a) X-POS)
      (cond [(= (trafficLight-lightLit tlight) 0)            
             (if(>= (+ (auto-x a) (auto-maxSpeed a)) X-POS)
                (make-auto (auto-x a) (auto-y a) 0 (auto-maxSpeed a) (auto-img a))
                (make-auto (auto-x a) (auto-y a) (* (auto-maxSpeed a) .5) (auto-maxSpeed a) (auto-img a)))]

            [(= (trafficLight-lightLit tlight) 1)            
             (make-auto (auto-x a) (auto-y a) (* (auto-maxSpeed a) .75) (auto-maxSpeed a) (auto-img a))]
        
            [(= (trafficLight-lightLit tlight) 2)
             (make-auto (auto-x a) (auto-y a) (auto-maxSpeed a) (auto-maxSpeed a) (auto-img a))])

      (make-auto (auto-x a) (auto-y a) (auto-maxSpeed a) (auto-maxSpeed a) (auto-img a))))
  
 
  



(main START)
