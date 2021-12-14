

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname traffic_work) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)

(require 2htdp/image)   
(require 2htdp/universe)   
(define SCENE-WIDTH  800)   
(define SCENE-HEIGHT 600)
(define SCENE-CENTER (make-posn (/ SCENE-WIDTH 2) (/ SCENE-HEIGHT 2)))
(define MTS (empty-scene SCENE-WIDTH SCENE-HEIGHT "transparent"))




(define TICKS-SECOND        28)


(define SIGNAL-X-POS (/ SCENE-WIDTH 2))
(define SIGNAL-Y-POS (/ SCENE-HEIGHT 4))
(define SIGNAL-POS   (make-posn SIGNAL-X-POS SIGNAL-Y-POS))
(define BULB-RADIUS         40) 

(define GREEN-LIGHT-LENGTH  5) 
(define YELLOW-LIGHT-LENGTH 2) 
(define RED-LIGHT-LENGTH    4) 
(define SIGNAL-TIME-COLOR "black")  
(define SIGNAL-TIME-SIZE  25)

(define BULBS
  (above
   (circle BULB-RADIUS "outline" "red")  
   (circle BULB-RADIUS "outline" "yellow")  
   (circle BULB-RADIUS "outline" "green")))





(define (ticks->seconds ticks)   
  (floor (/ ticks TICKS-SECOND)))  
(check-expect (ticks->seconds 0)   (floor (/ 0 TICKS-SECOND)))   
(check-expect (ticks->seconds 28)  (floor (/ 28 TICKS-SECOND)))   
(check-expect (ticks->seconds 280) (floor (/ 280 TICKS-SECOND)))  
(check-expect (ticks->seconds 279) (floor (/ 279 TICKS-SECOND)))   
(check-expect (ticks->seconds 281) (floor (/ 281 TICKS-SECOND)))





(define (pick-image val)   
  (cond  
    [(= val 0) AUTO-IMAGE1]   
    [(= val 1) AUTO-IMAGE2]   
    [(= val 2) AUTO-IMAGE3]))   
(check-expect (pick-image 0) AUTO-IMAGE1)   
(check-expect (pick-image 1) AUTO-IMAGE2)   
(check-expect (pick-image 2) AUTO-IMAGE3)  
(check-error  (pick-image 3)) 





(define-struct light (color ticks))

(define (fn-for-light light)
  (... (light-color light)
       (light-ticks light)))





(define (update-light light)   
  (make-light  
   (choose-light-color (light-ticks light))  
   (add1 (light-ticks light))))
(check-expect
 (update-light (make-light "green" 0))
 (make-light "green" (add1 0)))
(check-expect
 (update-light
  (make-light
   (choose-light-color
    (+ GREEN-LIGHT-LENGTH YELLOW-LIGHT-LENGTH))
   (* TICKS-SECOND
      (+ GREEN-LIGHT-LENGTH YELLOW-LIGHT-LENGTH))))
 (make-light "red"
             (add1
              (* TICKS-SECOND
                 (+ GREEN-LIGHT-LENGTH YELLOW-LIGHT-LENGTH)))))








(define (in-exclusive-range n lo hi)  
  (and (<= lo n) (< n hi)))  
(check-expect (in-exclusive-range 2 1 3) true)  
(check-expect (in-exclusive-range 2 1 2) false) 
(check-expect (in-exclusive-range 2 1 1) false)  
(check-expect (in-exclusive-range 2 3 4) false) 
(check-expect (in-exclusive-range 2 3 2) false) 
(check-expect (in-exclusive-range 2 3 1) false)  





(define (choose-light-color now)   
  (local   
    {(define seconds
       (modulo
        (ticks->seconds now)
        (+ GREEN-LIGHT-LENGTH YELLOW-LIGHT-LENGTH RED-LIGHT-LENGTH)))}   
    (cond   
      [(in-exclusive-range seconds 0 GREEN-LIGHT-LENGTH)   
       "green"]   
      [(in-exclusive-range seconds GREEN-LIGHT-LENGTH
                           (+ GREEN-LIGHT-LENGTH YELLOW-LIGHT-LENGTH))  
       "yellow"]  
      [(in-exclusive-range seconds
                           (+ GREEN-LIGHT-LENGTH YELLOW-LIGHT-LENGTH)
                           (+ GREEN-LIGHT-LENGTH YELLOW-LIGHT-LENGTH RED-LIGHT-LENGTH))  
       "red"])))



(check-expect (choose-light-color
               (* TICKS-SECOND 0))
              "green") 
(check-expect (choose-light-color
               (* TICKS-SECOND GREEN-LIGHT-LENGTH))
              "yellow") 
(check-expect (choose-light-color
               (* TICKS-SECOND
                  (+ GREEN-LIGHT-LENGTH YELLOW-LIGHT-LENGTH)))
              "red")






(define (render-light light)   
  (place-images   
   (list   
    BULBS   
    (circle BULB-RADIUS "solid" (light-color light))   
    (text (number->string (ticks->seconds (light-ticks light))) SIGNAL-TIME-SIZE SIGNAL-TIME-COLOR))   
   (list   
    SIGNAL-POS
    (make-posn SIGNAL-X-POS (light-color-ypos (light-color light)))   
    (make-posn (+ SIGNAL-X-POS 100) SIGNAL-Y-POS))   
   MTS))




(define (light-color-ypos color)   
  (cond   
    [(string=? color "red")   
     (- SIGNAL-Y-POS (* 2 BULB-RADIUS))]   
    [(string=? color "yellow")   
     SIGNAL-Y-POS]   
    [(string=? color "green")  
     (+ SIGNAL-Y-POS (* 2 BULB-RADIUS))]))   
(check-expect (light-color-ypos "red")
              (- SIGNAL-Y-POS (* 2 BULB-RADIUS)))   
(check-expect (light-color-ypos "yellow")
              SIGNAL-Y-POS)   
(check-expect (light-color-ypos "green")
              (+ SIGNAL-Y-POS (* 2 BULB-RADIUS)))


(define MAX-SPEED 20)






(define-struct auto (type speed pos-x pos-y))

(define (fn-for-auto auto)
  (... (auto-type auto)
       (auto-speed auto)
       (auto-pos-x auto)
       (auto-pos-y auto)))





(define (advance-auto a speed-multiplier)  
  (make-auto  
   (auto-type a)  
   (auto-speed a)  
   (+ (auto-pos-x a) (* speed-multiplier (auto-speed a)))  
   (auto-pos-y a))) 
(check-expect  
 (advance-auto  
  (make-auto  
   (pick-image 0)  
   10  
   0  
   0) 1)  
 (make-auto  
  (pick-image 0)  
  10  
  (+ 0 10)  
  0))  
(check-expect  
 (advance-auto  
  (make-auto  
   (pick-image 0)  
   0  
   0  
   0) 1) 
 (make-auto  
  (pick-image 0)  
  0  
  (+ 0 0)  
  0))




(define (right-of-light loa)
  (cond
    [(empty? loa) empty]
    [(in-exclusive-range
      (auto-pos-x (first loa))
      SIGNAL-X-POS
      (+ SCENE-WIDTH
         (image-height
          (auto-type (first loa)))))
     (cons
      (first loa)
      (right-of-light (rest loa)))]
    [else
     (right-of-light (rest loa))]))




(define (left-of-light loa)
  (cond
    [(empty? loa) empty]
    [(<= (auto-pos-x (first loa)) SIGNAL-X-POS)
     (cons
      (first loa)
      (left-of-light (rest loa)))]
    [else
     (left-of-light (rest loa))]))

(define LIST1 (list 
               (make-auto (pick-image 0) 0 10 200) 
               (make-auto (pick-image 1) 20 500 400) 
               (make-auto (pick-image 2) 20 0 10))) 
(define LIST2 (list  
               (make-auto (pick-image 0) 0 10 200) 
               (make-auto (pick-image 2) 20 0 10))) 
(check-expect (left-of-light empty) empty) 
(check-expect (left-of-light LIST1) LIST2)





(define (slow color loa) 
  (cond 
    [(empty? loa) empty] 
    [(string=? color "yellow") 
     (cons 
      (advance-auto (first loa) 0.75) 
      (slow color (rest loa)))] 
    [(string=? color "red") 
     (if (and
          (< (auto-pos-x (first loa)) SIGNAL-X-POS)
          (> (auto-pos-x (first loa)) (- SIGNAL-X-POS MAX-SPEED)))  
         (cons 
          (advance-auto (first loa) 0) 
          (slow color (rest loa))) 
         (cons 
          (advance-auto (first loa) 0.5) 
          (slow color (rest loa))))] 
    [(string=? color "green") 
     (cons 
      (advance-auto (first loa) 1)  
      (slow color (rest loa)))]))
(check-expect (slow "yellow" LIST2) (list  
                                     (make-auto (pick-image 0) 0 10 200) 
                                     (make-auto (pick-image 2) 20 15 10))) 
(check-expect (slow "red" LIST2) (list  
                                  (make-auto (pick-image 0) 0 10 200) 
                                  (make-auto (pick-image 2) 20 10 10))) 







(define (advance-autos loa)  
  (cond  
    [(empty? loa) empty]  
    [(cons? loa)
     (cons (advance-auto (first loa) 1)
           (advance-autos (rest loa)))])) 
(check-expect  
 (advance-autos  
  (list  
   (make-auto  
    (pick-image 0)  
    10  
    0  
    0)))  
 (list  
  (make-auto  
   (pick-image 0)  
   10  
   (+ 0 10)  
   0)))  
(check-expect  
 (advance-autos  
  (list  
   (make-auto  
    (pick-image 0)  
    0  
    0  
    0)))  
 (list  
  (make-auto  
   (pick-image 0)  
   0  
   (+ 0 0)  
   0)))  




(define (auto-img-position a)  
  (make-posn (auto-pos-x a) (auto-pos-y a)))  
(check-expect  
 (auto-img-position  
  (make-auto  
   (pick-image 0)  
   0  
   20  
   30))  
 (make-posn 20 30))  


(define (auto-types loa)  
  (cond  
    [(empty? loa) empty]  
    [(cons? loa)  
     (cons  
      (auto-type (first loa))  
      (auto-types (rest loa)))])) 



(define (auto-img-positions loa)  
  (cond  
    [(empty? loa) empty]  
    [(cons? loa)  
     (cons  
      (auto-img-position (first loa))  
      (auto-img-positions (rest loa)))]))  




(define (render-autos loa)  
  (place-images  
   (auto-types loa)  
   (auto-img-positions loa)  
   MTS))  
(check-expect  
 (render-autos  
  (list  
   (make-auto  
    (pick-image 0)  
    0  
    10  
    20)))  
 (place-images  
  (list (pick-image 0))  
  (list (make-posn 10 20))  
  MTS))  
(define-struct world (autos trafficlight))  





(define (tick-world ws)  
  (make-world  
   (append 
    (advance-autos (right-of-light (world-autos ws))) 
    (slow (light-color
           (update-light (world-trafficlight ws)))
          (left-of-light (world-autos ws))) 
    (cond  
      [(= 0 (random 20))  
       (list  
        
        
        
        
        
        (make-auto  
         (pick-image (random 3))  
         (add1 (random MAX-SPEED))  
         0 
         (+ (/ SCENE-HEIGHT 2) (random (/ SCENE-HEIGHT 2)))))]  
      [else null]))  
   (update-light (world-trafficlight ws)))) 
(define (render-world ws)  
  (place-images  
   (list  
    (render-light (world-trafficlight ws))  
    (render-autos (world-autos ws)))  
   (list SCENE-CENTER SCENE-CENTER)  
   MTS))  

(define START (make-world empty (make-light "green" 0)))   




(define (main initial-state)
  (big-bang initial-state
    (on-tick tick-world)
    (to-draw render-world)))  
(main START) 