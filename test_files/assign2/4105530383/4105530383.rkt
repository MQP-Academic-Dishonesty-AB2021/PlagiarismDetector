

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |traffic signal starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))








 WARMUP-Assignment2_Adam_Kalayjian_Aidan_Languedoc_CS1102A
 TRAFFIC-Assignment2_Adam_Kalayjian_Aidan_Languedoc_CS1102A

(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))
(define RED-SLOWDOWN-AMOUNT 0.5)
(define YELLOW-SLOWDOWN-AMOUNT 0.75)
(define TICKS-SECOND 28) 








(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 





(check-expect (draw-bulbs 0) (above (circle LIGHT-RADIUS "solid" "black")
                                    (circle LIGHT-RADIUS "solid" "black")
                                    (circle LIGHT-RADIUS "solid" "green")))
(check-expect (draw-bulbs 1) (above (circle LIGHT-RADIUS "solid" "black")
                                    (circle LIGHT-RADIUS "solid" "yellow")
                                    (circle LIGHT-RADIUS "solid" "black")))
(check-expect (draw-bulbs 2) (above (circle LIGHT-RADIUS "solid" "red")
                                    (circle LIGHT-RADIUS "solid" "black")
                                    (circle LIGHT-RADIUS "solid" "black")))

(define (draw-bulbs signal)
  (above
   (circle LIGHT-RADIUS "solid" (if (= signal 2) "red" "black"))
   (circle LIGHT-RADIUS "solid" (if (= signal 1) "yellow" "black"))
   (circle LIGHT-RADIUS "solid" (if (= signal 0) "green" "black"))))













(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)

(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))







(check-expect (progress-light 10) 0) 

(check-expect (progress-light (* 28 (+ GREEN-LENGTH))) 1)  

(check-expect (progress-light (* 28 (+ GREEN-LENGTH
                                       YELLOW-LENGTH))) 2)

(check-expect (progress-light (* 28 (+ GREEN-LENGTH
                                       YELLOW-LENGTH
                                       RED-LENGTH))) 0)

(define (progress-light tick)
  (local [(define seconds
            (modulo (ticks->seconds
                     tick)
                    (+ GREEN-LENGTH (+ YELLOW-LENGTH RED-LENGTH))))]
     (cond [(< seconds GREEN-LENGTH) 0]
           [(< seconds (+ GREEN-LENGTH YELLOW-LENGTH)) 1]
           [else 2])))







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)
(define AUTO-WIDTH (image-width AUTO-IMAGE1))













(define-struct auto (x y max-speed speed color))

(define (fn-for-auto aut1)
 (...
  (auto-x aut1)
  (auto-y aut1)
  (auto-max-speed aut1)
  (auto-speed aut1)
  (auto-color aut1)))






  

(check-expect (move-auto (make-auto 0 300 5 5 1) 0 400)
              (make-auto 5 300 5 5 1))

(check-expect (move-auto (make-auto 500 300 5 5 1) 0 400)
              (make-auto 505 300 5 5 1))

(check-expect (move-auto (make-auto 300 300 5 5 1) 2 400)
              (make-auto 305 300 5 (* 5 RED-SLOWDOWN-AMOUNT) 1))

(check-expect (move-auto (make-auto 300 300 5 5 1) 1 400)
              (make-auto 305 300 5 (* 5 YELLOW-SLOWDOWN-AMOUNT) 1))

(check-expect (move-auto (make-auto 500 300 5 5 1) 2 400)
              (make-auto 505 300 5 5 1))

(check-expect (move-auto (make-auto 399 300 5 (* 5 RED-SLOWDOWN-AMOUNT) 1) 2 400)
              (make-auto 400 300 5 0 1))

(define (move-auto auto light-state light-pos)
  (cond
    
    [(and (< (auto-speed auto) (auto-max-speed auto)) 
              (and
               (>= (+ (auto-speed auto) (auto-x auto)) light-pos)
               (= light-state 2)))
         (make-auto
          light-pos
          (auto-y auto)
          (auto-max-speed auto)
          0
          (auto-color auto))]
         
        [(and (< (auto-x auto) light-pos) (= light-state 1))
         (make-auto
          (+ (auto-x auto) (auto-speed auto))
          (auto-y auto)
          (auto-max-speed auto)
          (* (auto-max-speed auto) YELLOW-SLOWDOWN-AMOUNT)
          (auto-color auto))]
        
        [(and (< (auto-x auto) light-pos) (= light-state 2)) 
         (make-auto
          (+ (auto-x auto) (auto-speed auto))
          (auto-y auto)
          (auto-max-speed auto)
          (* (auto-max-speed auto) RED-SLOWDOWN-AMOUNT)
          (auto-color auto))]
        
        [else  
         (make-auto
          (+ (auto-x auto) (auto-max-speed auto))
          (auto-y auto)
          (auto-max-speed auto)
          (auto-max-speed auto)
          (auto-color auto))]))









(define (fn-for-loa loa)
  (cond [(empty? loa) (...)]
        [else
         (... (first loa)
              (fn-for-loa (rest loa)))]))






(define (new-auto loa)
  (local [(define spd (+ (random 20) 5))]
    (cons (make-auto
           0
           (+ (random (/ HEIGHT 2)) (/ HEIGHT 2))
           spd
           spd
           (random 3)) loa)))






(check-expect (move-all-autos (list
               (make-auto 100 300 5 5 1)
               (make-auto 100 400 3 3 1)
               (make-auto 100 500 20 20 1)) 0 300) 
              (list
               (make-auto 105 300 5 5 1)
               (make-auto 103 400 3 3 1)
               (make-auto 120 500 20 20 1))) 
(check-expect (move-all-autos (list
               (make-auto 100 300 5 5 1)
               (make-auto 100 400 3 3 1)
               (make-auto 100 500 20 20 1)) 1 300) 
              (list
               (make-auto 105 300 5 (* 5 YELLOW-SLOWDOWN-AMOUNT) 1)
               (make-auto 103 400 3 (* 3 YELLOW-SLOWDOWN-AMOUNT) 1)
               (make-auto 120 500 20 (* 20 YELLOW-SLOWDOWN-AMOUNT) 1))) 
(check-expect (move-all-autos empty 1 300) empty)

(define (move-all-autos loa light-state light-pos)
  (cond [(empty? loa) empty]
        [else
         (cons (move-auto (first loa) light-state light-pos)
               (move-all-autos (rest loa) light-state light-pos))]))







(check-expect (remove-autos (list (make-auto 4000 4000 5 5 2))) empty)
(check-expect (remove-autos (list
                             (make-auto 2 10 5 5 2)
                             (make-auto 4000 4000 5 5 2)))
              (list (make-auto 2 10 5 5 2)))
(check-expect (remove-autos empty) empty)

(define (remove-autos loa)
  (cond
    [(empty? loa) loa]
    [(> (+ WIDTH (/ AUTO-WIDTH 2)) (auto-x (first loa)))
     (cons (first loa) (remove-autos (rest loa)))]
    [else (remove-autos (rest loa))]))






(check-expect (draw-all-autos empty) MTS)
(check-expect (draw-all-autos (list
                               (make-auto 300 300 5 5 0)
                               (make-auto 320 400 5 5 1)
                               (make-auto 460 100 5 5 2)))
              (place-image
               (pick-image 0)
               300 300
               (place-image
               (pick-image 1)
               320 400
               (place-image
               (pick-image 2)
               460 100
               MTS))))
(check-expect (draw-all-autos (list
                               (make-auto 305.2 102.5 5 5 0)))
              (place-image
               (pick-image 0)
               305.2 102.5
               MTS))
                               
(define (draw-all-autos loa)
  (cond [(empty? loa) MTS]
        [else
         (place-image
          (pick-image (auto-color (first loa)))
          (auto-x (first loa))
          (auto-y (first loa))
          (draw-all-autos (rest loa)))]))








(define (add-new-autos loa rand)
  (if (<= rand 1)
      (new-auto loa)
      loa))









(define-struct road-state (tick light loa))

(define (fn-for-road-state rs)
 (...
  (road-state-tick rs)
  (road-state-light rs)
  (fn-for-loa (road-state-loa rs))))

(define TEST-ROAD-STATE (make-road-state
                         10
                         0
                         (list
                          (make-auto 100 400 5 5 1)
                          (make-auto 300 500 5 5 2))))





(check-expect (draw-road TEST-ROAD-STATE)
              (place-image (above
                (text "0" 24 "black")  
                (draw-bulbs 0)) X-POS Y-POS
                           (draw-all-autos (road-state-loa TEST-ROAD-STATE))))
(check-expect (draw-road (make-road-state 29 0 empty))
              (place-image (above
                (text "1" 24 "black")  
                (draw-bulbs 0)) X-POS Y-POS
                           MTS))

(define (draw-road rs)
  (place-image (above
                (text (number->string
                       (ticks->seconds (road-state-tick rs))) 24 "black")  
                (draw-bulbs (road-state-light rs))) X-POS Y-POS
               (draw-all-autos (road-state-loa rs))))






(define (progress-road rs)
  (make-road-state
   (add1 (road-state-tick rs))
   (progress-light (road-state-tick rs))
   (move-all-autos (add-new-autos
                    (remove-autos (road-state-loa rs)) (random 20))
                   (road-state-light rs) X-POS)))







(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)

(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))







(define (main c)
  (big-bang c
            (on-tick progress-road)  
            (to-draw draw-road)))    

(main (make-road-state 0 2 (new-auto empty)))