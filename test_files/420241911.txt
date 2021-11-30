

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment2-Traffic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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
(define CYCLE-LENGTH (+ RED-LENGTH YELLOW-LENGTH GREEN-LENGTH))

(define AUTO-SPAWN-RATE 5) 



(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))


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







(define MIN-Y (+ Y-POS (/ (image-height BULBS) 2) 10))
(define MIN-SPEED 2)
(define MAX-SPEED 15)

(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)










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

















(define-struct world-state (ticks autos))

(define (fn-for-world-state ws)
  (... (world-state-ticks ws)
       (fn-for-listofauto (world-state-autos ws))))

(define (fn-for-loa loa)
  (... (if (empty? loa) ...
           (... (fn-for-auto (first loa))
                (fn-for-loa (rest loa))))))


(define START (make-world-state 0 empty)) 


(define (main ws)
  (big-bang ws
    (on-tick update)
    (to-draw draw)))







(check-expect (light-from-ticks (* TICKS-SECOND CYCLE-LENGTH)) GREEN-LIGHT)
(check-expect (light-from-ticks (* TICKS-SECOND GREEN-LENGTH)) YELLOW-LIGHT)
(check-expect (light-from-ticks (* TICKS-SECOND (+ GREEN-LENGTH YELLOW-LENGTH 1))) RED-LIGHT)

(define (light-from-ticks ticks)
  (light-from-seconds (ticks->seconds ticks)))







(check-expect (light-from-seconds CYCLE-LENGTH) GREEN-LIGHT)
(check-expect (light-from-seconds GREEN-LENGTH) YELLOW-LIGHT)
(check-expect (light-from-seconds (+ GREEN-LENGTH YELLOW-LENGTH 1)) RED-LIGHT)

(define (light-from-seconds secs)
  (if (>= secs CYCLE-LENGTH)
      (light-from-seconds (modulo secs CYCLE-LENGTH))
      (cond
        [(< secs GREEN-LENGTH) GREEN-LIGHT]
        [(< secs (+ GREEN-LENGTH YELLOW-LENGTH)) YELLOW-LIGHT]
        [else RED-LIGHT])))







(check-expect (write-time 35)
              (text "1" 24 "black"))
(check-expect (write-time 280)
              (text "10" 24 "black"))
(check-expect (write-time 75)
              (text "2" 24 "black"))
(check-expect (write-time 120)
              (text "4" 24 "black"))

(define (write-time ticks)
  (text
   (number->string (ticks->seconds ticks)) 24 "black"))





(define (draw ws)
  (draw-autos (world-state-autos ws)
              (place-image
               (write-time (world-state-ticks ws)) (+ X-POS 100) Y-POS
               (place-image
                (light-from-ticks (world-state-ticks ws)) X-POS Y-POS MTS))))









(define (update ws)
  (make-world-state
   (add1 (world-state-ticks ws))
   (if (< (random 100) AUTO-SPAWN-RATE)
       (add-random-auto (update-autos (world-state-autos ws) (world-state-ticks ws)))
       (update-autos (world-state-autos ws) (world-state-ticks ws)))))


(define-struct auto (image x y init-speed))








(define (fn-for-auto auto)
  (... (auto-image auto)
       (auto-x auto)
       (auto-y auto)
       (auto-init-speed auto)))








(define A1 (make-auto (pick-image 0) 0 50 5))                     
(define A11 (make-auto (pick-image 0) 5 50 5))
(define A12 (make-auto (pick-image 0) 3.75 50 5))
(define A13 (make-auto (pick-image 0) 2.5 50 5))

(define A2 (make-auto (pick-image 1) 20 25 5))                    
(define A21 (make-auto (pick-image 1) 25 25 5))
(define A22 (make-auto (pick-image 1) 23.75 25 5))
(define A23 (make-auto (pick-image 1) 22.5 25 5))

(define A3 (make-auto (pick-image 2) WIDTH 50 5))                 
(define A31 (make-auto (pick-image 2) (+ WIDTH 5) 50 5))
(define A32 A31)
(define A33 A31)

(define A4 (make-auto (pick-image 0) (+ WIDTH 10) 50 5))          
(define A41 (make-auto (pick-image 0) (+ WIDTH 15) 50 5))
(define A42 A41)
(define A43 A41)

(define A5 (make-auto (pick-image 1) X-POS 50 5))           
(define A51 (make-auto (pick-image 1) (+ X-POS 5) 50 5))
(define A52 (make-auto (pick-image 1) (+ X-POS 3.75) 50 5))
(define A53 A5)


(define LOA1 (list A1 A2))
(define LOA11 (list A11 A21))
(define LOA12 (list A12 A22))
(define LOA13 (list A13 A23))

(define LOA3 (list A3 A4))
(define LOA31 (list A31))  
(define LOA32 (list A32))
(define LOA33 (list A33))

(define LOA5 (list A5))
(define LOA51 (list A51))
(define LOA52 (list A52))
(define LOA53 (list A53))






(define (add-random-auto loa)
  (cons (make-auto
         (pick-image (random 3))
         0
         (+ (random (- HEIGHT MIN-Y 20)) MIN-Y) 
         (+ (random (- MAX-SPEED MIN-SPEED)) MIN-SPEED))
        loa))







(check-expect (off-screen? A1) false)
(check-expect (off-screen? A2) false)
(check-expect (off-screen? A3) false)
(check-expect (off-screen? A4) true)

(define (off-screen? auto)
  (> (auto-x auto) WIDTH))










(check-expect (update-autos LOA1 31) LOA11)  
(check-expect (update-autos LOA3 25) LOA31)  
(check-expect (update-autos LOA5 25) LOA51)  

(check-expect (update-autos LOA1 168) LOA12) 
(check-expect (update-autos LOA3 168) LOA32) 
(check-expect (update-autos LOA5 168) LOA52) 

(check-expect (update-autos LOA1 200) LOA13) 
(check-expect (update-autos LOA3 200) LOA33) 
(check-expect (update-autos LOA5 200) LOA53) 


(define (update-autos loa ticks)
  (cond [(empty? loa) empty]
        [(off-screen? (first loa)) (update-autos (rest loa) ticks)]
        [else
         (local
           [(define auto (first loa))
            (define mult (get-multiplier auto ticks))]
           (cons
            (make-auto
             (auto-image auto)
             (+ (auto-x auto) (* (auto-init-speed auto) mult))
             (auto-y auto)
             (auto-init-speed auto))
            (update-autos (rest loa) ticks)))]))






(check-expect (get-multiplier A2 50) 1)     
(check-expect (get-multiplier A2 168) .75)  
(check-expect (get-multiplier A2 200) .5)   

(check-expect (get-multiplier A5 200) 0)    

(check-expect (get-multiplier A3 50) 1)     
(check-expect (get-multiplier A3 168) 1)    
(check-expect (get-multiplier A3 200) 1)    

(define (get-multiplier auto ticks)
  (if (>
       (- (auto-x auto) (* (auto-init-speed auto) .5)) X-POS)
      1
      (local
        [(define LIGHT (light-from-ticks ticks))]
        (cond
          [(image=? LIGHT GREEN-LIGHT) 1]
          [(image=? LIGHT YELLOW-LIGHT) .75]
          [(image=? LIGHT RED-LIGHT)
           (if (>= (auto-x auto) X-POS)
               0
               .5)]))))




(define (draw-autos loa img)
  (if (empty? loa)
      img
      (local
        [(define auto (first loa))
         (define auto-img (auto-image auto))]
        (if (empty? (rest loa))
            (place-image
             auto-img (auto-x auto) (auto-y auto) img)
            (place-image
             auto-img (auto-x auto) (auto-y auto) (draw-autos (rest loa) img))))))