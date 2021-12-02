

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname assignment2part0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)


(define HEIGHT 600)
(define WIDTH 800)
(define COW .)
(define MTS (empty-scene WIDTH HEIGHT))
(define DISPLAY-X-POS 150)
(define DISPLAY-Y-POS 50)
(define TILT-SPEED 5)






(define-struct worldstate (x y laps speed tilt))






(define world1 (make-worldstate 200 200 10 10 0))




 

(define START (make-worldstate 0 300 0 1 0)) 

(define (main worldstate)
  (big-bang worldstate 
    (on-tick   move-cow)    
    (to-draw   draw-cow)    
    (on-key    accel-cow)   
    (on-mouse  tele-cow)))  






(check-expect (move-cow (make-worldstate 1000 50 5 10 0))
              (make-worldstate 0 50 6 10 0))
(check-expect (move-cow (make-worldstate 400 50 6 0 TILT-SPEED))
              (make-worldstate 400 50 6 0 TILT-SPEED))
(check-expect (move-cow (make-worldstate 400 50 6 50 0))
              (make-worldstate 450 50 6 50 (* -1 TILT-SPEED)))
(check-expect (move-cow (make-worldstate 500 40 6 50 (* -1 TILT-SPEED)))
              (make-worldstate 550 40 6 50 0))
(check-expect (move-cow (make-worldstate 400 50 6 6 TILT-SPEED))
              (make-worldstate 406 50 6 6 0))
(define (move-cow ws)
  (cond [(>= (worldstate-x ws) WIDTH)
         (make-worldstate 0 (worldstate-y ws) (+ 1 (worldstate-laps ws)) (worldstate-speed ws) (worldstate-tilt ws))]
        [(and (> (worldstate-speed ws) 0) (>= (worldstate-tilt ws) 0))
         (advance-cow (rotate-cow ws (* -1 TILT-SPEED)))]
        [(and (> (worldstate-speed ws) 0) (< (worldstate-tilt ws) 0))
         (advance-cow (rotate-cow ws TILT-SPEED))]
        [else ws]))




(check-expect (draw-cow (make-worldstate 100 100 10 10 5))
                        (place-image (above/align
                                      "left"
                                      (counter "Speed: " 10 " pixels/tick")
                                      (counter "Distance: " 10 " laps"))
                                     DISPLAY-X-POS DISPLAY-Y-POS
                                     (place-image
                                      (rotate 5 COW)
                                      100 100
                                      MTS)))
              
(define (draw-cow ws)
  (place-image
   (above/align
    "left"
    (counter "Speed: " (worldstate-speed ws) " pixels/tick")
    (counter "Distance: " (worldstate-laps ws) " laps"))
   DISPLAY-X-POS DISPLAY-Y-POS
   (place-image
    (rotate (worldstate-tilt ws) COW)
    (worldstate-x ws)
    (worldstate-y ws)
    MTS)))







(check-expect (accel-cow (make-worldstate 0 0 0 0 0)
                         "w")
              (make-worldstate 0 0 0 1 0))
(check-expect (accel-cow (make-worldstate 0 0 0 0 0)
                         "up")
              (make-worldstate 0 0 0 1 0))
(check-expect (accel-cow (make-worldstate 0 0 0 23 0)
                         "w")
              (make-worldstate 0 0 0 24 0))
(check-expect (accel-cow (make-worldstate 0 0 0 24 0)
                         "up")
              (make-worldstate 0 0 0 25 0))
(check-expect (accel-cow (make-worldstate 0 0 0 10 0)
                         "s")
              (make-worldstate 0 0 0 9 0))
(check-expect (accel-cow (make-worldstate 0 0 0 10 0)
                         "down")
              (make-worldstate 0 0 0 9 0))
(check-expect (accel-cow (make-worldstate 0 0 0 0 0)
                         "s")
              (make-worldstate 0 0 0 0 0))
(check-expect (accel-cow (make-worldstate 0 0 0 0 0)
                         "down")
              (make-worldstate 0 0 0 0 0))
(check-expect (accel-cow (make-worldstate 0 0 0 400 0)
                         "r")
              (make-worldstate 0 0 0 1 0))
(check-expect (accel-cow (make-worldstate 0 0 0 400 0)
                         "n")
              (make-worldstate 0 0 0 400 0))
(define (accel-cow ws a-key)
  (cond
    [(or (key=? a-key "w") (key=? a-key "up"))
     (make-worldstate (worldstate-x ws) (worldstate-y ws) (worldstate-laps ws) (+ (worldstate-speed ws) 1) (worldstate-tilt ws))]
    [(and (> (worldstate-speed ws) 0) (or (key=? a-key "s") (key=? a-key "down")))
     (make-worldstate (worldstate-x ws) (worldstate-y ws) (worldstate-laps ws) (- (worldstate-speed ws) 1) (worldstate-tilt ws))]
    [(key=? a-key "r") (make-worldstate (worldstate-x ws) (worldstate-y ws) (worldstate-laps ws) 1 (worldstate-tilt ws))]
    [else ws]))





(check-expect (tele-cow
               (make-worldstate 1 1 1 1 1)
               100 100 "enter")
              (make-worldstate 1 1 1 1 1))
(check-expect (tele-cow
               (make-worldstate 1 1 1 1 1)
               156 277 "drag")
              (make-worldstate 156 277 1 1 1))
(check-expect (tele-cow
               (make-worldstate 1 1 1 1 1)
               23 333 "button-down")
              (make-worldstate 23 333 1 1 1))
(define (tele-cow ws x y event)
  (if (or (mouse=? event "button-down")
          (mouse=? event "drag"))
      (make-worldstate
       x y
       (worldstate-laps ws)
       (worldstate-speed ws)
       (worldstate-tilt ws))
      ws))




(check-expect (rotate-cow (make-worldstate 0 0 0 0 0) 0)
              (make-worldstate 0 0 0 0 0))
(check-expect (rotate-cow (make-worldstate 342 217 67 44 50) -50)
              (make-worldstate 342 217 67 44 0))
(check-expect (rotate-cow (make-worldstate 342 217 67 44 -50) 50)
              (make-worldstate 342 217 67 44 0))
(define (rotate-cow ws tilt)
  (make-worldstate
   (worldstate-x ws)
   (worldstate-y ws)
   (worldstate-laps ws)
   (worldstate-speed ws)
   (+ tilt (worldstate-tilt ws))))




(check-expect (advance-cow (make-worldstate 0 0 10 10 0))
              (make-worldstate 10 0 10 10 0))
(check-expect (advance-cow (make-worldstate 342 217 67 44 50))
              (make-worldstate 386 217 67 44 50))
(define (advance-cow ws)
  (make-worldstate
   (+ (worldstate-x ws) (worldstate-speed ws))
   (worldstate-y ws)
   (worldstate-laps ws)
   (worldstate-speed ws)
   (worldstate-tilt ws)))



(check-expect (counter "" 0 "") (text "0" 24 "red"))
(check-expect (counter "Time: " 10 " seconds") (text "Time: 10 seconds" 24 "red"))
(define (counter words num units)
  (text (string-append words (number->string num) units) 24 "red"))
 