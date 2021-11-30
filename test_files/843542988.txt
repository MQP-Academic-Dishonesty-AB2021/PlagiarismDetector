

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Kai Nakamura Cara Salter Assignment 2 Traffic Signal|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define TRAFFIC-SIGNAL-POS-X CTR-X) 
(define TRAFFIC-SIGNAL-POS-Y (/ HEIGHT 4)) 
(define BILLBOARD-POS-X (/ WIDTH 4)) 
(define BILLBOARD-POS-Y (/ HEIGHT 4)) 
(define TICKS-SECOND 28) 
(define MTS (empty-scene WIDTH HEIGHT))






(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 
(define TOTAL-LENGTH (+ GREEN-LENGTH
                        YELLOW-LENGTH
                        RED-LENGTH))
(define TRAFFIC-LIGHT-FONT-SIZE 16)
(define BILLBOARD-FONT-SIZE 24)
(define FONT-COLOR "black")

(define BULB-GREEN
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))
(define BULB-YELLOW
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))
(define BULB-RED
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)

(define AUTO-SPAWN-CHANCE 5) 










(define-struct auto (x y speed image))

(define AUTO-0 (make-auto 0 0 0 AUTO-IMAGE1))
(define AUTO-1 (make-auto 0 CTR-Y 1 AUTO-IMAGE1))
(define AUTO-2 (make-auto CTR-X CTR-Y 2 AUTO-IMAGE2))
(define AUTO-3 (make-auto WIDTH CTR-Y 3 AUTO-IMAGE3))
(define AUTO-4 (make-auto (+ 1 WIDTH) CTR-Y 1 AUTO-IMAGE1))

 








(define AUTOS-1 (list AUTO-1))
(define AUTOS-2 (list AUTO-1 AUTO-2))
(define AUTOS-3 (list AUTO-1 AUTO-2 AUTO-3))
(define AUTOS-4 (list AUTO-1 AUTO-2 AUTO-3 AUTO-4))

 











(define BILLBOARD-1 "abc")
(define BILLBOARD-2 "de")
(define BILLBOARD-3 "f")
(define BILLBOARDS-1 (list BILLBOARD-1))
(define BILLBOARDS-2 (list BILLBOARD-2 BILLBOARD-1))
(define BILLBOARDS-3 (list BILLBOARD-3 BILLBOARD-2 BILLBOARD-1))

 











(define-struct world (ticks autos billboards))

(define START (make-world 0 empty empty))

 














(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)

(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))



(check-expect (seconds->ticks 1) 28)
(check-expect (seconds->ticks 0) 0)
(check-expect (seconds->ticks 10) 280)

(define (seconds->ticks seconds)
  (* seconds TICKS-SECOND))





(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)

(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))



(define GREEN-TICKS (seconds->ticks (sub1 GREEN-LENGTH))) 
(define YELLOW-TICKS (seconds->ticks (sub1 (+ YELLOW-LENGTH GREEN-LENGTH))))
(define RED-TICKS (seconds->ticks (add1 (- TOTAL-LENGTH RED-LENGTH))))


(define GREEN-WORLD (make-world GREEN-TICKS empty empty))
(define YELLOW-WORLD (make-world YELLOW-TICKS empty empty))
(define RED-WORLD (make-world RED-TICKS empty empty))



"Start world with (main START)" 
(define (main world)
  (big-bang world
    (on-tick update)       
    (to-draw render)       
    (on-key  handle-key))) 



(check-expect (world-ticks (update START)) 1)
(check-expect (world-ticks (update (make-world 1 empty empty))) 2)

(define (update world)
  (make-world (add1 (world-ticks world))
              (remove-off-screen-autos (update-autos
                                        (spawn-auto (world-autos world)
                                                    (<= (random 100)
                                                        AUTO-SPAWN-CHANCE))
                                        (ticks->color
                                         (world-ticks world))))
              (world-billboards world)))



(check-expect (update-autos empty "green") empty)
(check-expect (update-autos AUTOS-1 "green")
              (list (update-auto AUTO-1 "green")))
(check-expect (update-autos AUTOS-2 "yellow")
              (list (update-auto AUTO-1 "yellow")
                    (update-auto AUTO-2 "yellow")))
(check-expect (update-autos AUTOS-3 "red")
              (list (update-auto AUTO-1 "red")
                    (update-auto AUTO-2 "red")
                    (update-auto AUTO-3 "red")))

(define (update-autos autos color)
  (cond [(empty? autos) empty]
        [else
         (cons (update-auto (first autos) color)
               (update-autos (rest autos) color))]))



(check-expect (update-auto AUTO-0 "green")
              AUTO-0)
(check-expect (update-auto AUTO-1 "green")
              (move-auto AUTO-1))
(check-expect (update-auto AUTO-1 "yellow")
              (slow-auto AUTO-1))
(check-expect (update-auto AUTO-3 "yellow")
              (move-auto AUTO-3))
(check-expect (update-auto AUTO-1 "red")
              (slow-and-stop-auto AUTO-1))
(check-expect (update-auto AUTO-2 "red")
              (slow-and-stop-auto AUTO-2))
(check-expect (update-auto AUTO-3 "red")
              (move-auto AUTO-3))
                                            
(define (update-auto auto color)
  (cond [(string=? color "green")
         (move-auto auto)]
        [(string=? color "yellow")
         (if (auto-passed-traffic-signal? auto)
             (move-auto auto)
             (slow-auto auto))]
        [(string=? color "red")
         (if (auto-passed-traffic-signal? auto)
             (move-auto auto)
             (slow-and-stop-auto auto))]))



(check-expect (move-auto-pixels AUTO-0 0) AUTO-0)
(check-expect (move-auto-pixels AUTO-0 10) (make-auto 10 0 0 AUTO-IMAGE1))
(check-expect (move-auto-pixels AUTO-0 0.75) (make-auto 0.75 0 0 AUTO-IMAGE1))

(define (move-auto-pixels auto dx)
  (make-auto (+ (auto-x auto)
                dx)
             (auto-y auto)
             (auto-speed auto)
             (auto-image auto)))



(check-expect (move-auto AUTO-0) AUTO-0)
(check-expect (move-auto AUTO-1) (make-auto 1 CTR-Y 1 AUTO-IMAGE1))

(define (move-auto auto)
  (move-auto-pixels auto (auto-speed auto)))



(check-expect (slow-auto AUTO-0) AUTO-0)
(check-expect (slow-auto AUTO-1) (make-auto 0.75 CTR-Y 1 AUTO-IMAGE1))

(define (slow-auto auto)
  (move-auto-pixels auto (* 0.75 (auto-speed auto))))



(check-expect (slow-and-stop-auto AUTO-1)
              (move-auto-pixels AUTO-1 (/ (auto-speed AUTO-1) 2)))
(check-expect (slow-and-stop-auto AUTO-2) AUTO-2)

(define (slow-and-stop-auto auto)
  (local [(define moved-auto (move-auto-pixels auto (/ (auto-speed auto) 2)))]
    (if (auto-passed-traffic-signal? moved-auto)
        auto
        moved-auto)))



(check-expect (remove-off-screen-autos empty) empty)
(check-expect (remove-off-screen-autos AUTOS-1) AUTOS-1)
(check-expect (remove-off-screen-autos AUTOS-4) AUTOS-3)

(define (remove-off-screen-autos autos)
  (cond [(empty? autos) empty]
        [else
         (if (auto-off-screen? (first autos))
             (remove-off-screen-autos (rest autos))
             (cons (first autos)
                   (remove-off-screen-autos (rest autos))))]))



(check-expect (auto-passed-traffic-signal? AUTO-1) false)
(check-expect (auto-passed-traffic-signal? AUTO-3) true)

(define (auto-passed-traffic-signal? auto)
  (> (auto-x auto) CTR-X))



(check-expect (auto-off-screen? AUTO-1) false)
(check-expect (auto-off-screen? (make-auto (+ 1 WIDTH) 0 0 AUTO-IMAGE1)) true)

(define (auto-off-screen? auto)
  (> (auto-x auto) WIDTH))



(check-expect (spawn-auto empty false) empty)
(check-expect (spawn-auto AUTOS-1 false) AUTOS-1)
(check-expect (length (spawn-auto empty true)) 1)
(check-expect (length (spawn-auto AUTOS-1 true)) 2)
(check-expect (auto? (first (spawn-auto empty true))) true)

(define (spawn-auto autos should-spawn?)
  (if should-spawn?
      (cons (make-auto 0
                       (+ CTR-Y (random CTR-Y))
                       (+ 8 (random 5))
                       (pick-image (random 3)))
            autos)
      autos))



(check-expect (render START)
              (render-billboards
               (world-billboards START)
               (render-autos (world-autos START)
                             (render-traffic-signal
                              (world-ticks START) MTS))))
(check-expect (render YELLOW-WORLD)
              (render-billboards
               (world-billboards YELLOW-WORLD)
               (render-autos (world-autos YELLOW-WORLD)
                             (render-traffic-signal
                              (world-ticks YELLOW-WORLD) MTS))))
              
(define (render world)
  (render-billboards (world-billboards world)
                     (render-autos (world-autos world)
                                   (render-traffic-signal
                                    (world-ticks world) MTS))))



(check-expect (render-traffic-signal 0 MTS)
              (place-image (above (get-traffic-signal-bulbs 0)
                                  (get-counter-image 0))
                           TRAFFIC-SIGNAL-POS-X TRAFFIC-SIGNAL-POS-Y MTS))
(check-expect (render-traffic-signal YELLOW-TICKS MTS)
              (place-image (above (get-traffic-signal-bulbs
                                   YELLOW-TICKS)
                                  (get-counter-image
                                   YELLOW-TICKS))
                           TRAFFIC-SIGNAL-POS-X TRAFFIC-SIGNAL-POS-Y MTS))

(define (render-traffic-signal ticks scene)
  (place-image (above (get-traffic-signal-bulbs ticks)
                      (get-counter-image ticks))
               TRAFFIC-SIGNAL-POS-X TRAFFIC-SIGNAL-POS-Y scene))



(check-expect (get-traffic-signal-bulbs YELLOW-TICKS) BULB-YELLOW)
(check-expect (get-traffic-signal-bulbs GREEN-TICKS) BULB-GREEN)
(check-expect (get-traffic-signal-bulbs RED-TICKS) BULB-RED)

(define (get-traffic-signal-bulbs ticks)
  (local [(define color (ticks->color ticks))]
    (cond
      [(string=? "green" color) BULB-GREEN]
      [(string=? "yellow" color) BULB-YELLOW]
      [(string=? "red" color) BULB-RED])))



(check-expect (get-counter-image 0)
              (text "0" TRAFFIC-LIGHT-FONT-SIZE FONT-COLOR))
(check-expect (get-counter-image (seconds->ticks 100))
              (text "100" TRAFFIC-LIGHT-FONT-SIZE FONT-COLOR))

(define (get-counter-image ticks)
  (text (number->string (ticks->seconds ticks))
        TRAFFIC-LIGHT-FONT-SIZE
        FONT-COLOR))



(check-expect (ticks->color GREEN-TICKS) "green")
(check-expect (ticks->color YELLOW-TICKS) "yellow")
(check-expect (ticks->color RED-TICKS) "red")

(define (ticks->color ticks)
  (local [(define seconds (ticks->seconds ticks))
          (define n (modulo seconds TOTAL-LENGTH))]
    (cond
      [(< n GREEN-LENGTH) "green"]
      [(< n (+ YELLOW-LENGTH GREEN-LENGTH)) "yellow"]
      [else "red"])))



(check-expect (render-autos AUTOS-1 MTS)
              (render-auto AUTO-1 MTS))
(check-expect (render-autos AUTOS-2 MTS)
              (render-auto AUTO-2
                           (render-auto AUTO-1 MTS)))
(check-expect (render-autos AUTOS-3 MTS)
              (render-auto AUTO-3
                           (render-auto AUTO-2
                                        (render-auto AUTO-1 MTS))))

(define (render-autos autos scene)
  (cond [(empty? autos) scene]
        [else
         (render-autos (rest autos)
                       (render-auto (first autos) scene))]))



(check-expect (render-auto AUTO-1 MTS)
              (place-image AUTO-IMAGE1 0 CTR-Y MTS))
(check-expect (render-auto AUTO-2 MTS)
              (place-image AUTO-IMAGE2 CTR-X CTR-Y MTS))
(check-expect (render-auto AUTO-3 MTS)
              (place-image AUTO-IMAGE3 WIDTH CTR-Y MTS))

(define (render-auto auto scene)
  (place-image (auto-image auto)
               (auto-x auto)
               (auto-y auto)
               scene))



(check-expect (render-billboards empty MTS) MTS)
(check-expect (render-billboards BILLBOARDS-1 MTS)
              (place-image (text BILLBOARD-1 BILLBOARD-FONT-SIZE FONT-COLOR)
                           BILLBOARD-POS-X
                           BILLBOARD-POS-Y
                           MTS))
(check-expect (render-billboards BILLBOARDS-2 MTS)
              (place-image (text BILLBOARD-2 BILLBOARD-FONT-SIZE FONT-COLOR)
                           BILLBOARD-POS-X
                           BILLBOARD-POS-Y
                           MTS))
               
(define (render-billboards billboards scene)
  (cond [(empty? billboards) scene]
        [else
         (place-image (text (first billboards) BILLBOARD-FONT-SIZE FONT-COLOR)
                      BILLBOARD-POS-X
                      BILLBOARD-POS-Y
                      scene)]))



(check-expect (handle-key START "shift") START)
(check-expect (handle-key START "rshift") START)
(check-expect (handle-key (make-world 0 empty (list " ")) "\b")
              (make-world 0 empty (list "")))
(check-expect (handle-key (make-world 0 empty (list " ")) "\u007F")
              (make-world 0 empty (list "")))
(check-expect (handle-key START "\r")
              (make-world 0 empty (list "\n")))
(check-expect (handle-key (make-world 0 empty (list " ")) "\t")
              (make-world 0 empty (clear-billboard (list " "))))
(check-expect (handle-key START "next")
              (make-world 0 empty (add-billboard empty)))
(check-expect (handle-key START "prior")
              (make-world 0 empty (remove-billboard empty)))
(check-expect (handle-key START " ")
              (make-world 0 empty (list " ")))

(define (handle-key world key-event)
  (cond [(or (key=? key-event "shift")
             (key=? key-event "rshift"))
         world]
        [(or (key=? key-event "\b")
             (key=? key-event "\u007F"))
         (make-world (world-ticks world)
                     (world-autos world)
                     (remove-char (world-billboards world)))]
        [(key=? key-event "\r")
         (make-world (world-ticks world)
                     (world-autos world)
                     (add-char (world-billboards world) "\n"))]
        [(key=? key-event "\t")
         (make-world (world-ticks world)
                     (world-autos world)
                     (clear-billboard (world-billboards world)))]
        [(key=? key-event "next")
         (make-world (world-ticks world)
                     (world-autos world)
                     (add-billboard (world-billboards world)))]
        [(key=? key-event "prior")
         (make-world (world-ticks world)
                     (world-autos world)
                     (remove-billboard (world-billboards world)))]
        [else
         (make-world (world-ticks world)
                     (world-autos world)
                     (add-char (world-billboards world) key-event))]))



(check-expect (add-char empty "a") (list "a"))
(check-expect (add-char BILLBOARDS-1 "d")
              (list (string-append BILLBOARD-1 "d")))
(check-expect (add-char BILLBOARDS-2 "f")
              (list (string-append BILLBOARD-2 "f") BILLBOARD-1))

(define (add-char billboards char)
  (cond [(empty? billboards) (list char)]
        [else
         (cons (string-append (first billboards) char)
               (rest billboards))]))



(check-expect (remove-char empty) empty)
(check-expect (remove-char (list "")) (list ""))
(check-expect (remove-char BILLBOARDS-1)
              (list (substring BILLBOARD-1 0
                               (- (string-length BILLBOARD-1) 1))))
(check-expect (remove-char BILLBOARDS-2)
              (list (substring BILLBOARD-2 0
                               (- (string-length BILLBOARD-2) 1))
                    BILLBOARD-1))

(define (remove-char billboards)
  (cond [(empty? billboards) empty]
        [else
         (if (> (string-length (first billboards)) 0)
             (cons (substring (first billboards) 0
                              (- (string-length (first billboards)) 1))
                   (rest billboards))
             billboards)]))



(check-expect (clear-billboard empty) empty)
(check-expect (clear-billboard BILLBOARDS-1) (list ""))
(check-expect (clear-billboard BILLBOARDS-2) (list "" BILLBOARD-1))

(define (clear-billboard billboards)
  (cond [(empty? billboards) empty]
        [else
         (cons "" (rest billboards))]))



(check-expect (add-billboard empty) (list ""))
(check-expect (add-billboard BILLBOARDS-1) (list "" BILLBOARD-1))

(define (add-billboard billboards)
  (cons "" billboards))



(check-expect (remove-billboard empty) empty)
(check-expect (remove-billboard BILLBOARDS-1) empty)
(check-expect (remove-billboard BILLBOARDS-2) BILLBOARDS-1)
(check-expect (remove-billboard BILLBOARDS-3) BILLBOARDS-2)

(define (remove-billboard billboards)
  (cond [(empty? billboards) empty]
        [else (rest billboards)]))