

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)

(define COW .)

(define COW-FONTSIZE 20)
(define COW-FONTCOLOR "black")
(define COW-TEXTX (/ HEIGHT 10))
(define COW-LAPSY (/ WIDTH 20))
(define COW-SPEEDY (/ WIDTH 10))

(define MTS (empty-scene WIDTH HEIGHT))

(define COW-INIT_SPEED 1)
(define COW-INIT_LAPS 0)

(define COW-ROT_HIGH -2)
(define COW-ROT_REST 0)
(define COW-ROT_LOW 2)

(define-struct cow-ws (x y speed laps rot))







 

(define COW-START (make-cow-ws 0 (/ WIDTH 2) COW-INIT_SPEED COW-INIT_LAPS 0))

(define (cow-main ws)
  (big-bang ws
    (on-tick cow-do-turn)
    (on-draw cow-draw-scene)
    (on-key cow-input)
    (on-mouse cow-click)))






(define COW-DO-TURN1 (make-cow-ws (/ WIDTH 2) (/ HEIGHT 2) 10 0 0))
(define COW-DO-TURN2 (make-cow-ws (- WIDTH 5) (/ HEIGHT 2) 8 0 0))
(define COW-DO-TURN3 (make-cow-ws (/ WIDTH 2) (/ HEIGHT 2) 0 0 0))
(define COW-DO-TURN4 (make-cow-ws (- WIDTH 5) (/ HEIGHT 2) 5 0 0))
(check-expect (cow-do-turn COW-DO-TURN1)
              (make-cow-ws (+ (cow-ws-x COW-DO-TURN1) (cow-ws-speed COW-DO-TURN1))
                           (cow-ws-y COW-DO-TURN1)
                           (cow-ws-speed COW-DO-TURN1)
                           (cow-ws-laps COW-DO-TURN1)
                           (rotate-cow (cow-ws-rot COW-DO-TURN1)
                                       (not (= (cow-ws-speed COW-DO-TURN1) 0)))))

(check-expect (cow-do-turn COW-DO-TURN2)
              (make-cow-ws 0
                           (cow-ws-y COW-DO-TURN2)
                           (cow-ws-speed COW-DO-TURN2)
                           (+ 1 (cow-ws-laps COW-DO-TURN2))
                           (rotate-cow (cow-ws-rot COW-DO-TURN2)
                                       (not (= (cow-ws-speed COW-DO-TURN2) 0)))))

(check-expect (cow-do-turn COW-DO-TURN3) COW-DO-TURN3)

(check-expect (cow-do-turn COW-DO-TURN4)
              (make-cow-ws 0
                           (cow-ws-y COW-DO-TURN4)
                           (cow-ws-speed COW-DO-TURN4)
                           (+ 1 (cow-ws-laps COW-DO-TURN4))
                           (rotate-cow (cow-ws-rot COW-DO-TURN4)
                                       (not (= (cow-ws-speed COW-DO-TURN4) 0)))))

(define (cow-do-turn cow-ws)
  (make-cow-ws (if (<= WIDTH (+ (cow-ws-x cow-ws) (cow-ws-speed cow-ws)))
                   0 (+ (cow-ws-x cow-ws) (cow-ws-speed cow-ws)))
               (cow-ws-y cow-ws)
               (cow-ws-speed cow-ws)
               (if (<= WIDTH (+ (cow-ws-x cow-ws) (cow-ws-speed cow-ws)))
                   (+ 1 (cow-ws-laps cow-ws)) (cow-ws-laps cow-ws))
               (rotate-cow (cow-ws-rot cow-ws)
                           (not (= (cow-ws-speed cow-ws) 0)))))




(check-expect (rotate-cow COW-ROT_LOW false) COW-ROT_REST)
(check-expect (rotate-cow COW-ROT_HIGH true) COW-ROT_LOW)
(check-expect (rotate-cow COW-ROT_LOW true) COW-ROT_HIGH)
(check-expect (rotate-cow COW-ROT_REST true) COW-ROT_LOW)

(define (rotate-cow rot moving)
  (cond [moving
         (if (> rot COW-ROT_REST)
             COW-ROT_HIGH
             COW-ROT_LOW)]
        [else COW-ROT_REST]))




(define COW-STUCK (make-cow-ws (/ WIDTH 2) (/ HEIGHT 2) 0 2 0))
(define COW-MOVING (make-cow-ws (/ WIDTH 2) (/ HEIGHT 2) 4 3 -8))


(check-expect (cow-draw-scene COW-START)
              (cow-add-labels COW-START
                              (place-image (rotate (cow-ws-rot COW-START) COW)
                                           (cow-ws-x COW-START)
                                           (cow-ws-y COW-START)
                                           MTS)))


(check-expect (cow-draw-scene COW-STUCK)
              (cow-add-labels COW-STUCK
                              (place-image (rotate (cow-ws-rot COW-STUCK) COW)
                                           (cow-ws-x COW-STUCK)
                                           (cow-ws-y COW-STUCK)
                                           MTS)))

(check-expect (cow-draw-scene COW-MOVING)
              (cow-add-labels COW-MOVING
                              (place-image (rotate (cow-ws-rot COW-MOVING) COW)
                                           (cow-ws-x COW-MOVING)
                                           (cow-ws-y COW-MOVING)
                                           MTS)))

(define (cow-draw-scene cow-ws)
  (cow-add-labels cow-ws (place-image
                          (rotate (cow-ws-rot cow-ws) COW)
                          (cow-ws-x cow-ws)
                          (cow-ws-y cow-ws)
                          MTS)))





(define LABELCOW1 (make-cow-ws 0 0 2 3 0))
(define LABELCOW2 (make-cow-ws 0 0 2 3 0))

(check-expect (cow-add-labels LABELCOW1 MTS) 
              (place-image
               (text "Laps: 3" COW-FONTSIZE COW-FONTCOLOR) COW-TEXTX COW-LAPSY
               (place-image
                (text "Speed: 2" COW-FONTSIZE COW-FONTCOLOR)
                COW-TEXTX COW-SPEEDY MTS)))
(check-expect (cow-add-labels LABELCOW2 MTS)
              (place-image
               (text (string-append "Laps: "
                                    (number->string (cow-ws-laps LABELCOW2)))
                     COW-FONTSIZE COW-FONTCOLOR)
               COW-TEXTX COW-LAPSY
               
               (place-image
                (text (string-append "Speed: "
                                     (number->string (cow-ws-speed LABELCOW2)))
                      COW-FONTSIZE COW-FONTCOLOR)
                COW-TEXTX COW-SPEEDY MTS)))

(define (cow-add-labels cow-ws scene)
  (place-image (text (string-append
                      "Laps: "
                      (number->string (cow-ws-laps cow-ws)))
                     COW-FONTSIZE
                     COW-FONTCOLOR)
               COW-TEXTX
               COW-LAPSY
               (place-image
                (text (string-append
                       "Speed: "
                       (number->string (cow-ws-speed cow-ws)))
                      COW-FONTSIZE
                      COW-FONTCOLOR)
                COW-TEXTX
                COW-SPEEDY
                scene)))




(define KEYEX1 (make-cow-ws 0 0 0 2 0))
(define KEYEX2 (make-cow-ws 0 0 1 2 0))

(check-expect (cow-input COW-START "t") COW-START)
(check-expect (cow-input KEYEX1 "a") KEYEX1)
(check-expect (cow-input KEYEX2 "a") (cow-mod-speed KEYEX2 -1))
(check-expect (cow-input KEYEX2 "s") (cow-mod-speed KEYEX2 1))

(define (cow-input cow-ws key)
  (cond [(key=? key "a") (cow-mod-speed cow-ws -1)]
        [(key=? key "s") (cow-mod-speed cow-ws 1)]
        [else cow-ws]))




(check-expect (cow-mod-speed COW-START 1)
              (make-cow-ws (cow-ws-x COW-START)
                           (cow-ws-y COW-START)
                           (+ 1 (cow-ws-speed COW-START))
                           (cow-ws-laps COW-START)
                           (cow-ws-rot COW-START)))

(check-expect (cow-mod-speed COW-START -1)
              (make-cow-ws (cow-ws-x COW-START)
                           (cow-ws-y COW-START)
                           (- 1 (cow-ws-speed COW-START))
                           (cow-ws-laps COW-START)
                           (cow-ws-rot COW-START)))

(check-expect (cow-mod-speed KEYEX1 -1)
              (make-cow-ws (cow-ws-x KEYEX1)
                           (cow-ws-y KEYEX1)
                           0
                           (cow-ws-laps KEYEX1)
                           (cow-ws-rot KEYEX1)))

(define (cow-mod-speed cow-ws speed)
  (make-cow-ws (cow-ws-x cow-ws)
               (cow-ws-y cow-ws)
               (max (+ (cow-ws-speed cow-ws) speed) 0)
               (cow-ws-laps cow-ws)
               (cow-ws-rot cow-ws)))






(check-expect (cow-click COW-START 240 10 "button-down")
              (make-cow-ws 240 10 (cow-ws-speed COW-START)
                           (cow-ws-laps COW-START) (cow-ws-rot COW-START)))

(check-expect (cow-click COW-START 240 10 "drag") COW-START)

 

(define (cow-click cow-ws x y mouse-event)
  (cond
    [(string=? mouse-event "button-down")
     (make-cow-ws x y
                  (cow-ws-speed cow-ws)
                  (cow-ws-laps cow-ws)
                  (cow-ws-rot cow-ws))]
    [else cow-ws]))









(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define LABEL-X (* 2/3 WIDTH))

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







(define AUTO-IMAGE1 .)
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)










(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)

(define (seconds->ticks seconds)
  (floor (* seconds TICKS-SECOND)))

(check-expect (seconds->ticks 0) 0)
(check-expect (seconds->ticks 1) TICKS-SECOND)
(check-expect (seconds->ticks 2.4) (floor (* 2.4 TICKS-SECOND)))





(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else      AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)

(define GREEN_MULT 1)
(define YELLOW_MULT 3/4)
(define RED_MULT 1/2)

(define MAX-SPEED 5)
(define STOP-POINT-MAX (/ WIDTH 2))
(define STOP-POINT-MIN (- (/ WIDTH 2) MAX-SPEED))




(define-struct traffic (light ticks-till-change all-ticks))








(define TRAF-FONT-SIZE 20)
(define TRAF-FONT-COLOR "black")

(define TRAFFIC-START (make-traffic "green" (seconds->ticks GREEN-LENGTH) 0))

(define (traffic-main traffic)
  (big-bang traffic
    (on-tick traffic-turn)
    (on-draw traffic-draw)))







(check-expect (traffic-turn TRAFFIC-START)
              (make-traffic (traffic-light TRAFFIC-START)
                            (sub1 (traffic-ticks-till-change TRAFFIC-START))
                            (add1 (traffic-all-ticks TRAFFIC-START))))

(define TRAF-TURN-EX1 (make-traffic "green" 0 123))

(check-expect (traffic-turn TRAF-TURN-EX1)
              (make-traffic
               "yellow"
               (seconds->ticks YELLOW-LENGTH)
               (add1 (traffic-all-ticks TRAF-TURN-EX1))))

(check-expect (traffic-turn (make-traffic "green" 0 0))
              (make-traffic "yellow" (seconds->ticks YELLOW-LENGTH) (add1 0)))
(check-expect (traffic-turn (make-traffic "yellow" 0 0))
              (make-traffic "red" (seconds->ticks RED-LENGTH) (add1 0)))
(check-expect (traffic-turn (make-traffic "red" 0 0))
              (make-traffic "green" (seconds->ticks GREEN-LENGTH) (add1 0)))
(check-expect (traffic-turn (make-traffic "green" 1 0))
              (make-traffic "green" (sub1 1) (add1 0)))
(check-expect (traffic-turn (make-traffic "red" 17 0))
              (make-traffic "red" (sub1 17) (add1 0)))

(define (traffic-turn traffic)
  (cond [(> (traffic-ticks-till-change traffic) 0)
         (make-traffic
          (traffic-light traffic)
          (sub1 (traffic-ticks-till-change traffic))
          (add1 (traffic-all-ticks traffic)))]
        [(string=? (traffic-light traffic) "green")
         (make-traffic
          "yellow"
          (seconds->ticks YELLOW-LENGTH)
          (add1 (traffic-all-ticks traffic)))]
        [(string=? (traffic-light traffic) "yellow")
         (make-traffic
          "red"
          (seconds->ticks RED-LENGTH)
          (add1 (traffic-all-ticks traffic)))]
        [(string=? (traffic-light traffic) "red")
         (make-traffic
          "green"
          (seconds->ticks GREEN-LENGTH)
          (add1 (traffic-all-ticks traffic)))]))




(check-expect (traffic-draw TRAFFIC-START)
              (place-image (seconds-label TRAFFIC-START) LABEL-X Y-POS 
                           (place-image
                            (make-bulbs TRAFFIC-START) X-POS Y-POS MTS)))

(define TRAF-DRAW-EX1 (make-traffic "red" 9 172))
(check-expect (traffic-draw TRAF-DRAW-EX1)
              (place-image (seconds-label TRAF-DRAW-EX1) LABEL-X Y-POS 
                           (place-image
                            (make-bulbs TRAF-DRAW-EX1) X-POS Y-POS MTS)))

(define (traffic-draw traffic)
  (place-image (seconds-label traffic) LABEL-X Y-POS
               (place-image (make-bulbs traffic) X-POS Y-POS MTS)))




(check-expect (seconds-label TRAFFIC-START)
              (text (string-append
                     "Seconds: "
                     (number->string
                      (ticks->seconds (traffic-all-ticks TRAFFIC-START))))
                    TRAF-FONT-SIZE TRAF-FONT-COLOR))

(check-expect (seconds-label TRAF-DRAW-EX1)
              (text (string-append
                     "Seconds: "
                     (number->string
                      (ticks->seconds (traffic-all-ticks TRAF-DRAW-EX1))))
                    TRAF-FONT-SIZE TRAF-FONT-COLOR))

(define (seconds-label traffic)
  (text (string-append
         "Seconds: "
         (number->string (ticks->seconds (traffic-all-ticks traffic))))
        TRAF-FONT-SIZE TRAF-FONT-COLOR))




(check-expect (make-bulbs TRAFFIC-START)
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "solid" "green")))

(check-expect (make-bulbs TRAF-DRAW-EX1)
              (above
               (circle LIGHT-RADIUS "solid" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "outline" "green")))

(define (make-bulbs traffic)
  (above
   (circle LIGHT-RADIUS (if (string=? (traffic-light traffic) "red")
                            "solid" "outline") "red")
   (circle LIGHT-RADIUS (if (string=? (traffic-light traffic) "yellow")
                            "solid" "outline") "yellow")
   (circle LIGHT-RADIUS (if (string=? (traffic-light traffic) "green")
                            "solid" "outline") "green")))



(define-struct ws-auto (traffic autos))





(define-struct auto (x y image speed))










(define (random-auto x)
  (make-auto x
             (+ (random (* HEIGHT 1/3)) (/ HEIGHT 2))
             (pick-image (random 3))
             (add1 (random (sub1 MAX-SPEED)))))



(check-expect (n-random-autos 0 4) empty)

(define (n-random-autos n range)
  (if (= 0 n)
      empty
      (cons (random-auto (random range))
            (n-random-autos (sub1 n) range))))

(define START (make-ws-auto TRAFFIC-START (n-random-autos 12 40)))

(define (main ws-auto)
  (big-bang ws-auto
    (on-tick ws-auto-turn)
    (on-draw ws-auto-draw)))





(define RAND-AUTO-START (random-auto 0))
(define RAND-AUTO-END (random-auto WIDTH))

(check-expect (ws-auto-turn (make-ws-auto (make-traffic "green" 0 0) empty))
              (make-ws-auto (traffic-turn (make-traffic "green" 0 0)) empty))
(check-expect (ws-auto-turn (make-ws-auto (make-traffic "green" 0 0)
                                          (list RAND-AUTO-START)))
              (make-ws-auto (traffic-turn (make-traffic "green" 0 0))
                            (auto-turn (list RAND-AUTO-START) GREEN_MULT)))

(define (ws-auto-turn ws-auto)
  (make-ws-auto (traffic-turn (ws-auto-traffic ws-auto))
                (auto-turn (ws-auto-autos ws-auto) (light-mult ws-auto))))



(check-expect (light-mult
               (make-ws-auto (make-traffic "green" 0 0) empty)) GREEN_MULT)
(check-expect (light-mult
               (make-ws-auto (make-traffic "yellow" 0 0) empty)) YELLOW_MULT)
(check-expect (light-mult
               (make-ws-auto (make-traffic "red" 0 0) empty)) RED_MULT)

(define (light-mult ws-auto)
  (local [(define light-color (traffic-light (ws-auto-traffic ws-auto)))]
    (cond [(string=? light-color "green") GREEN_MULT]
          [(string=? light-color "yellow") YELLOW_MULT]
          [else RED_MULT])))







(define AT-RAND-1 (random-auto 0))
(define AT-RAND-2 (random-auto 7))
(define AT-RAND-3 (random-auto 19))
(define AT-RAND-4 (random-auto STOP-POINT-MIN))
(define AT-RAND-5 (random-auto (* 2 (/ WIDTH 3))))




(check-expect (auto-turn (list AT-RAND-1 AT-RAND-2 AT-RAND-3) GREEN_MULT)
              (list (move-auto AT-RAND-1 GREEN_MULT)
                    (move-auto AT-RAND-2 GREEN_MULT)
                    (move-auto AT-RAND-3 GREEN_MULT)))
(check-expect (auto-turn (list AT-RAND-1 AT-RAND-4 AT-RAND-5) RED_MULT)
              (list (move-auto AT-RAND-1 RED_MULT)
                    (move-auto AT-RAND-4 RED_MULT)
                    (move-auto AT-RAND-5 RED_MULT)))

(define (auto-turn loa mult)
  (cond [(empty? loa) empty]
        [(> (auto-x (move-auto (first loa) mult)) WIDTH)
         (cons (random-auto 0) (auto-turn (rest loa) mult))]
        [else (cons (move-auto (first loa) mult) (auto-turn (rest loa) mult))]))




(check-expect (move-auto AT-RAND-1 GREEN_MULT)
              (make-auto (+ (* (auto-speed AT-RAND-1) GREEN_MULT)
                            (auto-x AT-RAND-1))
                         (auto-y AT-RAND-1)
                         (auto-image AT-RAND-1)
                         (auto-speed AT-RAND-1)))

(check-expect (move-auto AT-RAND-2 YELLOW_MULT)
              (make-auto (+ (* (auto-speed AT-RAND-2) YELLOW_MULT)
                            (auto-x AT-RAND-2))
                         (auto-y AT-RAND-2)
                         (auto-image AT-RAND-2)
                         (auto-speed AT-RAND-2)))

(check-expect (move-auto AT-RAND-5 YELLOW_MULT)
              (make-auto (+ (* (auto-speed AT-RAND-5) GREEN_MULT)
                            (auto-x AT-RAND-5))
                         (auto-y AT-RAND-5)
                         (auto-image AT-RAND-5)
                         (auto-speed AT-RAND-5)))

(check-expect (move-auto AT-RAND-5 RED_MULT)
              (make-auto (+ (* (auto-speed AT-RAND-5) GREEN_MULT)
                            (auto-x AT-RAND-5))
                         (auto-y AT-RAND-5)
                         (auto-image AT-RAND-5)
                         (auto-speed AT-RAND-5)))

(check-expect (move-auto AT-RAND-4 RED_MULT)
              (make-auto (+ (* (auto-speed AT-RAND-4) 0) (auto-x AT-RAND-4))
                         (auto-y AT-RAND-4)
                         (auto-image AT-RAND-4)
                         (auto-speed AT-RAND-4)))

(check-expect (move-auto AT-RAND-3 RED_MULT)
              (make-auto (+ (* (auto-speed AT-RAND-3) RED_MULT)
                            (auto-x AT-RAND-3))
                         (auto-y AT-RAND-3)
                         (auto-image AT-RAND-3)
                         (auto-speed AT-RAND-3)))

(define (move-auto auto mult)
  (make-auto (+ (* (auto-speed auto)
                   (get-mult (auto-x auto) mult))
                (auto-x auto))
             (auto-y auto)
             (auto-image auto)
             (auto-speed auto)))



(check-expect (get-mult 20 GREEN_MULT) GREEN_MULT)
(check-expect (get-mult STOP-POINT-MAX GREEN_MULT) GREEN_MULT)
(check-expect (get-mult (* 2/3 WIDTH) GREEN_MULT) GREEN_MULT)
(check-expect (get-mult 20 YELLOW_MULT) YELLOW_MULT)
(check-expect (get-mult STOP-POINT-MAX YELLOW_MULT) YELLOW_MULT)
(check-expect (get-mult (* 2/3 WIDTH) YELLOW_MULT) GREEN_MULT)
(check-expect (get-mult 20 RED_MULT) RED_MULT)
(check-expect (get-mult STOP-POINT-MAX RED_MULT) 0)
(check-expect (get-mult (* 2/3 WIDTH) RED_MULT) GREEN_MULT)

(define (get-mult x mult)
  (cond [(> x STOP-POINT-MAX) GREEN_MULT]
        [(< x STOP-POINT-MIN) mult]
        [else (if (= mult RED_MULT) 0 mult)]))



(define DA-RAND-1 (random-auto 0))
(define DA-RAND-2 (random-auto 20))
(define DA-RAND-3 (random-auto 175))
(define DA-RAND-4 (random-auto (* WIDTH 1/2)))
(define DA-RAND-5 (random-auto WIDTH))

(define DA-WS-1 (make-ws-auto (ws-auto-traffic START)
                              (list DA-RAND-1 DA-RAND-2 DA-RAND-3)))
(define DA-WS-2 (make-ws-auto (make-traffic "red" 15 180)
                              (list DA-RAND-3 DA-RAND-4 DA-RAND-5)))

(check-expect (ws-auto-draw DA-WS-1)
              (draw-autos (ws-auto-autos DA-WS-1)
                          (traffic-draw (ws-auto-traffic DA-WS-1))))
(check-expect (ws-auto-draw DA-WS-2)
              (draw-autos (ws-auto-autos DA-WS-2)
                          (traffic-draw (ws-auto-traffic DA-WS-2))))

(define (ws-auto-draw ws-auto)
  (draw-autos
   (ws-auto-autos ws-auto)
   (traffic-draw (ws-auto-traffic ws-auto))))



(check-expect (draw-autos empty MTS) MTS)
(check-expect (draw-autos (list DA-RAND-1 DA-RAND-2 DA-RAND-3) MTS)
              (place-auto DA-RAND-1
                          (place-auto DA-RAND-2 (place-auto DA-RAND-3 MTS))))
(check-expect (draw-autos (list DA-RAND-3 DA-RAND-4 DA-RAND-5) MTS)
              (place-auto DA-RAND-3
                          (place-auto DA-RAND-4 (place-auto DA-RAND-5 MTS))))

(define (draw-autos loa img)
  (cond [(empty? loa) img]
        [else (place-auto (first loa) (draw-autos (rest loa) img))]))



(check-expect (place-auto DA-RAND-1 MTS)
              (place-image (auto-image DA-RAND-1)
                           (auto-x DA-RAND-1) (auto-y DA-RAND-1) MTS))
(check-expect (place-auto DA-RAND-2 MTS)
              (place-image (auto-image DA-RAND-2)
                           (auto-x DA-RAND-2) (auto-y DA-RAND-2) MTS))

(define (place-auto auto img)
  (place-image (auto-image auto)
               (auto-x auto)
               (auto-y auto)
               img))