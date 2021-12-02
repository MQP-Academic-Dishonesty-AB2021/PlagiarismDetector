

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment 2 Traffic|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))

(define TICKS-SECOND 28) 

(define FONTCOLOR "BLACK")
(define FONTSIZE 24)





(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 


(define TOTAL-LENGTH (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))


(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

(define GREENLIGHT
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

(define YELLOWLIGHT
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))

(define REDLIGHT
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))
  
  







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)

(define STARTAUTO-X 0) 









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




(define-struct traffic-light (timer color))


(define TL1 (make-traffic-light 0 "GREEN"))

(define TL2 (make-traffic-light (* 6 28) "YELLOW"))

(define TL3 (make-traffic-light (* 9 28) "RED"))

(define (fn-for-traffic-light tl)
  (... (traffic-light-timer tl)
       (traffic-light-color tl)))








(define-struct auto (x y dx image))

 
  



(define (random-range min max)
  (+ (random (- max min)) min)) 




(define A1 
  (make-auto STARTAUTO-X (/ HEIGHT 2) 40 AUTO-IMAGE1))
(define A2 
  (make-auto STARTAUTO-X (* HEIGHT .75) 20 AUTO-IMAGE1))
(define A3 
  (make-auto (+ WIDTH 1) (* HEIGHT .8) 10 AUTO-IMAGE1))
(define A4 
  (make-auto (- WIDTH 10) (* HEIGHT .75) 30 AUTO-IMAGE1))
(define A5 
  (make-auto (* WIDTH .46) (* HEIGHT .75) 30 AUTO-IMAGE1))






 
(define LOA1 empty)
(define LOA2 (list A1 A2 A3))









(define-struct ws (tl loa))

(define START (make-ws TL1 LOA1))
(define TEST (make-ws TL2 LOA2))
          

 







(define (main ws)
  (big-bang ws
    (on-tick update-traffic)     
    (to-draw render)))           

(check-random (update-traffic START)
              (make-ws (next-light(ws-tl START)) (loa-handler START)))
(check-random (update-traffic TEST)
              (make-ws (next-light(ws-tl TEST)) (loa-handler TEST)))



(define (update-traffic ws)
  (make-ws (next-light(ws-tl ws)) (loa-handler ws)))



(check-expect (next-light (make-traffic-light 0 "GREEN"))
              (make-traffic-light 1 "GREEN"))
(check-expect (next-light (make-traffic-light 140 "GREEN"))
              (make-traffic-light 141 "YELLOW"))
(check-expect (next-light (make-traffic-light 308 "RED"))
              (make-traffic-light 309 "GREEN"))
(check-expect (next-light (make-traffic-light 307 "RED"))
              (make-traffic-light 308 "RED"))

(define (next-light tl)
  (cond [(< (remainder (ticks->seconds (traffic-light-timer tl)) TOTAL-LENGTH)
            GREEN-LENGTH)
         (make-traffic-light (add1 (traffic-light-timer tl)) "GREEN")]
        [(<(remainder (ticks->seconds (traffic-light-timer tl)) TOTAL-LENGTH)
           (+ GREEN-LENGTH YELLOW-LENGTH))
         (make-traffic-light (add1 (traffic-light-timer tl)) "YELLOW")]
        [else
         (make-traffic-light (add1 (traffic-light-timer tl)) "RED")]))


(check-random (loa-handler START)
      (if (< (random 100) 5) 
          (cons (make-auto STARTAUTO-X  (random-range (/ HEIGHT 2) HEIGHT)
                       (random-range 10 50) (pick-image (random 3)))
                (move-autos (ws-loa START) START))
          (move-autos (ws-loa START) START)))
(check-random (loa-handler TEST)
      (if (< (random 100) 5) 
          (cons (make-auto STARTAUTO-X  (random-range (/ HEIGHT 2) HEIGHT)
                       (random-range 10 50) (pick-image (random 3)))
                (move-autos (ws-loa TEST) TEST))
          (move-autos (ws-loa TEST) TEST)))






(define (loa-handler ws)
  (if (< (random 100) 5) 
      (cons (make-auto STARTAUTO-X  (random-range (/ HEIGHT 2) HEIGHT)
                       (random-range 10 50) (pick-image (random 3)))
            (move-autos (ws-loa ws) ws))
      (move-autos (ws-loa ws) ws)))
  




(check-expect (move-autos empty (make-ws TL1 empty)) empty)




(check-expect (move-autos (list A1 A4) (make-ws TL1 (list A1 A4)))
              (list (move-one-auto A1 (make-ws TL1 (list A1 A4)))))

(check-expect (move-autos (list A1 A2 A3 A4) (make-ws TL1 (list A1 A2 A3 A4)))
              (list (move-one-auto A1 (make-ws TL1 (list A1 A2 A3 A4)))
                    (move-one-auto A2 (make-ws TL1 (list A1 A2 A3 A4)))))

(check-expect (move-autos (list A1 A2 A3 A4) (make-ws TL2 (list A1 A2 A3 A4)))
              (list (move-one-auto A1 (make-ws TL2 (list A1 A2 A3 A4)))
                    (move-one-auto A2 (make-ws TL2 (list A1 A2 A3 A4)))))
                    
(check-expect (move-autos (list A1 A2 A3 A4) (make-ws TL3 (list A1 A2 A3 A4)))
              (list (move-one-auto A1 (make-ws TL3 (list A1 A2 A3 A4)))
                    (move-one-auto A2 (make-ws TL3 (list A1 A2 A3 A4)))))


(define (move-autos loa ws)
  (cond [(empty? loa) empty]
        [(> (auto-x (move-one-auto (first loa) ws)) WIDTH)
         (move-autos (rest loa) ws)]
        [else
         (cons (move-one-auto (first loa) ws) (move-autos (rest loa) ws))]))

(check-expect (move-one-auto A1 (make-ws TL1 A1)) 
              (make-auto
               (update-x (auto-x A1) (auto-dx A1) 1)
               (auto-y A1) (auto-dx A1)(auto-image A1)))

(check-expect (move-one-auto A1 (make-ws TL2 A1))
              (make-auto
               (update-x (auto-x A1) (auto-dx A1) 0.75) 
               (auto-y A1) (auto-dx A1)(auto-image A1)))

(check-expect (move-one-auto A1 (make-ws TL3 A1))
              (make-auto
               (update-x (auto-x A1) (auto-dx A1) 0.5) 
               (auto-y A1) (auto-dx A1)(auto-image A1)))

(check-expect (move-one-auto A5 (make-ws TL3 A5)) A5) 
              

(check-expect (move-one-auto A4 (make-ws TL3 A4))
              (make-auto
               (update-x (auto-x A4) (auto-dx A4) 1) 
               (auto-y A4) (auto-dx A4)(auto-image A4)))




(define (move-one-auto auto ws)
  (cond [(or (> (auto-x auto) (/ WIDTH 2)) 
             (string=? (traffic-light-color (ws-tl ws)) "GREEN"))
         (make-auto
          (update-x (auto-x auto) (auto-dx auto) 1)
          (auto-y auto) (auto-dx auto)(auto-image auto))]
        [(string=? (traffic-light-color (ws-tl ws)) "YELLOW")
         (make-auto
          (update-x (auto-x auto) (auto-dx auto) .75)
          (auto-y auto) (auto-dx auto)(auto-image auto))]
        [(string=? (traffic-light-color (ws-tl ws)) "RED")
         (if (> (auto-x auto) (* WIDTH .45))
             (make-auto
              (update-x (auto-x auto) (auto-dx auto) 0)
              (auto-y auto) (auto-dx auto)(auto-image auto))
             (make-auto
              (update-x (auto-x auto) (auto-dx auto) .5)
              (auto-y auto) (auto-dx auto)(auto-image auto)))]))





(check-expect (update-x 400 50 1) 450) 
(check-expect (update-x 0 10 0.5) 5) 
(check-expect (update-x 400 30 0.75) 422.5) 

(define (update-x x dx mult)
  (+ x (* dx mult)))         

(check-expect (render START)
              (place-image (render-light (ws-tl START)) X-POS Y-POS
                (place-image (text
                             (string-append "Timer: " (number->string
                                                       (ticks->seconds (traffic-light-timer (ws-tl START)))))
                             FONTSIZE FONTCOLOR) (* WIDTH 0.25) (* HEIGHT .1) (render-autos (ws-loa START)))))
(check-expect (render TEST)
              (place-image (render-light (ws-tl TEST)) X-POS Y-POS
                (place-image (text
                             (string-append "Timer: " (number->string
                                                       (ticks->seconds (traffic-light-timer (ws-tl TEST)))))
                             FONTSIZE FONTCOLOR) (* WIDTH 0.25) (* HEIGHT .1) (render-autos (ws-loa TEST)))))



(define (render ws)
  (place-image (render-light (ws-tl ws)) X-POS Y-POS
               (place-image (text
                             (string-append "Timer: " (number->string
                                                       (ticks->seconds (traffic-light-timer (ws-tl ws)))))
                             FONTSIZE FONTCOLOR) (* WIDTH 0.25) (* HEIGHT .1) (render-autos (ws-loa ws)))))


(check-expect (render-light TL1) GREENLIGHT)
(check-expect (render-light TL2) YELLOWLIGHT)
(check-expect (render-light TL3) REDLIGHT)



(define (render-light tl)
  (cond [(string=? (traffic-light-color tl) "GREEN") GREENLIGHT]
        [(string=? (traffic-light-color tl) "YELLOW") YELLOWLIGHT]
        [else REDLIGHT]))

(check-expect (render-autos LOA1) 
              MTS)
(check-expect (render-autos LOA2)
              (place-image (auto-image A1) (auto-x A1) (auto-y A1)
                           (place-image (auto-image A2) (auto-x A2)
                                        (auto-y A2)
                                        (place-image (auto-image A3) (auto-x A3)
                                                     (auto-y A3)
                                                     MTS))))



(define (render-autos loa)
  (if (empty? loa) MTS
      (place-image (auto-image (first loa)) (auto-x (first loa))
                   (auto-y (first loa)) (render-autos (rest loa)))))
  



