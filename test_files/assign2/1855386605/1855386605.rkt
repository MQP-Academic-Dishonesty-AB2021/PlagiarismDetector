

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |traffic signal part 2 done|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))

(define FONTCOLOR "black") 
(define FONTSIZE 30) 
(define TIMER-X-OFFSET 150)
(define TIMER-Y-OFFSET 80)


(define TICKS-SECOND 28) 






(define LIGHT-RADIUS 40) 

(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 




(define SPAWN-RATE 5)

(define SPAWN-LIMIT (+ Y-POS (* 1.5 LIGHT-RADIUS) 100))

(define YELLOW-SPEED 0.75)

(define RED-SPEED 0.5) 

(define MIN-SPEED 6)
(define MAX-SPEED 10)


(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))


(define-struct ws (ticks loa))




(define-struct auto (x y speed img))






(define auto0 (make-auto 0 400 2 0))
(define auto1 (make-auto 100 400 2 1))
(define auto2 (make-auto 200 400 2 2))
(define auto3 (make-auto (+ WIDTH 1) 500 4 2))

 

 


(define START (make-ws 0 (list (make-auto 0 400 2 2))))

(define (main ws)
  (big-bang ws
    (on-tick update-ws)
    (to-draw render)))











 
 

 

(define (update-ws ws)
  (make-ws (+ 1 (ws-ticks ws))
           (append
            (new-auto 0)
            (update-loa
             (ws-loa ws)
             (sec->color (ticks->seconds (ws-ticks ws)))))))









(check-random (new-auto 0)
              (if (< (random 100) SPAWN-RATE)
                  (cons (make-auto
                         0
                         (+ (random (- HEIGHT SPAWN-LIMIT)) SPAWN-LIMIT)
                         (+ MIN-SPEED (random (- MAX-SPEED MIN-SPEED)))
                         (random 3))
                        empty)
                  empty))
                                                                            



(define (new-auto x)
  (if (< (random 100) SPAWN-RATE)
      (cons (make-auto
             0
             (+ (random (- HEIGHT SPAWN-LIMIT)) SPAWN-LIMIT)
             (+ MIN-SPEED (random (- MAX-SPEED MIN-SPEED)))
             (random 3)) empty)
      empty))










(check-expect (update-loa empty "green") empty)

(check-expect (update-loa (list auto0) "green")
              (list (update-auto auto0 "green")))
(check-expect (update-loa (list auto0) "yellow")
              (list (update-auto auto0 "yellow")))
(check-expect (update-loa (list auto0) "red")
              (list (update-auto auto0 "red")))


(check-expect (update-loa (list auto3) "green") empty)
(check-expect (update-loa (list auto3) "yellow") empty)
(check-expect (update-loa (list auto3) "red") empty)


(check-expect (update-loa (list auto0 auto2) "red")
              (list (update-auto auto0 "red")
                    (update-auto auto2 "red")))
(check-expect (update-loa (list auto0 auto2) "yellow")
              (list (update-auto auto0 "yellow")
                    (update-auto auto2 "yellow")))
(check-expect (update-loa (list auto0 auto2) "green")
              (list (update-auto auto0 "green")
                    (update-auto auto2 "green")))


(check-expect (update-loa (list auto0 auto3) "red")
              (list (update-auto auto0 "red")))
(check-expect (update-loa (list auto1 auto3) "yellow")
              (list (update-auto auto1 "yellow")))
(check-expect (update-loa (list auto2 auto3) "green")
              (list (update-auto auto2 "green")))


(check-expect (update-loa (list auto0 auto1 auto2 auto3) "red")
              (list (update-auto auto0 "red")
                    (update-auto auto1 "red")
                    (update-auto auto2 "red")))

(check-expect (update-loa (list auto0 auto1 auto2 auto3) "yellow")
              (list (update-auto auto0 "yellow")
                    (update-auto auto1 "yellow")
                    (update-auto auto2 "yellow")))

(check-expect (update-loa (list auto0 auto1 auto2 auto3) "green")
              (list (update-auto auto0 "green")
                    (update-auto auto1 "green")
                    (update-auto auto2 "green")))

(define (update-loa loa color)
  (cond [(empty? loa) empty]
        [(off-screen? (first loa)) (update-loa (rest loa) color)]
        [else
         (cons (update-auto (first loa) color)
               (update-loa (rest loa) color))]))








(check-expect (off-screen? auto3) true)
(check-expect (off-screen? auto0) false)

(define (off-screen? a)
  (> (auto-x a) WIDTH))








(check-expect (update-auto (make-auto 0 500 8 1) "green")
              (make-auto (+ 0 8) 500 8 1))

(check-expect (update-auto (make-auto 0 500 8 1) "yellow")
              (make-auto (+ 0 (* 8 0.75)) 500 8 1))


(check-expect (update-auto (make-auto 0 500 6 1) "red")
              (make-auto (+ 0 (* 6 0.5)) 500 6 1))


(check-expect (update-auto (make-auto X-POS 500 6 1) "red")
              (make-auto X-POS 500 6 1))


(check-expect (update-auto (make-auto (+ X-POS 1) 500 6 1) "red")
              (make-auto (+ (+ X-POS 1) 6) 500 6 1))


(check-expect (update-auto (make-auto (+ X-POS 1) 500 6 1) "yellow")
              (make-auto (+ (+ X-POS 1) 6) 500 6 1))

(define (update-auto auto color)
  (make-auto
   (+ (auto-x auto) (cond [(> (auto-x auto) X-POS) (auto-speed auto)]
                          [(string=? color "red")
                           
                           (if (> (+ (auto-x auto) (auto-speed auto)) X-POS)
                               0
                               (* (auto-speed auto) RED-SPEED))]
                          [(string=? color "yellow") (* (auto-speed auto) YELLOW-SPEED)]
                          [else (auto-speed auto)]))
   (auto-y auto)
   (auto-speed auto)
   (auto-img auto)))








(check-expect (render (make-ws 0
                               empty))
              (place-image (text (number->string (ticks->seconds 0))
                                 FONTSIZE FONTCOLOR)
                           (+ X-POS TIMER-X-OFFSET)
                           (+ Y-POS TIMER-Y-OFFSET) 
                           (place-image
                            (draw-bulbs (sec->color (ticks->seconds 0)))
                            X-POS Y-POS
                            MTS)))

(check-expect (render (make-ws ( * TICKS-SECOND (add1 RED-LENGTH))
                               empty))
              (place-image (text
                            (number->string
                             (ticks->seconds
                              ( * TICKS-SECOND (add1 RED-LENGTH))))
                            FONTSIZE FONTCOLOR)
                           (+ X-POS TIMER-X-OFFSET)
                           (+ Y-POS TIMER-Y-OFFSET) 
                           (place-image
                            (draw-bulbs
                             (sec->color
                              (ticks->seconds
                               ( * TICKS-SECOND (add1 RED-LENGTH)))))
                            X-POS Y-POS
                            MTS)))

(check-expect (render (make-ws ( * TICKS-SECOND (add1 RED-LENGTH))
                               (list auto0)))
              (place-image (text
                            (number->string
                             (ticks->seconds
                              ( * TICKS-SECOND (add1 RED-LENGTH))))
                            FONTSIZE FONTCOLOR)
                           (+ X-POS TIMER-X-OFFSET)
                           (+ Y-POS TIMER-Y-OFFSET) 
                           (place-image
                            (draw-bulbs
                             (sec->color
                              (ticks->seconds
                               ( * TICKS-SECOND (add1 RED-LENGTH)))))
                            X-POS Y-POS
                            (draw-loa (list auto0)))))

(check-expect (render (make-ws ( * TICKS-SECOND (add1 RED-LENGTH))
                               (list auto0 auto1)))
              (place-image (text
                            (number->string
                             (ticks->seconds
                              ( * TICKS-SECOND (add1 RED-LENGTH))))
                            FONTSIZE FONTCOLOR)
                           (+ X-POS TIMER-X-OFFSET)
                           (+ Y-POS TIMER-Y-OFFSET) 
                           (place-image
                            (draw-bulbs
                             (sec->color
                              (ticks->seconds
                               ( * TICKS-SECOND (add1 RED-LENGTH)))))
                            X-POS Y-POS
                            (draw-loa (list auto0 auto1)))))

(check-expect (render (make-ws ( * TICKS-SECOND (add1 RED-LENGTH))
                               (list auto0 auto1 auto2)))
              (place-image (text
                            (number->string
                             (ticks->seconds
                              ( * TICKS-SECOND (add1 RED-LENGTH))))
                            FONTSIZE FONTCOLOR)
                           (+ X-POS TIMER-X-OFFSET)
                           (+ Y-POS TIMER-Y-OFFSET) 
                           (place-image
                            (draw-bulbs
                             (sec->color
                              (ticks->seconds
                               ( * TICKS-SECOND (add1 RED-LENGTH)))))
                            X-POS Y-POS
                            (draw-loa (list auto0 auto1 auto2)))))

(define (render ws)
  (place-image
   (text
    (number->string
     (ticks->seconds (ws-ticks ws)))
    FONTSIZE
    FONTCOLOR)
   (+ X-POS TIMER-X-OFFSET)
   (+ Y-POS TIMER-Y-OFFSET) 
   (place-image
    (draw-bulbs
     (sec->color
      (ticks->seconds
       (ws-ticks ws))))
    X-POS Y-POS
    (draw-loa (ws-loa ws)))))







(check-expect (draw-loa empty) MTS)

(check-expect (draw-loa (list auto0))
              (place-image
               (pick-image (auto-img auto0)) (auto-x auto0) (auto-y auto0)
               MTS))

(check-expect (draw-loa (list auto0 auto1))
              (place-image
               (pick-image (auto-img auto1)) (auto-x auto1) (auto-y auto1)
               (place-image
                (pick-image (auto-img auto0)) (auto-x auto0) (auto-y auto0)
                MTS)))

(check-expect (draw-loa (list auto0 auto1 auto2))
              (place-image
               (pick-image (auto-img auto2)) (auto-x auto2) (auto-y auto2)
               (place-image
                (pick-image (auto-img auto1)) (auto-x auto1) (auto-y auto1)
                (place-image
                 (pick-image (auto-img auto0)) (auto-x auto0) (auto-y auto0)
                 MTS))))

(define (draw-loa loa)
  (cond [(empty? loa) MTS]
        [else
         (place-image
          (pick-image (auto-img (first loa)))
          (auto-x (first loa))
          (auto-y (first loa))
          (draw-loa (rest loa)))]))





  

(check-expect (draw-bulbs "red") (above
                                  (circle LIGHT-RADIUS "solid" "red")
                                  (circle LIGHT-RADIUS "outline" "yellow")
                                  (circle LIGHT-RADIUS "outline" "green")))
(check-expect (draw-bulbs "yellow") (above
                                     (circle LIGHT-RADIUS "outline" "red")
                                     (circle LIGHT-RADIUS "solid" "yellow")
                                     (circle LIGHT-RADIUS "outline" "green")))
(check-expect (draw-bulbs "green") (above
                                    (circle LIGHT-RADIUS "outline" "red")
                                    (circle LIGHT-RADIUS "outline" "yellow")
                                    (circle LIGHT-RADIUS "solid" "green")))
(check-expect (draw-bulbs "black") (above
                                    (circle LIGHT-RADIUS "outline" "red")
                                    (circle LIGHT-RADIUS "outline" "yellow")
                                    (circle LIGHT-RADIUS "outline" "green")))


(define (draw-bulbs color)
  (above
   (circle LIGHT-RADIUS (if (string=? color "red") "solid" "outline") "red")
   (circle LIGHT-RADIUS (if (string=? color "yellow") "solid" "outline") "yellow")
   (circle LIGHT-RADIUS (if (string=? color "green") "solid" "outline") "green")))








(check-expect (sec->color 0) "green")
(check-expect (sec->color ( + GREEN-LENGTH -1)) "green")
(check-expect (sec->color ( + GREEN-LENGTH YELLOW-LENGTH -1)) "yellow")
(check-expect (sec->color ( + RED-LENGTH YELLOW-LENGTH GREEN-LENGTH -1)) "red")
(check-expect (sec->color ( + RED-LENGTH YELLOW-LENGTH GREEN-LENGTH 1)) "green")

(define (sec->color sec)
  (local [(define remainder
            (modulo sec (+ RED-LENGTH YELLOW-LENGTH GREEN-LENGTH)))]
    (cond [(< remainder GREEN-LENGTH) "green"]
          [(< remainder (+ GREEN-LENGTH YELLOW-LENGTH)) "yellow"]
          [else "red"])))
                         










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


