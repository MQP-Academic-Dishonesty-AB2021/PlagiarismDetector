

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname assignment2part1-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define XMID (/ WIDTH 2))
(define X-POS XMID) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))

(define TICKS-SECOND 28)

(define AUTO-SPAWNRATE 5) 

(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 

(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)


(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)



(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))


(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)




(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))


(define CYCLE-LENGTH 
  (+ RED-LENGTH YELLOW-LENGTH GREEN-LENGTH)) 

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

(define DISPLAY-X-POS 60)
(define DISPLAY-Y-POS 60)







(define-struct auto (model speed x y))


 


 










(define-struct worldstate (color ticks loa))



(define START (make-worldstate "green" 0 empty))

(define (main ws)
  (big-bang ws
    (on-tick   time-tick)
    (to-draw   draw-all)))


(check-expect (counter "" 0) (text "0" 24 "black"))
(check-expect (counter "Test: " 100) (text "Test: 100" 24 "black"))



(define (counter words num)
  (text (string-append words (number->string num)) 24 "black"))












(define (time-tick ws)
  (make-worldstate (worldstate-color (change-color ws))
                   (+ (worldstate-ticks ws) 1) (new-auto-chance (worldstate-loa ws) (worldstate-color (change-color ws)))))


(check-expect (change-color (make-worldstate "red" 0 empty)) 
              (make-worldstate "green" 0 empty))
(check-expect (change-color (make-worldstate "green" (- (* GREEN-LENGTH TICKS-SECOND) 1) empty)) 
              (make-worldstate "green" (- (* GREEN-LENGTH TICKS-SECOND) 1) empty))
(check-expect (change-color (make-worldstate "green" (* GREEN-LENGTH TICKS-SECOND) empty)) 
              (make-worldstate "yellow" (* GREEN-LENGTH TICKS-SECOND) empty))
(check-expect (change-color (make-worldstate "yellow" (* (+ GREEN-LENGTH YELLOW-LENGTH) TICKS-SECOND) empty)) 
              (make-worldstate "red" (* (+ GREEN-LENGTH YELLOW-LENGTH) TICKS-SECOND) empty))



(define (change-color ws)
  (cond
    [(< (modulo (ticks->seconds (worldstate-ticks ws)) CYCLE-LENGTH) GREEN-LENGTH)
     (make-worldstate "green" (worldstate-ticks ws) (worldstate-loa ws))]
    [(>= (modulo (ticks->seconds (worldstate-ticks ws)) CYCLE-LENGTH) (+ GREEN-LENGTH YELLOW-LENGTH))
     (make-worldstate "red" (worldstate-ticks ws) (worldstate-loa ws))]
    [else
     (make-worldstate "yellow" (worldstate-ticks ws) (worldstate-loa ws))]))


(check-expect (state-to-color (make-worldstate "" 0 empty)) GREEN-LIGHT)
(check-expect (state-to-color (make-worldstate "green" 0 empty)) GREEN-LIGHT)
(check-expect (state-to-color (make-worldstate "yellow" 0 empty)) YELLOW-LIGHT)
(check-expect (state-to-color (make-worldstate "red" 0 empty)) RED-LIGHT)



(define (state-to-color ws)
  (cond
    [(string=? (worldstate-color ws) "red") RED-LIGHT]
    [(string=? (worldstate-color ws) "yellow") YELLOW-LIGHT]
    [else GREEN-LIGHT]))




(define (draw-all ws)
  (place-image (counter "Time: " (ticks->seconds (worldstate-ticks ws)))
               DISPLAY-X-POS DISPLAY-Y-POS
               (place-image (state-to-color (change-color ws))
                            X-POS Y-POS
                            (get-auto-images (worldstate-loa ws)))))


(check-expect (get-auto-images empty) MTS)
(check-expect (get-auto-images (list (make-auto 0 0 100 200) (make-auto 1 0 200 300) (make-auto 2 0 300 400)))
              (place-image AUTO-IMAGE1 100 200 (place-image AUTO-IMAGE2 200 300 (place-image AUTO-IMAGE3 300 400 MTS))))



(define (get-auto-images loa)
  (cond [(empty? loa) MTS]
        [else (place-image (pick-image (auto-model (first loa))) (auto-x (first loa)) (auto-y (first loa))
                           (get-auto-images (rest loa)))]))






(define (new-auto-chance loa color)
  (cond [(<= (random 101) AUTO-SPAWNRATE)
         (cons (make-auto (random 3) (+ (random 9) 2) 0 (+ (random (/ HEIGHT 2)) (/ HEIGHT 2))) (move-autos loa color))]
        [else (move-autos loa color)]))


(check-expect (move-autos empty "") empty)
(check-expect (move-autos (list (make-auto 0 2 XMID 100) (make-auto 2 5 (- XMID 3) 100)
                                (make-auto 1 4 (- (/ XMID 2) 10) 100)) "red")
              (list (make-auto 0 2 XMID 100) (make-auto 2 5 XMID 100)
                    (make-auto 1 4 (+ 4 (- (/ XMID 2) 10)) 100)))
(check-expect (move-autos (list (make-auto 1 5 (* XMID 1.5) 100) (make-auto 0 2 (* XMID 2.5) 100)) "yellow")
              (list (make-auto 1 5 (+ 5 (* XMID 1.5)) 100)))
(check-expect (move-autos (list (make-auto 1 8 (* XMID 0.75) 250)) "green")
              (list (make-auto 1 8 (+ 8 (* XMID 0.75)) 250)))




(define (move-autos loa color)
  (cond [(empty? loa) empty]
        [(<= (auto-x (first loa)) WIDTH)
         (cons (check-pos (first loa) color) (move-autos (rest loa) color))]
        [else (move-autos (rest loa) color)]
        ))

(check-expect (check-pos (make-auto 0 0 0 0) "") (make-auto 0 0 0 0)) 
(check-expect (check-pos (make-auto 0 2 XMID 100) "red") (make-auto 0 2 XMID 100)) 
(check-expect (check-pos (make-auto 0 5 (- XMID 3) 100) "red") (make-auto 0 5 XMID 100)) 
(check-expect (check-pos (make-auto 0 10 (- (/ XMID 2) 5) 100) "red")
              (make-auto 0 10 (+ (* 10 0.5) (- (/ XMID 2) 5)) 100)) 
(check-expect (check-pos (make-auto 0 4 (- (/ XMID 2) 10) 100) "red")
              (make-auto 0 4 (+ 4 (- (/ XMID 2) 10)) 100)) 
(check-expect (check-pos (make-auto 0 8 (- (/ XMID 2) 3) 100) "yellow")
              (make-auto 0 8 (+ (* 8 0.75) (- (/ XMID 2) 3)) 100)) 
(check-expect (check-pos (make-auto 0 4 (- (/ XMID 2) 10) 100) "yellow")
              (make-auto 0 4 (+ 4 (- (/ XMID 2) 10)) 100)) 
(check-expect (check-pos (make-auto 0 5 (- XMID 3) 100) "green")
              (make-auto 0 5 (+ 5 (- XMID 3)) 100)) 





(define (check-pos anauto color)
  (make-auto (auto-model anauto)
             (auto-speed anauto)
             (+ (auto-x anauto) (* (auto-speed anauto) (speed-factor anauto color)))
             (auto-y anauto))
  )


(check-expect (speed-factor (make-auto 0 0 0 0) "") 1)
(check-expect (speed-factor (make-auto 0 2 XMID 100) "red") 0)
(check-expect (speed-factor (make-auto 0 5 (- XMID 3) 100) "red") (/ (- XMID (- XMID 3)) 5))
(check-expect (speed-factor (make-auto 0 10 (/ XMID 2) 100) "red") 0.5)
(check-expect (speed-factor (make-auto 0 4 (- (/ XMID 2) 10) 100) "red") 1)
(check-expect (speed-factor (make-auto 0 8 (+ (/ XMID 2) 3) 100) "yellow") 0.75)
(check-expect (speed-factor (make-auto 0 4 (- (/ XMID 2) 10) 100) "yellow") 1)
(check-expect (speed-factor (make-auto 0 5 (- XMID 3) 100) "green") 1)






(define (speed-factor anauto color)
  (cond
    [(and (string=? color "red") (>= (+ (auto-x anauto) (auto-speed anauto)) XMID)
          (<= (auto-x anauto) XMID)) 
     (/ (- XMID (auto-x anauto)) (auto-speed anauto))]
    [(and (string=? color "red") (>= (+ (auto-x anauto) (auto-speed anauto)) (/ XMID 2))
          (< (auto-x anauto) XMID)) 
     0.5]
    [(and (string=? color "yellow") (>= (+ (auto-x anauto) (auto-speed anauto)) (/ XMID 2))
          (< (auto-x anauto) XMID)) 
     0.75]
    [else 1])) 
