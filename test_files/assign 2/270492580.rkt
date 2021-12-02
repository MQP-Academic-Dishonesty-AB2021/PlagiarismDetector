

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |traffic signal starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


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


(define RED-LIT
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))

(define YELLOW-LIT
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))

(define GREEN-LIT
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)


(define BOTTOM-OF-LIGHT (+ Y-POS
                           (* 3 LIGHT-RADIUS)
                           (ceiling (/ (image-height AUTO-IMAGE1) 2))))









(define-struct auto (xpos ypos speed img))

 






 





(define-struct world (loa ticks))

 






 



(define (main world)
  (big-bang world
    (on-tick update-world) 
    (on-draw draw-world))) 





(define auto1 (make-auto 200 250 4 AUTO-IMAGE1))
(define auto2 (make-auto 500 300 5 AUTO-IMAGE2))
(define auto3 (make-auto 300 500 8 AUTO-IMAGE3))

(define START (make-world empty 0))
(define MID (make-world (list auto1 auto2 auto3) (* 4 TICKS-SECOND)))

(define (update-world world)
  (make-world (check-auto-cutoff
               (update-loa
                (can-add-auto
                 (world-loa world))
                (get-color-from-time (world-ticks world))))
              (+ (world-ticks world) 1)))



(check-expect (check-auto-cutoff empty) empty)
(check-expect (check-auto-cutoff (list auto1 auto2)) (list auto1 auto2))
(check-expect (check-auto-cutoff (list (make-auto (+ WIDTH 1) 400 8 AUTO-IMAGE1))) empty)

(define (check-auto-cutoff loa)
  (cond [(empty? loa) empty]
        [else
         (if (<= (auto-xpos (first loa)) WIDTH)
             (cons (first loa) (check-auto-cutoff (rest loa)))
             (check-auto-cutoff (rest loa)))]))



(check-expect (draw-world START) (place-images (append (list (draw-traffic-light (get-color-from-time (world-ticks START)))
                                                             (draw-text (ticks->seconds (world-ticks START))))
                                                       (draw-autos (world-loa START)))
                                               (append (list (make-posn X-POS Y-POS)
                                                             (make-posn (+ X-POS 100) Y-POS))
                                                       (get-auto-positions (world-loa START)))
                                               MTS))
(check-expect (draw-world MID) (place-images (append (list (draw-traffic-light (get-color-from-time (world-ticks MID)))
                                                           (draw-text (ticks->seconds (world-ticks MID))))
                                                     (draw-autos (world-loa MID)))
                                             (append (list (make-posn X-POS Y-POS)
                                                           (make-posn (+ X-POS 100) Y-POS))
                                                     (get-auto-positions (world-loa MID)))
                                             MTS))

(define (draw-world world)
  (place-images (append (list (draw-traffic-light (get-color-from-time (world-ticks world)))
                              (draw-text (ticks->seconds (world-ticks world))))
                        (draw-autos (world-loa world)))
                (append (list (make-posn X-POS Y-POS)
                              (make-posn (+ X-POS 100) Y-POS))
                        (get-auto-positions (world-loa world)))
                MTS))



(check-expect (draw-autos empty) empty)
(check-expect (draw-autos (list auto1 auto2)) (list AUTO-IMAGE1 AUTO-IMAGE2))

(define (draw-autos loa)
  (cond [(empty? loa) empty]
        [else
         (cons (auto-img (first loa))
               (draw-autos (rest loa)))]))



(define (get-auto-positions loa)
  (cond [(empty? loa) empty]
        [else
         (cons (make-posn (auto-xpos (first loa)) (auto-ypos (first loa)))
               (get-auto-positions (rest loa)))]))



(check-expect (draw-traffic-light "green") GREEN-LIT)
(check-expect (draw-traffic-light "red") RED-LIT)
(check-expect (draw-traffic-light "blue") empty-image)

(define (draw-traffic-light light)
  (cond [(string=? light "green") GREEN-LIT]
        [(string=? light "yellow") YELLOW-LIT]
        [(string=? light "red") RED-LIT]
        [else empty-image]))



(check-expect (get-color-from-time 0) "green")
(check-expect (get-color-from-time (* 4 TICKS-SECOND)) "green")
(check-expect (get-color-from-time (* 5 TICKS-SECOND)) "yellow")
(check-expect (get-color-from-time (* 6 TICKS-SECOND)) "yellow")
(check-expect (get-color-from-time (* 7 TICKS-SECOND)) "red")
(check-expect (get-color-from-time (* 10 TICKS-SECOND)) "red")
(check-expect (get-color-from-time (* 11 TICKS-SECOND)) "green")
(check-expect (get-color-from-time (* 13 TICKS-SECOND)) "green")

(define (get-color-from-time ticks)
  (cond [(< (modulo (ticks->seconds ticks) 11) GREEN-LENGTH) "green"]
        [(< (modulo (ticks->seconds ticks) 11) (+ GREEN-LENGTH YELLOW-LENGTH)) "yellow"]
        [else "red"]))



(check-expect (draw-text 0) (text (number->string 0) 24 "black"))
(check-expect (draw-text 11) (text (number->string 11) 24 "black"))

(define (draw-text seconds)
  (text (number->string seconds) 24 "black"))



(check-expect (update-auto (make-auto 200 250 4 AUTO-IMAGE1) "green") (make-auto 204 250 4 AUTO-IMAGE1))
(check-expect (update-auto (make-auto 500 300 5 AUTO-IMAGE2) "yellow") (make-auto 505 300 5 AUTO-IMAGE2))

(define (update-auto auto light)
  (make-auto (+ (auto-xpos auto) (* (auto-speed auto) (get-speed-factor auto light)))
             (auto-ypos auto)
             (auto-speed auto)
             (auto-img auto)))


(check-expect (get-speed-factor auto1 "green") 1)
(check-expect (get-speed-factor auto2 "yellow") 1)
(check-expect (get-speed-factor auto3 "red") .5)

(define (get-speed-factor auto light)
  (if (<= (auto-xpos auto) X-POS)
      (cond [(string=? light "green") 1]
            [(string=? light "yellow") .75]
            [(string=? light "red") (if (>= (+ (auto-xpos auto) (auto-speed auto)) X-POS)
                                        0
                                        .5)])
      1))



(check-expect (update-loa empty "green") empty)
(check-expect (update-loa (list auto1 auto2) "green")
              (list (make-auto 204 250 4 AUTO-IMAGE1)
                    (make-auto 505 300 5 AUTO-IMAGE2)))

(define (update-loa loa light)
  (cond [(empty? loa) empty]
        [else
         (cons (update-auto (first loa) light)
               (update-loa (rest loa) light))]))



(define (spawn-auto bool)
  (make-auto 0
             (+ (random (- HEIGHT BOTTOM-OF-LIGHT)) BOTTOM-OF-LIGHT)
             (+ (random 6) 5)
             (pick-image (random 3))))



(define (can-add-auto loa)
  (if (= (random 20) 0)
      (cons (spawn-auto true) loa)
      loa))








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

(main START)

