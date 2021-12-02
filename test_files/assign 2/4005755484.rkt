

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define REDL
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))

(define GREENL
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))

(define YELLOWL
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)













(define-struct light (change-time color))

(define (fn-for-light alight)
  (cond [(... (light-change-time alight) (string=? "r" (light-color alight))) ...]
        [(... (light-change-time alight) (string=? "g" (light-color alight))) ...]
        [(... (light-change-time alight) (string=? "y" (light-color alight))) ...]))

(define L0 (make-light 0 "r"))









(define-struct auto (x y speed type))

(define (fn-for-auto aauto)
  (... (auto-x aauto)
       (auto-y aauto)
       (auto-speed aauto)
       (auto-type aauto)))

(define A0 (make-auto 0 (/ HEIGHT 2) 3 AUTO-IMAGE1))







(define (fn-for-loa loa)
  (cond [(empty? loa) ...]
        [else (... (fn-for-auto (first loa))
                   (fn-for-loa (rest loa)))]))








(define-struct world-state (time-elapsed lights autos))

(define (fn-for-ws ws)
  (... (world-state-time-elapsed ws)
       (fn-for-light (world-state-lights ws))
       (fn-for-loa (world-state-autos ws))))


(define START (make-world-state 0 L0 empty))
(define WS0 (make-world-state 0 (make-light 0 "g") (list (make-auto 0 300 10 AUTO-IMAGE1))))
(define WS1 (make-world-state 0 (make-light 0 "y") (list (make-auto 0 300 10 AUTO-IMAGE1))))










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







(define (main ws)
  (big-bang ws
    (on-tick update)
    (to-draw render)))







(define (update ws)
  (make-world-state (add1 (world-state-time-elapsed ws))
                    (update-light (world-state-lights ws))
                    (create-auto ws)))







(define (update-light alight)
  (local ((define cur-time (ticks->seconds (light-change-time alight))))
    (cond [(and (>= cur-time RED-LENGTH)
                (string=? "r" (light-color alight)))
           (make-light 0 "g")]
          [(and (>= cur-time GREEN-LENGTH)
                (string=? "g" (light-color alight)))
           (make-light 0 "y")]
          [(and (>= cur-time YELLOW-LENGTH)
                (string=? "y" (light-color alight)))
           (make-light 0 "r")]
          [else (make-light (add1 (light-change-time alight)) (light-color alight))])))
                     

(check-expect (update-light L0) (make-light 1 "r"))
(check-expect (update-light (make-light (* 28 RED-LENGTH) "r")) (make-light 0 "g"))
(check-expect (update-light (make-light (* 28 GREEN-LENGTH) "g")) (make-light 0 "y"))
(check-expect (update-light (make-light (* 28 YELLOW-LENGTH) "y")) (make-light 0 "r"))









(define (create-auto ws)
  (if (= 5 (random 20))
      (local ((define new-auto (make-auto 0 (+ (/ HEIGHT 2) (random (floor (/ HEIGHT 2))))
                                          (+ 3 (random 10))
                                          (pick-image (random 3)))))
        (cons new-auto (update-autos (world-state-autos ws) ws)))
      (update-autos (world-state-autos ws) ws)))
  







(define (update-autos loa ws)  
  (cond [(empty? loa) empty]
        [(> (auto-x (first loa)) WIDTH)
         (update-autos (rest loa) ws)]
        [else (cons (update-auto (first loa) ws)
                    (update-autos (rest loa) ws))]))


(check-expect (update-autos empty START) empty)
(check-expect (update-autos (world-state-autos WS0) WS0) (list (make-auto 10 300 10 AUTO-IMAGE1)))
(check-expect (update-autos (list (make-auto 0 300 10 AUTO-IMAGE1) (make-auto 10 300 10 AUTO-IMAGE1)) WS0)
              (list (make-auto 10 300 10 AUTO-IMAGE1) (make-auto 20 300 10 AUTO-IMAGE1)))












(define (update-auto aauto ws)
  (local ((define l-color (light-color (world-state-lights ws))))
    (if (> (auto-x aauto) X-POS)
        (change-x aauto (auto-speed aauto))
        (cond [(string=? "g" l-color) (change-x aauto (auto-speed aauto))]
              [(string=? "y" l-color) (change-x aauto (* 3/4 (auto-speed aauto)))]
              [else (if (>= (+ (auto-x aauto) (auto-speed aauto)) X-POS)
                        (make-auto X-POS (auto-y aauto) (auto-speed aauto) (auto-type aauto))
                        (change-x aauto (* 1/2 (auto-speed aauto))))]))))

(check-expect (update-auto (make-auto 0 300 10 AUTO-IMAGE1) WS0) (make-auto 10 300 10 AUTO-IMAGE1))
(check-expect (update-auto (make-auto 0 300 10 AUTO-IMAGE1) START) (make-auto 5 300 10 AUTO-IMAGE1))
(check-expect (update-auto (make-auto X-POS 300 10 AUTO-IMAGE1) START) (make-auto X-POS 300 10 AUTO-IMAGE1))
(check-expect (update-auto (make-auto (+ 1 X-POS) 300 10 AUTO-IMAGE1) START) (make-auto (+ 11 X-POS) 300 10 AUTO-IMAGE1))
(check-expect (update-auto (make-auto 0 300 10 AUTO-IMAGE1) WS1) (make-auto (* 3/4 10) 300 10 AUTO-IMAGE1))







(define (change-x aauto speed)
  (make-auto (+ speed (auto-x aauto)) (auto-y aauto) (auto-speed aauto) (auto-type aauto)))

(check-expect (change-x A0 0) A0)
(check-expect (change-x A0 5) (make-auto 5 (/ HEIGHT 2) 3 AUTO-IMAGE1))
(check-expect (change-x (make-auto 100 100 100 AUTO-IMAGE1) 5) (make-auto 105 100 100 AUTO-IMAGE1))








(define (render ws)
  (place-image/align (render-light ws) (- X-POS LIGHT-RADIUS) Y-POS "left" "middle"
                     (render-autos (world-state-autos ws))))


(check-expect (render START) (place-image/align (beside REDL (square 40 "solid" "white") (text "0" 24 "black"))
                                                (- X-POS LIGHT-RADIUS) Y-POS "left" "middle" MTS))
(check-expect (render (make-world-state 20 (make-light 0 "g") empty))
              (place-image/align (beside GREENL (square 40 "solid" "white")
                                         (text (number->string (ticks->seconds 20)) 24 "black"))
                                 (- X-POS LIGHT-RADIUS) Y-POS "left" "middle" MTS))






(define (render-autos loa)
    (cond [(empty? loa) MTS]
          [else (place-image (auto-type (first loa)) (auto-x (first loa))
                             (auto-y (first loa)) (render-autos (rest loa)))]))


(check-expect (render-autos empty) MTS)
(check-expect (render-autos (list (make-auto 0 300 10 AUTO-IMAGE1))) (place-image AUTO-IMAGE1 0 300 MTS))
(check-expect (render-autos (list (make-auto 0 300 10 AUTO-IMAGE1) (make-auto 10 400 10 AUTO-IMAGE2)))
              (place-image AUTO-IMAGE2 10 400 (place-image AUTO-IMAGE1 0 300 MTS)))







(define (render-light ws)
  (beside (choose-light (world-state-lights ws))
          (square 40 "solid" "white")
          (text (number->string (ticks->seconds (world-state-time-elapsed ws))) 24 "black")))
  

(check-expect (render-light START) (beside REDL (square 40 "solid" "white") (text "0" 24 "black")))

(check-expect (render-light (make-world-state 20 (make-light 0 "g") empty))
              (beside GREENL (square 40 "solid" "white") (text (number->string (ticks->seconds 20)) 24 "black")))






(define (choose-light alight)
  (cond[(string=? "r" (light-color alight)) REDL]
       [(string=? "g" (light-color alight)) GREENL]
       [(string=? "y" (light-color alight)) YELLOWL]))

(check-expect (choose-light L0) REDL)
(check-expect (choose-light (make-light 50 "g")) GREENL)
(check-expect (choose-light (make-light 1 "y")) YELLOWL)