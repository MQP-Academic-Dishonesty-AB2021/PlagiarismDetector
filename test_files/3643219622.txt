

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname assignment2_road) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




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

(define SPEED-LIMIT 20) 

(define CAR-SPAWN-RATE 5) 



(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))







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










(define-struct road (tick bulbs autos billboards main-board?))




(define-struct bulb (color length filled?))




(define-struct auto (img x y speed))

(define init-bulbs (list
                    (make-bulb "green" GREEN-LENGTH false)
                    (make-bulb "yellow" YELLOW-LENGTH false)
                    (make-bulb "red" RED-LENGTH false)))

(define init-road (make-road 0 init-bulbs empty (list "" "") true))

(define START init-road)

(define (main road)
  (big-bang road
    (on-tick on-road-tick)
    (on-key board-key)
    (to-draw road-draw)
    )
  )

(define (board-key road key)
  (cond
    [(key=? key "\r") (update-billboard road "\n")]
    [(key=? key "\b") (make-road
                       (road-tick road)
                       (road-bulbs road)
                       (road-autos road)
                       (if (road-main-board? road)
                           (cons
                            (substring (first (road-billboards road)) 0 (if
                                                                         (= (string-length (first (road-billboards road))) 0)
                                                                         0
                                                                         (sub1 (string-length (first (road-billboards road))))
                                                                         ))
                            (cons (first (rest (road-billboards road))) empty))
                           (cons
                            (first (road-billboards road))
                            (cons (substring (first (rest (road-billboards road))) 0 (if
                                                                                      (= (string-length (first (rest (road-billboards road)))) 0)
                                                                                      0
                                                                                      (sub1 (string-length (first (rest (road-billboards road)))))
                                                                                      )) empty))
                           )
                       (road-main-board? road)
                       )]
    [(key=? key "\t") (make-road
                       (road-tick road)
                       (road-bulbs road)
                       (road-autos road)
                       (if (road-main-board? road)
                           (cons "" (cons (first (rest (road-billboards road))) empty))
                           (cons (first (road-billboards road)) (cons "" empty))
                           )
                       (road-main-board? road)
                       )]
    [(key=? key "up") (make-road
                       (road-tick road)
                       (road-bulbs road)
                       (road-autos road)
                       (cons (first (road-billboards road)) (cons "" empty))
                       true
                       )]
    [(key=? key "down") (make-road
                         (road-tick road)
                         (road-bulbs road)
                         (road-autos road)
                         (road-billboards road)
                         false
                         )]
    [(key=? key "shift") (make-road
                          (road-tick road)
                          (road-bulbs road)
                          (road-autos road)
                          (road-billboards road)
                          (road-main-board? road)
                          )]
    [else (update-billboard road key)]))

(check-expect (board-key (make-road 0 empty empty (list "H" "") true) "i") (make-road 0 empty empty (list "Hi" "") true))
(check-expect (board-key (make-road 0 empty empty (list "Howdy!" "") true) "\t") (make-road 0 empty empty (list "" "") true))
(check-expect (board-key (make-road 0 empty empty (list "Hey." "") true) "\b") (make-road 0 empty empty (list "Hey" "") true))
(check-expect (board-key (make-road 0 empty empty (list "" "") true) "\b") (make-road 0 empty empty (list "" "") true))
(check-expect (board-key (make-road 0 empty empty (list "" "") false) "\b") (make-road 0 empty empty (list "" "") false))
(check-expect (board-key (make-road 0 empty empty (list "Is that a space?!" "") true) " ") (make-road 0 empty empty (list "Is that a space?! " "") true))
(check-expect (board-key (make-road 0 empty empty (list "How's the air down there?" "") true) "\r") (make-road 0 empty empty (list "How's the air down there?\n" "") true))
(check-expect (board-key (make-road 0 empty empty (list "og message" "Mor") false) "e") (make-road 0 empty empty (list "og message" "More") false))
(check-expect (board-key (make-road 0 empty empty (list "abc" "def") false) "up") (make-road 0 empty empty (list "abc" "") true))
(check-expect (board-key (make-road 0 empty empty (list "abc" "") true) "down") (make-road 0 empty empty (list "abc" "") false))



(define (update-billboard road key)

  (make-road
   (road-tick road)
   (road-bulbs road)
   (road-autos road)
   (if (road-main-board? road)
       (cons (string-append (first (road-billboards road)) key) (cons (first (rest (road-billboards road))) empty))
       (cons (first (road-billboards road)) (cons (string-append (first (rest (road-billboards road))) key) empty))
       )
   (road-main-board? road)
   )
  )

(check-expect (update-billboard (make-road 0 empty empty (list "H" "") true) "i") (make-road 0 empty empty (list "Hi" "") true))
(check-expect (update-billboard (make-road 0 empty empty (list "Woah! Another check-expect" "") true) "?") (make-road 0 empty empty (list "Woah! Another check-expect?" "") true))
(check-expect (update-billboard (make-road 0 empty empty (list "What is this strange character?" "") true) "#") (make-road 0 empty empty (list "What is this strange character?#" "") true))

(define (on-road-tick road)
  (make-road
   (add1 (road-tick road))
   (update-bulbs road)
   (update-autos road)
   (road-billboards road)
   (road-main-board? road)
   )
  )




(check-expect (update-bulbs (make-road 7 init-bulbs empty empty true)) (list (make-bulb "green" GREEN-LENGTH true) (make-bulb "yellow" YELLOW-LENGTH false) (make-bulb "red" RED-LENGTH false)))

(check-expect (update-bulbs (make-road 168 init-bulbs empty empty true)) (list (make-bulb "green" GREEN-LENGTH false) (make-bulb "yellow" YELLOW-LENGTH true) (make-bulb "red" RED-LENGTH false)))

(check-expect (update-bulbs (make-road 280 init-bulbs empty empty true)) (list (make-bulb "green" GREEN-LENGTH false) (make-bulb "yellow" YELLOW-LENGTH false) (make-bulb "red" RED-LENGTH true)))



(define (update-bulbs road)
  (cond
    [(< (cycle-time road) GREEN-LENGTH)
     (set-bulb "green")]
    [(< (cycle-time road) (+ GREEN-LENGTH YELLOW-LENGTH))
     (set-bulb "yellow")]
    [(< (cycle-time road) (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))
     (set-bulb "red")]))

(check-expect (update-bulbs init-road) (list
                                        (make-bulb "green" GREEN-LENGTH true)
                                        (make-bulb "yellow" YELLOW-LENGTH false)
                                        (make-bulb "red" RED-LENGTH false)
                                        ))














(define (set-bulb color)
  (cond [(string=? color "green") (list
                                   (make-bulb "green" GREEN-LENGTH true)
                                   (make-bulb "yellow" YELLOW-LENGTH false)
                                   (make-bulb "red" RED-LENGTH false)
                                   )]
        [(string=? color "yellow") (list
                                    (make-bulb "green" GREEN-LENGTH false)
                                    (make-bulb "yellow" YELLOW-LENGTH true)
                                    (make-bulb "red" RED-LENGTH false)
                                    )]
        [(string=? color "red") (list
                                 (make-bulb "green" GREEN-LENGTH false)
                                 (make-bulb "yellow" YELLOW-LENGTH false)
                                 (make-bulb "red" RED-LENGTH true)
                                 )]
        )
  )

(check-expect (set-bulb "green") (list
                                  (make-bulb "green" GREEN-LENGTH true)
                                  (make-bulb "yellow" YELLOW-LENGTH false)
                                  (make-bulb "red" RED-LENGTH false)))
(check-expect (set-bulb "yellow") (list
                                   (make-bulb "green" GREEN-LENGTH false)
                                   (make-bulb "yellow" YELLOW-LENGTH true)
                                   (make-bulb "red" RED-LENGTH false)))
(check-expect (set-bulb "red") (list
                                (make-bulb "green" GREEN-LENGTH false)
                                (make-bulb "yellow" YELLOW-LENGTH false)
                                (make-bulb "red" RED-LENGTH true)))




(define (cycle-time road)
  (ticks->seconds
   (modulo (road-tick road) (* (+ RED-LENGTH (+ GREEN-LENGTH YELLOW-LENGTH)) TICKS-SECOND))))

(check-expect (cycle-time (make-road 0 empty empty empty true)) 0)






(define (update-autos road)
  (local [(define rnd-spawn (random 100))]
    (if (<= rnd-spawn CAR-SPAWN-RATE)
        (generate-autos road)
        (update-old-autos road)
        )
    )
  )





(define (update-old-autos road)
  (cond
    [(empty? (road-autos road)) empty]
    [(> (auto-x (first (road-autos road))) WIDTH)
     (update-old-autos (make-road (road-tick road) (road-bulbs road) (rest (road-autos road)) (road-billboards road) (road-main-board? road)))]
    [else (cons (make-auto
                 (auto-img (first (road-autos road)))
                 (inc-speed (road-bulbs road) (first (road-autos road)))
                 
                 (auto-y (first (road-autos road)))
                 (auto-speed (first (road-autos road))))
                (update-old-autos (make-road (road-tick road) (road-bulbs road) (rest (road-autos road)) (road-billboards road) (road-main-board? road))))]))

(check-expect (update-old-autos init-road) empty)
(check-expect (update-old-autos (make-road 1 init-bulbs (list (make-auto AUTO-IMAGE1 0 0 1)) empty true)) (list (make-auto AUTO-IMAGE1 1 0 1)))
(check-expect (update-old-autos (make-road 1 init-bulbs (list (make-auto AUTO-IMAGE1 6 0 3)) empty true)) (list (make-auto AUTO-IMAGE1 9 0 3)))



(define (inc-speed bulbs auto)
  (if (<= (auto-x auto) (/ WIDTH 2))
      (cond [(filled-bulb? "yellow" bulbs) (+ (* (auto-speed auto) .75) (auto-x auto))]
            [(filled-bulb? "red" bulbs) (if (and (> (+ (auto-x auto) (auto-speed auto)) (/ WIDTH 2)) (<= (auto-x auto) (/ WIDTH 2)))
                                            (auto-x auto)
                                            (+ (* (auto-speed auto) .5) (auto-x auto))
                                            )]
            [else (+ (auto-speed auto) (auto-x auto))]
            )
      (+ (auto-speed auto) (auto-x auto))
      )
  )

(check-expect (inc-speed (list
                          (make-bulb "green" GREEN-LENGTH true)
                          (make-bulb "yellow" YELLOW-LENGTH false)
                          (make-bulb "red" RED-LENGTH false))
                         (make-auto AUTO-IMAGE1 0 0 1)) 1)

(check-expect (inc-speed (list
                          (make-bulb "green" GREEN-LENGTH true)
                          (make-bulb "yellow" YELLOW-LENGTH false)
                          (make-bulb "red" RED-LENGTH false))
                         (make-auto AUTO-IMAGE1 0 0 3)) 3)

(check-expect (inc-speed (list
                          (make-bulb "green" GREEN-LENGTH false)
                          (make-bulb "yellow" YELLOW-LENGTH false)
                          (make-bulb "red" RED-LENGTH true))
                         (make-auto AUTO-IMAGE1 (/ WIDTH 2) 0 3)) (/ WIDTH 2))



(define (generate-autos road)
  (cons
   (local [(define rnd-img (random 3))
           (define rnd-y (+ (* LIGHT-RADIUS 6) (random (- HEIGHT (* LIGHT-RADIUS 6)))))
           (define rnd-speed (add1 (random (sub1 SPEED-LIMIT))))
           ]
     (make-auto
      (cond [(= rnd-img 0) AUTO-IMAGE1]
            [(= rnd-img 1) AUTO-IMAGE2]
            [else AUTO-IMAGE3])
      0
      rnd-y
      rnd-speed
      ))
   (update-old-autos road)
   ))



(define (road-draw road)
  (place-images (append
                 (list (above
                        (circle LIGHT-RADIUS (if (filled-bulb? "red" (road-bulbs road)) "solid" "outline") "red")
                        (circle LIGHT-RADIUS (if (filled-bulb? "yellow" (road-bulbs road)) "solid" "outline") "yellow")
                        (circle LIGHT-RADIUS (if (filled-bulb? "green" (road-bulbs road)) "solid" "outline") "green"))
                       (text (number->string (ticks->seconds (road-tick road))) 30 "black")
                       (text (if (road-main-board? road) (first (road-billboards road)) (first (rest (road-billboards road)))) 30 "black")
                       )
                 (get-auto-images (road-autos road)))
                
                (append
                 (list (make-posn (/ WIDTH 2) (* LIGHT-RADIUS 3))
                       (make-posn (+ 100 (/ WIDTH 2)) (* LIGHT-RADIUS 3))
                       (make-posn 100 (* LIGHT-RADIUS 3)))
                 (get-auto-positions (road-autos road)))
                
                MTS
                )
  )



(define (get-auto-images autos)
  (cond
    [(empty? autos) empty]
    [else
     (cons (auto-img (first autos))
           (get-auto-images (rest autos)))]))

(check-expect (get-auto-images empty) empty)
(check-expect (get-auto-images (list (make-auto AUTO-IMAGE1 0 0 1))) (list AUTO-IMAGE1))
(check-expect (get-auto-images (list (make-auto AUTO-IMAGE1 0 0 1) (make-auto AUTO-IMAGE2 0 0 1) (make-auto AUTO-IMAGE3 0 0 1))) (list AUTO-IMAGE1 AUTO-IMAGE2 AUTO-IMAGE3))



(define (get-auto-positions autos)
  (cond
    [(empty? autos) empty]
    [else
     (cons (make-posn (auto-x (first autos)) (auto-y (first autos)))
           (get-auto-positions (rest autos)))]))

(check-expect (get-auto-positions empty) empty)
(check-expect (get-auto-positions (list (make-auto AUTO-IMAGE1 0 5 1))) (list (make-posn 0 5)))
(check-expect (get-auto-positions (list (make-auto AUTO-IMAGE1 0 0 1) (make-auto AUTO-IMAGE1 10 10 3))) (list (make-posn 0 0) (make-posn 10 10)))



(define (filled-bulb? color bulbs)
  (cond [(string=? color (bulb-color (first bulbs))) (bulb-filled? (first bulbs))]
        [else (filled-bulb? color (rest bulbs))]
        )
  )

(check-expect (filled-bulb? "green" (list (make-bulb "green" GREEN-LENGTH true) (make-bulb "yellow" YELLOW-LENGTH false) (make-bulb "red" GREEN-LENGTH false))) true)
(check-expect (filled-bulb? "yellow" (list (make-bulb "green" GREEN-LENGTH false) (make-bulb "yellow" YELLOW-LENGTH true) (make-bulb "red" GREEN-LENGTH false))) true)
(check-expect (filled-bulb? "red" (list (make-bulb "green" GREEN-LENGTH true) (make-bulb "yellow" YELLOW-LENGTH false) (make-bulb "red" GREEN-LENGTH false))) false)