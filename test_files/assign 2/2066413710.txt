

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Jason_Angie_Traffic_Light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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








(define-struct light (color time))









(define-struct auto (x dx starting-speed y img))






 

(define loa-start (list (make-auto 0 0 0 0 (square 0 "outline" "white"))))






(define-struct ws (light loa total-time))

(define START (make-ws (make-light "green" 0) loa-start 0))

(define (main ws)
  (big-bang ws
    (on-tick    change-world) 
    (to-draw    draw-world)))







(define (change-world ws)
  (make-ws (next-light (ws-light ws))
           (get-loa (could-add-auto(ws-loa ws)) (light-color (ws-light ws)))
           (add1 (ws-total-time ws)))) 

(check-expect (switch-light (make-light "green" 0) GREEN-LENGTH "yellow") (make-light "green" (add1 0)))
(check-expect (switch-light (make-light "green" 140) GREEN-LENGTH "yellow") (make-light "yellow" 0))
(check-expect (next-light (make-light "green" 0))  (make-light "green" (add1 0)))
(check-expect (next-light (make-light "green" 140))  (make-light "yellow"  0))
(check-expect (next-light (make-light "yellow" 28))  (make-light "yellow" (add1 28)))
(check-expect (next-light (make-light "red" (* 28 4)))  (make-light "green"  0))
              


(define (next-light aLight)
  (cond [(string=? (light-color aLight) "green")
         (switch-light aLight GREEN-LENGTH "yellow")]

        [(string=? (light-color aLight) "yellow")
         (switch-light aLight YELLOW-LENGTH "red")]

        [(string=? (light-color aLight) "red")
         (switch-light aLight RED-LENGTH "green")]))
  


(define (switch-light ws length nextColor)
  (if (= (ticks->seconds (light-time ws)) length)
      (make-light nextColor 0)
      (make-light (light-color ws) (add1 (light-time ws)))))

  




(define (draw-world ws)
  (place-images 
   (append (list (draw-light (ws-light ws))
                 (text (number->string (ticks->seconds(ws-total-time ws))) 24 "black"))
           (get-auto-images(ws-loa ws)))
   
   (append (list (make-posn X-POS Y-POS)
                 (make-posn (+ 75 X-POS) Y-POS))
           (get-auto-positons (ws-loa ws)))
   MTS))



(check-expect (get-auto-positons empty) empty)
(check-expect (get-auto-positons auto-list1) (list (make-posn 3 381) (make-posn 285 550)))
(check-expect (get-auto-positons auto-list2) (list (make-posn 285 550)))


(define (get-auto-positons loa)
  (cond [(empty? loa) empty]
        [else (cons (make-posn (auto-x (first loa))
                               (auto-y (first loa)))
                    (get-auto-positons (rest loa)))]))



(check-expect (get-auto-images empty) empty)
(check-expect (get-auto-images auto-list1) (list AUTO-IMAGE1 AUTO-IMAGE2))
(check-expect (get-auto-images auto-list2) (list AUTO-IMAGE2))


(define (get-auto-images loa)
  (cond [(empty? loa) empty]
        [else (cons (auto-img (first loa))
                    (get-auto-images (rest loa)))]))






(check-expect (draw-light (make-light "green" 3)) (above
                                                   (circle LIGHT-RADIUS "outline" "red")
                                                   (circle LIGHT-RADIUS "outline" "yellow")
                                                   (circle LIGHT-RADIUS "solid" "green")))

(check-expect (draw-light (make-light "yellow" 4)) (above
                                                    (circle LIGHT-RADIUS "outline" "red")
                                                    (circle LIGHT-RADIUS "solid" "yellow")
                                                    (circle LIGHT-RADIUS "outline" "green")))

(check-expect (draw-light (make-light "red" 3)) (above
                                                 (circle LIGHT-RADIUS "solid" "red")
                                                 (circle LIGHT-RADIUS "outline" "yellow")
                                                 (circle LIGHT-RADIUS "outline" "green")))

(define (draw-light light)
  (cond [(string=? "green" (light-color light))
         (make-bulbs "outline" "outline" "solid")]
        [(string=? "yellow" (light-color light))
         (make-bulbs "outline" "solid" "outline")]
        [(string=? "red" (light-color light))
         (make-bulbs "solid" "outline" "outline")]))







(check-expect (make-bulbs "outline" "outline" "solid") (above
                                                        (circle LIGHT-RADIUS "outline" "red")
                                                        (circle LIGHT-RADIUS "outline" "yellow")
                                                        (circle LIGHT-RADIUS "solid" "green")))

(check-expect (make-bulbs "outline" "solid" "outline") (above
                                                        (circle LIGHT-RADIUS "outline" "red")
                                                        (circle LIGHT-RADIUS "solid" "yellow")
                                                        (circle LIGHT-RADIUS "outline" "green")))

(check-expect (make-bulbs "solid" "outline" "outline") (above
                                                        (circle LIGHT-RADIUS "solid" "red")
                                                        (circle LIGHT-RADIUS "outline" "yellow")
                                                        (circle LIGHT-RADIUS "outline" "green")))

(define (make-bulbs redState yellowState greenSate)
  (above
   (circle LIGHT-RADIUS redState "red")
   (circle LIGHT-RADIUS yellowState "yellow")
   (circle LIGHT-RADIUS greenSate "green")))
  
   







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)


(define auto1 (make-auto 3 4 9 381 AUTO-IMAGE1))
(define auto2 (make-auto 285 3 6 550  AUTO-IMAGE2))

(define auto-list1 (list auto1 auto2))
(define auto-list2 (list auto2))








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




(define (could-add-auto loa)
  (if (= (random 20) 0)
      (cons (random-auto (add1 (random 10))) loa)
      loa))



(define (random-auto speed)
  (make-auto 0
             speed
             speed
             (+ (/ HEIGHT 2) (random (/ HEIGHT 2)))
             (pick-image (random 3))))

(check-expect (get-loa empty "green") empty)
(check-expect (get-loa auto-list1 "green") (append (advance-auto auto1 "green") (advance-auto auto2 "green"))) 
(check-expect (get-loa auto-list2 "yellow") (advance-auto auto2 "yellow"))
(check-expect (get-loa auto-list2 "red") (advance-auto auto2 "red"))




(define (get-loa loa color)
  (cond [(empty? loa) empty]
        [else (append (advance-auto(first loa) color)
                      (get-loa (rest loa) color))]))


(check-expect (advance-auto auto1 "green") (list (make-auto 7 9 9 381 AUTO-IMAGE1)))
(check-expect (advance-auto auto1 "red") (list (make-auto 7 4.5 9 381 AUTO-IMAGE1)))
(check-expect (advance-auto auto2 "yellow") (list (make-auto 288 4.5 6 550 AUTO-IMAGE2)))




(define (advance-auto auto color)
  (cond [(> (auto-x auto) WIDTH) empty]
        [else (list (make-auto (+ (auto-dx auto) (auto-x auto))
                               (new-dx auto color)
                               (auto-starting-speed auto)
                               (auto-y auto)
                               (auto-img auto)))]))



(check-expect (new-dx auto1 "green") 9)
(check-expect (new-dx auto1 "red") 4.5)
(check-expect (new-dx auto2 "green") 6)
(check-expect (new-dx auto2 "yellow") 4.5)




(define (new-dx auto color)
  (cond
    
    [(string=? color "green")
     (auto-starting-speed auto)]
    [(and (< (auto-x auto) X-POS) (string=? color "yellow"))
     (* .75 (auto-starting-speed auto))]
    [(and (string=? color "red") (< (auto-x auto) X-POS))
     (* .5 (auto-starting-speed auto))]
    [(and (string=? color "red") (<= (auto-x auto) (+ 10 X-POS)))
     0]
    [else (auto-starting-speed auto)]))
        


