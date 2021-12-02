

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Stoplightpart1and2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))

(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)

(define TICKS-SECOND 28) 






(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 
(define TOTAL_LENGTH (+(+ RED-LENGTH GREEN-LENGTH)YELLOW-LENGTH))


(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))
   




(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)


(check-expect (To-ticks 1) 28)
(check-expect (To-ticks 0) 0)
(check-expect (To-ticks 6) 168)




(define (To-ticks seconds)
  (* seconds 28))




(define (main-light ticks)
  (big-bang ticks
    (on-tick increase-tick)
    (to-draw render-light)))



(check-expect(increase-tick 2) 3)
(check-expect(increase-tick 0) 1)

(define (increase-tick ticks)
  (+ 1 ticks))



(check-expect (render-light 0) (place-image(light-on "green") X-POS Y-POS MTS))
(check-expect (render-light (To-ticks GREEN-LENGTH)) (place-image(light-on "yellow") X-POS Y-POS MTS))
(check-expect (render-light (+(To-ticks GREEN-LENGTH) 28)) (place-image(light-on "yellow") X-POS Y-POS MTS))
(check-expect (render-light (- (To-ticks TOTAL_LENGTH) 28)) (place-image(light-on "red") X-POS Y-POS MTS))
(check-expect (render-light (To-ticks TOTAL_LENGTH)) (place-image(light-on "green") X-POS Y-POS MTS))
(check-expect (render-light (+ (To-ticks TOTAL_LENGTH) 28)) (place-image(light-on "green") X-POS Y-POS MTS))            

(define (render-light ticks)
  (place-image(light-on (get-light ticks)) X-POS Y-POS MTS))




(check-expect (get-light (To-ticks 0)) "green")
(check-expect (get-light (To-ticks 4)) "green")
(check-expect (get-light (To-ticks 5)) "yellow")
(check-expect (get-light (To-ticks 6)) "yellow")
(check-expect (get-light (To-ticks 7)) "red")
(check-expect (get-light (To-ticks 11)) "green")
(define (get-light ticks)
  (cond [(< (light-time ticks)  GREEN-LENGTH) "green"]
        [(and(>= (light-time ticks) GREEN-LENGTH) (< (light-time ticks) (+ GREEN-LENGTH  YELLOW-LENGTH)))
         "yellow"]
        [else "red"])) 




(check-expect (light-time 0) 0)
(check-expect (light-time TOTAL_LENGTH) 0)
(check-expect (light-time 28) (remainder (ticks->seconds 28) TOTAL_LENGTH))
(check-expect (light-time 168) (remainder (ticks->seconds 168) TOTAL_LENGTH))
(define (light-time ticks)
  (remainder (ticks->seconds ticks) TOTAL_LENGTH))

(check-expect (light-on "green")   
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "solid" "green")))
(check-expect (light-on "red")
              (above
               (circle LIGHT-RADIUS "solid" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "outline" "green")))
(check-expect (light-on "yellow")
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "solid" "yellow")
               (circle LIGHT-RADIUS "outline" "green")))
(check-expect (light-on "something")
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "outline" "green")))




(define (light-on string)
  (cond [(string=? "green" string)  
         (above
          (circle LIGHT-RADIUS "outline" "red")
          (circle LIGHT-RADIUS "outline" "yellow")
          (circle LIGHT-RADIUS "solid" "green"))]
        [(string=? "yellow" string)
         (above
          (circle LIGHT-RADIUS "outline" "red")
          (circle LIGHT-RADIUS "solid" "yellow")
          (circle LIGHT-RADIUS "outline" "green"))]
        [(string=? "red" string)
         (above
          (circle LIGHT-RADIUS "solid" "red")
          (circle LIGHT-RADIUS "outline" "yellow")
          (circle LIGHT-RADIUS "outline" "green"))]
        [else 
         (above
          (circle LIGHT-RADIUS "outline" "red")
          (circle LIGHT-RADIUS "outline" "yellow")
          (circle LIGHT-RADIUS "outline" "green"))]))



(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)
 
 


(define-struct auto (x y dx image))
(define (fn-for-auto auto)
  (...
   (auto-x auto) 
   (auto-y auto) 
   (auto-dx auto) 
   (auto-image auto))) 









(check-expect(update-auto empty "" empty) empty)
(check-expect(update-auto (make-auto 1 2 3 empty) "x" 2) (make-auto 2 2 3 empty))
(check-expect(update-auto (make-auto 1 2 3 empty) "y" 4) (make-auto 1 4 3 empty))
(check-expect(update-auto (make-auto 1 2 3 empty) "dx" 6) (make-auto 1 2 6 empty))
(check-expect(update-auto (make-auto 1 2 3 empty) "" 6) (make-auto 1 2 3 empty))


(define (update-auto auto1 change new)
  (cond
    [(empty? auto1) empty]
    [(string=? "x" change)
     (make-auto new
                (auto-y auto1)
                (auto-dx auto1)
                (auto-image auto1))]
    [(string=? "y" change)
     (make-auto (auto-x auto1)
                new 
                (auto-dx auto1)
                (auto-image auto1))]
    [(string=? "dx" change)
     (make-auto (auto-x auto1)
                (auto-y auto1)
                new
                (auto-image auto1))]
    [else auto1]))
     





(define LOA empty)
 
 
               



(check-expect(drive (make-ws empty 0)) empty)
(check-expect(drive (make-ws(list (make-auto 0 10 1 empty)) 0)) (list (make-auto 1 10 1 empty)))
(check-expect(drive (make-ws (list(make-auto 0 10 4 empty) (make-auto WIDTH 50 1 empty)) (* 28 5)))
             (list (make-auto 3 10 4 empty)))
(check-expect(drive (make-ws (list(make-auto X-POS 10 4 empty) (make-auto WIDTH 50 1 empty)) (* 28 10)))
             (list (make-auto X-POS 10 4 empty)))
(check-expect(drive (make-ws (list(make-auto 600 10 4 empty) (make-auto WIDTH 50 1 empty)) (* 28 10)))
             (list (make-auto 604 10 4 empty)))
(check-expect(drive (make-ws (list(make-auto (- X-POS 50) 10 4 empty) (make-auto WIDTH 50 1 empty)) (* 28 10)))
             (list (make-auto (+ (- X-POS 50)2) 10 4 empty)))
(check-expect(drive (make-ws (list(make-auto 0 10 0 empty) (make-auto WIDTH 50 1 empty)) (* 28 3)))
             (list (make-auto 0 10 0 empty)))
(check-expect(drive (make-ws (list(make-auto 0 10 3 empty) (make-auto WIDTH 50 1 empty)) (* 28 3)))
             (list (make-auto 3 10 3 empty)))

(define (drive ws )
  (cond [(empty? (ws-LOA ws)) empty]
        [(empty? (first(ws-LOA ws))) (drive (make-ws(rest (ws-LOA ws))(ws-ticks ws)))]
        [else 
         (if (>= (auto-x (first (ws-LOA ws))) WIDTH )
             (drive (make-ws(rest (ws-LOA ws))(ws-ticks ws)))
             (cons (position (first (ws-LOA ws)) (ws-ticks ws))
                   (drive (make-ws(rest (ws-LOA ws))(ws-ticks ws)))))]))







(check-expect(position empty 0 ) empty)
(check-expect(position (make-auto 0 10 4 empty) (* 28 5)) (make-auto 3 10 4 empty))
(check-expect(position (make-auto 0 10 4 empty) (* 28 10)) (make-auto 2 10 4 empty))
(check-expect(position (make-auto 0 10 3 empty) (* 28 3)) (make-auto 3 10 3 empty))

(define (position auto1 ticks)
  (cond [(empty? auto1) empty]
        [(string=? (get-light ticks) "yellow")
         (update-auto auto1 "x"
                      (+ (auto-x auto1)
                         (* 3 (/ (auto-dx auto1) 4))))]             
        [(string=? (get-light ticks) "red")
         (if (and (<= (-(auto-x auto1) X-POS) 0) (>=(-(auto-x auto1) X-POS) (* -1 (auto-dx auto1))))
             (update-auto auto1 "x" X-POS)
             (if (< (auto-x auto1) X-POS)
                 (update-auto auto1 "x"
                               (+ (auto-x auto1) (/(auto-dx auto1)2) ))
                 (update-auto auto1 "x" (+ (auto-x auto1) (auto-dx auto1)))))]
        [else
         (update-auto auto1 "x" (+(auto-x auto1) (auto-dx auto1)))]))



(define-struct ws (LOA ticks))

 

(define START (make-ws (list empty) 0))

(define (main ws)
  (big-bang ws
    (on-tick action)
   (to-draw render-cars)))




(check-random (action (make-ws empty 0)) (make-ws (build-autolist (+ 1 (random 20)) (make-ws empty 0)) 1))
(check-random (action (make-ws (list (make-auto 0 0 2 AUTO-IMAGE2) empty) 0)) 
              (make-ws (build-autolist(+ 1 (random 20)) (make-ws (list (make-auto 2 0 2 AUTO-IMAGE2)) 1)) 1))
(define (action ws)
  (make-ws (drive (make-ws (build-autolist (random 20) ws) (ws-ticks ws))) (increase-tick (ws-ticks ws))))




(check-random (build-autolist 1 (make-ws LOA 0)) (cons (make-auto 0 (random HEIGHT) (+ 1(random 20)) (pick-image (random 3))) LOA))
(check-random (build-autolist 20 (make-ws LOA 0)) LOA)
(define (build-autolist num ws)
  (if 
   (= num 1) 
   (cons (make-auto 0 (random HEIGHT) (+ 1 (random 20)) (pick-image (random 3))) (ws-LOA ws))
   (ws-LOA ws)))



(check-expect  (render-cars (make-ws empty 0)) (render-light 0))
(check-expect  (render-cars (make-ws (list (make-auto 0 10 1 AUTO-IMAGE2))   0))
               (place-image AUTO-IMAGE2 0 10 (render-light 0)))
(check-expect (render-cars (make-ws (list (make-auto 0 10 1 AUTO-IMAGE2) empty)   0))
               (place-image AUTO-IMAGE2 0 10 (render-light 0)))

(define (render-cars ws)
        (cond [(empty? (ws-LOA ws))
               (render-light (ws-ticks ws))]
              [(empty? (first (ws-LOA ws))) (render-cars (make-ws (rest (ws-LOA ws)) (ws-ticks ws)))]
              [else (place-image
                    (auto-image (first (ws-LOA ws)))
                    (auto-x (first (ws-LOA ws)))
                    (auto-y (first (ws-LOA ws)))
                    (render-cars (make-ws (rest (ws-LOA ws)) (ws-ticks ws))))]))
 


               