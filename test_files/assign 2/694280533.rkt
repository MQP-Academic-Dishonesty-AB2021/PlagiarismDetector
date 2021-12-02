

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Assignment #2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)




(define HEIGHT-COW 600)
(define WIDTH-COW 800)
(define COW .)

(define MTS-COW (empty-scene WIDTH-COW HEIGHT-COW))









(define-struct ws-cow (xpos ypos speed laps rotate))








(define START-COW (make-ws-cow 0 (/ HEIGHT-COW 2) 1 0 1))
(define ws-cow1 (make-ws-cow 0 (/ HEIGHT-COW 2) 1 0 1))
(define ws-cow2 (make-ws-cow 50 0 50 0 1))
(define ws-cow3 (make-ws-cow (- WIDTH-COW 5) 500 10 0 1))
(define ws-cow4 (make-ws-cow WIDTH-COW 70 40 0 1))
(define ws-cow5 (make-ws-cow 0 70 0 0 1))





(define (main-cow ws-cow)
  (big-bang ws-cow
    (on-tick update-ws-cow)
    (on-key key-speed)
    (on-mouse mouse-cow)
    (to-draw render-ws-cow)
    )
  )



(check-expect (update-ws-cow ws-cow1) (make-ws-cow (+ (ws-cow-speed ws-cow1)
                                          (ws-cow-xpos ws-cow1))
                                       (ws-cow-ypos ws-cow1)
                                       (ws-cow-speed ws-cow1)
                                       (ws-cow-laps ws-cow1)
                                       (- (ws-cow-rotate ws-cow1))))

(check-expect (update-ws-cow ws-cow4) (make-ws-cow 0
                                       (ws-cow-ypos ws-cow4)
                                       (ws-cow-speed ws-cow4)
                                       (+ 1 (ws-cow-laps ws-cow4))
                                       (- (ws-cow-rotate ws-cow4))))
                               
                               

(define (update-ws-cow ws-cow)
  (if (>= (+ (ws-cow-speed ws-cow) (ws-cow-xpos ws-cow)) WIDTH-COW)
      (loop-cow ws-cow)
      (make-ws-cow (+ (ws-cow-speed ws-cow) (ws-cow-xpos ws-cow))
               (ws-cow-ypos ws-cow) (ws-cow-speed ws-cow)
               (ws-cow-laps ws-cow)
               (- (ws-cow-rotate ws-cow)))
      ))




(check-expect (loop-cow ws-cow4) (make-ws-cow 0
                                      (ws-cow-ypos ws-cow4)
                                      (ws-cow-speed ws-cow4)
                                      (+ 1 (ws-cow-laps ws-cow4))
                                      (- (ws-cow-rotate ws-cow4))))
(check-expect (loop-cow ws-cow3) (make-ws-cow 0
                                      (ws-cow-ypos ws-cow3)
                                      (ws-cow-speed ws-cow3)
                                      (+ 1 (ws-cow-laps ws-cow3))
                                      (- (ws-cow-rotate ws-cow3))))

(define (loop-cow ws-cow)
  (make-ws-cow 0 (ws-cow-ypos ws-cow) (ws-cow-speed ws-cow) (+ 1 (ws-cow-laps ws-cow)) (- (ws-cow-rotate ws-cow))
           )
  )
 
  


(check-expect (mouse-cow ws-cow1 50 200 "button-down") (make-ws-cow 50
                                                            200
                                                            (ws-cow-speed ws-cow1)
                                                            (ws-cow-laps ws-cow1)
                                                            (ws-cow-rotate ws-cow1)))
(check-expect (mouse-cow ws-cow2 400 100 "button-down") (make-ws-cow 400
                                                             100
                                                             (ws-cow-speed ws-cow2)
                                                             (ws-cow-laps ws-cow2)
                                                             (ws-cow-rotate ws-cow2)))
(check-expect (mouse-cow ws-cow3 0 0 "button-up") (make-ws-cow (ws-cow-xpos ws-cow3)
                                                       (ws-cow-ypos ws-cow3)
                                                       (ws-cow-speed ws-cow3)
                                                       (ws-cow-laps ws-cow3)
                                                       (ws-cow-rotate ws-cow3)))


(define (mouse-cow ws-cow x y mouse)
  (cond
    [(mouse=? mouse "button-down")
     (make-ws-cow x y (ws-cow-speed ws-cow) (ws-cow-laps ws-cow) (ws-cow-rotate ws-cow))]
    
    [else ws-cow]
    )
  )




(check-expect (key-speed ws-cow1 "a") (make-ws-cow (ws-cow-xpos ws-cow1)
                                           (ws-cow-ypos ws-cow1)
                                           (- (ws-cow-speed ws-cow1) 1)
                                           (ws-cow-laps ws-cow1)
                                           (ws-cow-rotate ws-cow1)))
(check-expect (key-speed ws-cow1 "s") (make-ws-cow (ws-cow-xpos ws-cow1)
                                           (ws-cow-ypos ws-cow1)
                                           (+ (ws-cow-speed ws-cow1) 1)
                                           (ws-cow-laps ws-cow1)
                                           (ws-cow-rotate ws-cow1)))
(check-expect (key-speed ws-cow5 "a") (make-ws-cow (ws-cow-xpos ws-cow5)
                                           (ws-cow-ypos ws-cow5)
                                           (ws-cow-speed ws-cow5)
                                           (ws-cow-laps ws-cow5)
                                           (ws-cow-rotate ws-cow5)))
(check-expect (key-speed ws-cow2 "x") ws-cow2)


(define (key-speed ws-cow key)
  (cond
    [(key=? "s" key)
     (make-ws-cow (ws-cow-xpos ws-cow)
              (ws-cow-ypos ws-cow)
              (+ 1 (ws-cow-speed ws-cow))
              (ws-cow-laps ws-cow)
              (ws-cow-rotate ws-cow))]
    [(and (> (ws-cow-speed ws-cow) 0) (key=? "a" key))
     (make-ws-cow (ws-cow-xpos ws-cow)
              (ws-cow-ypos ws-cow)
              (- (ws-cow-speed ws-cow) 1)
              (ws-cow-laps ws-cow)
              (ws-cow-rotate ws-cow))]
    [else ws-cow]
    )
  )
    
    
     



(check-expect (render-ws-cow ws-cow1)
              (place-image (rotate-cow ws-cow1) (ws-cow-xpos ws-cow1) (ws-cow-ypos ws-cow1) (render-text ws-cow1))
              )
  (check-expect (render-ws-cow ws-cow5)
              (place-image (rotate-cow ws-cow5) (ws-cow-xpos ws-cow5) (ws-cow-ypos ws-cow5) (render-text ws-cow5))
              )

(define (render-ws-cow ws-cow)
  (place-image (rotate-cow ws-cow) (ws-cow-xpos ws-cow) (ws-cow-ypos ws-cow) (render-text ws-cow))
  )



(check-expect (rotate-cow ws-cow1) (rotate (ws-cow-rotate ws-cow1) COW))
(check-expect (rotate-cow ws-cow5) COW)

(define (rotate-cow ws-cow)
  (if (= (ws-cow-speed ws-cow) 0)
      COW
      (rotate (ws-cow-rotate ws-cow) COW)
      )
  )



(check-expect (render-text ws-cow1)
              (place-image (text
                            (string-append "Distance: "
                                           (number->string (ws-cow-laps ws-cow1))
                                           " laps\n"
                                           "Speed: "
                                           (number->string (ws-cow-speed ws-cow1))
                                           " pixels/tick") 20 "red")
                           100 50 MTS-COW))
(check-expect (render-text ws-cow2)
              (place-image (text
                            (string-append "Distance: "
                                           (number->string (ws-cow-laps ws-cow2))
                                           " laps\n"
                                           "Speed: "
                                           (number->string (ws-cow-speed ws-cow2))
                                           " pixels/tick") 20 "red")
                           100 50 MTS-COW))
 

(define (render-text ws-cow)
  (place-image (text (string-append "Distance: "
                                    (number->string (ws-cow-laps ws-cow))
                                    " laps\n" "Speed: "
                                    (number->string (ws-cow-speed ws-cow))
                                    " pixels/tick") 20 "red") 100 50 MTS-COW)
  )




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
(define TOTAL-LENGTH (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))


(define BULBS
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))







(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)
(define MAX-SPEED 25)
(define START-X 0)
(define MAX-Y (/ HEIGHT 2))




(define-struct auto (xpos ypos speed image))

 
(define AUTO1 (make-auto 0 (+ (/ HEIGHT 2) 10) 3 AUTO-IMAGE1))
(define AUTO2 (make-auto 50 (/ HEIGHT 2) 4 AUTO-IMAGE2))
(define AUTO3 (make-auto X-POS (- HEIGHT 10) 5 AUTO-IMAGE3))
(define AUTO4 (make-auto (+ X-POS 10) (- HEIGHT 15) 10 AUTO-IMAGE3))
(define AUTO5 (make-auto WIDTH (- (/ HEIGHT 2) 5) 15 AUTO-IMAGE2)) 
(define AUTO6 (make-auto (- X-POS 1) (/ HEIGHT 4) 10 AUTO-IMAGE1))




(define AUTOS1 (list AUTO1 AUTO2 AUTO3))
(define AUTOS2 (list AUTO3 AUTO2 AUTO1))
(define AUTOS3 empty)
(define AUTOS4 (list AUTO4 AUTO5 AUTO1))

 





(define STRINGS1 empty)
(define STRINGS2 (list "Hi! Hello!"))
(define STRINGS3 (list "Hi! Hello!" "CS 1102" "CS 2103"))

 






(define-struct ws (time autos strings))

 

(define START (make-ws 0 empty empty))
(define WS1 (make-ws 0 AUTOS2 STRINGS2))
(define WS2 (make-ws 500 AUTOS3 STRINGS3))
(define WS3 (make-ws 2000 AUTOS4 STRINGS1))









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
    (on-tick update-ws (/ 1 TICKS-SECOND))
    (to-draw render-ws)
    (on-key key-handler)
    )
  )






(define (update-ws ws)
  (make-ws (tick (ws-time ws)) (update-autos (ws-autos ws)
                                             (seconds-in-cycle (ws-time ws)))
           (ws-strings ws)))




(check-expect (tick 100) 101)
(check-expect (tick 0) 1)
(check-expect (tick 1000) 1001)

(define (tick time)
  (+ time 1))







(define (update-autos autos time)
  (if (= (random 20) 1)
      (cons (random-auto (random 10000)) (move-autos autos time))
      (move-autos autos time)))





(check-expect (random-auto 300) (make-auto START-X
                                           (- HEIGHT (modulo 300 MAX-Y))
                                           (+ 1 (modulo 300 MAX-SPEED))
                                           (pick-image (modulo 300 3))))
(check-expect (random-auto 1) (make-auto START-X
                                         (- HEIGHT (modulo 1 MAX-Y))
                                         (+ 1 (modulo 1 MAX-SPEED))
                                         (pick-image (modulo 100 3))))

(define (random-auto number) (make-auto START-X
                                        (- HEIGHT (modulo number MAX-Y))
                                        (+ 1 (modulo number MAX-SPEED))
                                        (pick-image (modulo number 3))))






(check-expect (move-autos empty 0) empty)
(check-expect (move-autos (list AUTO1 AUTO2 AUTO3) GREEN-LENGTH)
              (list (move-auto AUTO1 GREEN-LENGTH)
                    (move-auto AUTO2 GREEN-LENGTH)
                    (move-auto AUTO3 GREEN-LENGTH)))
(check-expect (move-autos (list AUTO3 AUTO4 AUTO5)
                          (+ GREEN-LENGTH YELLOW-LENGTH))
              (list
               (move-auto AUTO3 (+ GREEN-LENGTH YELLOW-LENGTH))
               (move-auto AUTO4 (+ GREEN-LENGTH YELLOW-LENGTH))))

(define (move-autos autos time)
  (cond [(empty? autos) empty]
        [else
         (if (auto? (move-auto (first autos) time))
             (cons (move-auto (first autos) time)
                   (move-autos (rest autos) time))
             (move-autos (rest autos) time))]))







(check-expect (move-auto AUTO1 0) (make-auto (+ (auto-xpos AUTO1) (get-speed
                                                                   AUTO1 0))
                                             (auto-ypos AUTO1)
                                             (auto-speed AUTO1)
                                             (auto-image AUTO1)))
(check-expect (move-auto AUTO2 GREEN-LENGTH) (make-auto (+
                                                         (auto-xpos AUTO2)
                                                         (get-speed
                                                          AUTO2
                                                          GREEN-LENGTH))
                                                        (auto-ypos AUTO2)
                                                        (auto-speed AUTO2)
                                                        (auto-image AUTO2)))
(check-expect (move-auto AUTO5 (+ GREEN-LENGTH YELLOW-LENGTH)) false)

(define (move-auto auto time)
  (if ( >= (+ (auto-xpos auto) (get-speed auto time)) WIDTH)
      false
      (make-auto (+ (auto-xpos auto) (get-speed auto time))
                 (auto-ypos auto)
                 (auto-speed auto)
                 (auto-image auto))))







(check-expect (get-speed AUTO1 0) (auto-speed AUTO1))
(check-expect (get-speed AUTO2 (+ GREEN-LENGTH YELLOW-LENGTH))
              (/ (auto-speed AUTO2) 2))
(check-expect (get-speed AUTO3 GREEN-LENGTH)
              (/ (* 3 (auto-speed AUTO3)) 4))
(check-expect (get-speed AUTO3 (+ GREEN-LENGTH YELLOW-LENGTH)) 0)
(check-expect (get-speed AUTO4 0) (auto-speed AUTO4))
(check-expect (get-speed AUTO4 GREEN-LENGTH) (auto-speed AUTO4))
(check-expect (get-speed AUTO4 (+ GREEN-LENGTH YELLOW-LENGTH))
              (auto-speed AUTO4))
(check-expect (get-speed AUTO6 (+ GREEN-LENGTH YELLOW-LENGTH))
              (- X-POS (auto-xpos AUTO6)))

(define (get-speed auto time)
  (cond [(light-color? time "green") (auto-speed auto)]
        [(light-color? time "yellow")
         (if (> (auto-xpos auto) X-POS)
             (auto-speed auto)
             (/ (* 3 (auto-speed auto)) 4))]
        [(light-color? time "red")
         (cond [(> (auto-xpos auto) X-POS) (auto-speed auto)]
               [(= (auto-xpos auto) X-POS) 0]
               [(> (+ (auto-xpos auto) (/ (auto-speed auto) 2)) X-POS)
                (- X-POS (auto-xpos auto))]
               [else (/ (auto-speed auto) 2)])]))





(check-expect (render-ws WS1)
              (render-autos (ws-autos WS1)
                            (render-time (ticks->seconds (ws-time WS1))
                                         (render-lights (seconds-in-cycle
                                                         (ws-time WS1))
                                                        (render-billboard
                                                         (ws-strings WS1)
                                                         MTS)))))
(check-expect (render-ws WS3)
              (render-autos (ws-autos WS3)
                            (render-time (ticks->seconds (ws-time WS3))
                                         (render-lights (seconds-in-cycle
                                                         (ws-time WS3))
                                                        (render-billboard
                                                         (ws-strings WS3)
                                                         MTS)))))

(define (render-ws ws)
  (render-autos (ws-autos ws) (render-time (ticks->seconds (ws-time ws))
                                           (render-lights (seconds-in-cycle
                                                           (ws-time ws))
                                                          (render-billboard
                                                           (ws-strings ws)
                                                           MTS)))))





(check-expect (render-autos empty (square 1000 "solid" "red"))
              (square 1000 "solid" "red"))
(check-expect (render-autos AUTOS1 MTS) (render-auto
                                         AUTO1
                                         (render-auto AUTO2
                                                      (render-auto
                                                       AUTO3
                                                       MTS))))
(check-expect (render-autos AUTOS4 MTS) (render-auto
                                         AUTO4
                                         (render-auto AUTO5
                                                      (render-auto
                                                       AUTO1
                                                       MTS))))

(define (render-autos autos scene)
  (cond [(empty? autos) scene]
        [else
         (render-auto (first autos)
                      (render-autos (rest autos) scene))]))




(check-expect (render-auto AUTO1 MTS) (place-image (auto-image AUTO1)
                                                   (auto-xpos AUTO1)
                                                   (auto-ypos AUTO1)
                                                   MTS))
(check-expect (render-auto AUTO5
                           (square 1000 "solid" "blue"))
              (place-image (auto-image AUTO5)
                           (auto-xpos AUTO5)
                           (auto-ypos AUTO5)
                           (square 1000 "solid" "blue")))
              
(define (render-auto auto scene)
  (place-image (auto-image auto) (auto-xpos auto) (auto-ypos auto) scene))





(check-expect (draw-lights 0) (above
                               (light-fill 0 "red")
                               (light-fill 0 "yellow")
                               (light-fill 0 "green")
                               )
              )
(check-expect (draw-lights GREEN-LENGTH) (above
                                          (light-fill GREEN-LENGTH "red")
                                          (light-fill GREEN-LENGTH "yellow")
                                          (light-fill GREEN-LENGTH "green")
                                          )
              )

(define (draw-lights time)
  (above
   (light-fill time "red")
   (light-fill time "yellow")
   (light-fill time "green")
   )
  )
      



(check-expect (seconds-in-cycle 5000) (modulo (ticks->seconds 5000)
                                              TOTAL-LENGTH))
(check-expect (seconds-in-cycle 0) (modulo (ticks->seconds 0) TOTAL-LENGTH))

(define (seconds-in-cycle ticks)
  (modulo (ticks->seconds ticks) TOTAL-LENGTH)
  )




(check-expect (light-fill 0 "green") (circle LIGHT-RADIUS "solid" "green"))
(check-expect (light-fill GREEN-LENGTH "green")
              (circle LIGHT-RADIUS "outline" "green"))
(check-expect (light-fill GREEN-LENGTH "yellow")
              (circle LIGHT-RADIUS "solid" "yellow"))
(check-expect (light-fill (+ GREEN-LENGTH YELLOW-LENGTH) "yellow")
              (circle LIGHT-RADIUS "outline" "yellow"))
(check-expect (light-fill (+ GREEN-LENGTH YELLOW-LENGTH) "red")
              (circle LIGHT-RADIUS "solid" "red"))
(check-expect (light-fill 0 "red")
              (circle LIGHT-RADIUS "outline" "red"))

(define (light-fill time color)
  (circle LIGHT-RADIUS (if (light-color? time color)
                           "solid"
                           "outline")
          color)
  )




(check-expect (light-color? 0 "green") true)
(check-expect (light-color? GREEN-LENGTH "green") false)
(check-expect (light-color? GREEN-LENGTH "yellow") true)
(check-expect (light-color? (+ GREEN-LENGTH YELLOW-LENGTH) "yellow") false)
(check-expect (light-color? (+ GREEN-LENGTH YELLOW-LENGTH) "red") true)
(check-expect (light-color? 0 "red") false)

(define (light-color? time color)
  (cond
    [(string=? color "green") (< time GREEN-LENGTH)]
    [(string=? color "yellow") (and (< time (+ YELLOW-LENGTH GREEN-LENGTH))
                                    (>= time GREEN-LENGTH))]
    [(string=? color "red") (>= time (+ GREEN-LENGTH YELLOW-LENGTH))]
    )
  )





(check-expect (render-time 0 MTS) (place-image (text (number->string 0) 20 "black")
                                               (+ X-POS LIGHT-RADIUS
                                                  LIGHT-RADIUS) Y-POS MTS))
(check-expect (render-time 100 (square 1000 "solid" "blue"))
              (place-image (text
                            (number->string 100) 20 "black")
                           (+ X-POS LIGHT-RADIUS LIGHT-RADIUS)
                           Y-POS (square 1000 "solid" "blue")))

(define (render-time time scene)
  (place-image (text (number->string time) 20 "black") (+ X-POS
                                                          LIGHT-RADIUS
                                                          LIGHT-RADIUS)
               Y-POS scene)
  )




(check-expect (render-lights 0 MTS) (place-image (draw-lights 0)
                                                 X-POS
                                                 Y-POS
                                                 MTS))
(check-expect (render-lights YELLOW-LENGTH (square 1000 "solid" "blue"))
              (place-image (draw-lights YELLOW-LENGTH) X-POS Y-POS
                           (square 1000 "solid" "blue")))

(define (render-lights time scene)
  (place-image (draw-lights time)
               X-POS
               Y-POS
               scene))








(check-expect (key-handler WS1 "a") (make-ws (ws-time WS1) (ws-autos WS1)
                                             (cons (modify-string
                                                    (first (ws-strings WS1))
                                                    "a")
                                                   (rest (ws-strings WS1)))))
(check-expect (key-handler WS2 "next") (make-ws (ws-time WS2) (ws-autos WS2)
                                                (cons "" (ws-strings WS2))))
(check-expect (key-handler WS2 "prior") (make-ws (ws-time WS2) (ws-autos WS2)
                                                 (rest (ws-strings WS2))))
(check-expect (key-handler (make-ws 0 empty empty) "prior") (make-ws 0 empty empty))
(check-expect (key-handler (make-ws 0 empty empty) "b") (make-ws 0 empty (list "b")))
(check-expect (key-handler WS3 "pause") WS3)


(define (key-handler ws key)
  (make-ws (ws-time ws) (ws-autos ws)
           (cond [(= (string-length key) 1) (if (empty? (ws-strings ws))
                                                (cons
                                                 (modify-string "" key) empty)
                                                (cons
                                                 (modify-string
                                                  (first (ws-strings ws)) key)
                                                 (rest (ws-strings ws))))]
                 [(key=? key "next") (cons "" (ws-strings ws))]
                 [(key=? key "prior") (if (empty? (ws-strings ws))
                                          empty
                                          (rest (ws-strings ws)))]
                 [else (ws-strings ws)])))





(check-expect (modify-string "CS 1101" " ") "CS 1101 ")
(check-expect (modify-string "Hi!" " ") "Hi! ")
(check-expect (modify-string "Bye!" "\r") "Bye!\n")
(check-expect (modify-string "CS 1102" "\t") "")
(check-expect (modify-string "MA 1023" "\b") "MA 102")
(check-expect (modify-string "" "\b") "")

(define (modify-string string key)
  (cond [(key=? key "\r") (string-append string "\n")]
        [(key=? key "\t") ""]
        [(key=? key "\b") (if (string=? string "")
                              ""
                              (substring string 0
                                         (- (string-length string) 1)))]
        [else (string-append string key)]))





(check-expect (render-billboard empty MTS) MTS)
(check-expect (render-billboard (list "Hi!") MTS) (place-image
                                                   (text "Hi!" 20 "black")
                                                   (/ X-POS 2) Y-POS MTS))
(check-expect (render-billboard (list "Bye!" "HI!")
                                (square 1000 "solid" "green"))
              (place-image (text "Bye!" 20 "black")
                           (/ X-POS 2) Y-POS
                           (square 1000 "solid" "green")))

(define (render-billboard texts scene)
  (if (empty? texts)
      scene
      (place-image (text (first texts) 20 "black")
                   (/ X-POS 2) Y-POS
                   scene)))