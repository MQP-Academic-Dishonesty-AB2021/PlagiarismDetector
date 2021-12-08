

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname VenatArjunVieiraJulie_trafficlightFinal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


















(define TOTAL-LENGTH (+ GREEN-LENGTH
                        YELLOW-LENGTH
                        RED-LENGTH))
(define TEXT-SIZE 20)
(define TEXT-COLOR "black")
(define CREATE-GREEN-LIGHT
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "solid" "green")))
(define CREATE-YELLOW-LIGHT
  (above
   (circle LIGHT-RADIUS "outline" "red")
   (circle LIGHT-RADIUS "solid" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))
(define CREATE-RED-LIGHT
  (above
   (circle LIGHT-RADIUS "solid" "red")
   (circle LIGHT-RADIUS "outline" "yellow")
   (circle LIGHT-RADIUS "outline" "green")))
(define GR-YW (+ GREEN-LENGTH YELLOW-LENGTH))
(define TICKS-GREEN (* TICKS-SECOND GREEN-LENGTH))
(define TICKS-GR-YW (* TICKS-SECOND GR-YW))
(define TICKS-TOTAL (* TICKS-SECOND TOTAL-LENGTH))




(define-struct light (color ticks seconds length))





 
     



(define START-LIGHT (make-light "green" 0 0 GREEN-LENGTH)) 
(define GREEN-LIGHT (make-light "green" 26 0 GREEN-LENGTH))
(define YELLOW-LIGHT (make-light "yellow" (- TICKS-GR-YW 1) (- GR-YW 1) YELLOW-LENGTH))
(define RED-LIGHT (make-light "red" (- TICKS-TOTAL 1) (- TOTAL-LENGTH 1) RED-LENGTH))







(check-expect (update-light-ticks START-LIGHT)
              (make-light (light-color START-LIGHT)
                          (+ 1 (light-ticks START-LIGHT))
                          (light-seconds START-LIGHT)
                          (light-length START-LIGHT)))

(check-expect (update-light-ticks (make-light "green" 0 0 GREEN-LENGTH))
              (make-light "green" 1 0 GREEN-LENGTH))

(check-expect (update-light-ticks (make-light "yellow" 44 1 YELLOW-LENGTH))
              (make-light "yellow" 45 1 YELLOW-LENGTH))

(check-expect (update-light-ticks (make-light "red" 55 1 RED-LENGTH))
              (make-light "red" 56 2 RED-LENGTH))

(check-expect (update-light-ticks (make-light "green"
                                              (- (* TICKS-SECOND GREEN-LENGTH) 1)
                                              (- GREEN-LENGTH 1)
                                              GREEN-LENGTH))
              (make-light "green"
                          (* TICKS-SECOND GREEN-LENGTH)
                          GREEN-LENGTH
                          GREEN-LENGTH))

(check-expect (update-light-ticks (make-light "yellow"
                                              (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH))
                                              (+ YELLOW-LENGTH GREEN-LENGTH)
                                              YELLOW-LENGTH))
              (make-light "yellow"
                          (+ 1 (* TICKS-SECOND (+ YELLOW-LENGTH GREEN-LENGTH)))
                          (+ YELLOW-LENGTH GREEN-LENGTH)
                          YELLOW-LENGTH))

(check-expect (update-light-ticks (make-light "red"
                                              (- (* TICKS-SECOND TOTAL-LENGTH) 1)
                                              (- TOTAL-LENGTH 1)
                                              RED-LENGTH))
              (make-light "red"
                          (* TICKS-SECOND TOTAL-LENGTH)
                          TOTAL-LENGTH
                          RED-LENGTH))

(define (update-light-ticks light)
  (make-light (light-color light)
              (+ 1 (light-ticks light))
              (ticks->seconds (+ 1 (light-ticks light)))
              (light-length light))
  )










(define light-red (make-light "red" (+ TICKS-TOTAL 5) TOTAL-LENGTH RED-LENGTH))

(check-expect (display-timer-text (make-light "green" 14 0 GREEN-LENGTH))
              (text "0" TEXT-SIZE TEXT-COLOR))

(check-expect (display-timer-text (make-light "yellow" 81 3 YELLOW-LENGTH))
              (text "3" TEXT-SIZE TEXT-COLOR))

(check-expect (display-timer-text light-red)
              (text (number->string (light-seconds light-red)) TEXT-SIZE TEXT-COLOR))

(define (display-timer-text light)
  (text (number->string (light-seconds light)) TEXT-SIZE TEXT-COLOR))









(check-expect (display-light-timer (make-light "green" 14 0 2))
              (beside
               (above
                (circle LIGHT-RADIUS "outline" "red")
                (circle LIGHT-RADIUS "outline" "yellow")
                (circle LIGHT-RADIUS "solid" "green"))
               (text (number->string 0) TEXT-SIZE TEXT-COLOR)))

(check-expect (display-light-timer (make-light "yellow" 57 2 3))
              (beside
               CREATE-YELLOW-LIGHT
               (text (number->string 2) TEXT-SIZE TEXT-COLOR)))

(check-expect (display-light-timer (make-light "red" 150 5 5))
              (beside
               CREATE-RED-LIGHT
               (text (number->string 5) TEXT-SIZE TEXT-COLOR)))

(define (display-light-timer light)
  (beside
   (cond [(string=? (light-color light) "green")
          CREATE-GREEN-LIGHT
          ]
         [(string=? (light-color light) "yellow")
          CREATE-YELLOW-LIGHT
          ]
         [(string=? (light-color light) "red")
          CREATE-RED-LIGHT]
         )
   (display-timer-text light)))











(define light-yellow (make-light "yellow" TICKS-GR-YW GR-YW YELLOW-LENGTH))
(define light-green (make-light "green" 0 0 GREEN-LENGTH))
  
(check-expect (get-light light-green) light-green)
(check-expect (get-light (make-light "red" 0 0 GREEN-LENGTH)) light-green) 
(check-expect (get-light light-yellow) (make-light "red" (light-ticks light-yellow) (light-seconds light-yellow) RED-LENGTH))

(define (get-light light)
  (local [(define remainder (modulo (light-seconds light) TOTAL-LENGTH))]
    (cond [(< remainder GREEN-LENGTH)
           (make-light "green"
                       (light-ticks light)
                       (light-seconds light)
                       GREEN-LENGTH)] 
          [(< remainder (+ GREEN-LENGTH YELLOW-LENGTH))
           (make-light "yellow"
                       (light-ticks light)
                       (light-seconds light)
                       YELLOW-LENGTH)] 
          [else
           (make-light "red"
                       (light-ticks light)
                       (light-seconds light)
                       RED-LENGTH)] 
          )))








(check-expect (draw-light (make-light "green" 14 0 2))
              (beside
               (above
                (circle LIGHT-RADIUS "outline" "red")
                (circle LIGHT-RADIUS "outline" "yellow")
                (circle LIGHT-RADIUS "solid" "green"))
               (text (number->string 0) TEXT-SIZE TEXT-COLOR)))

(check-expect (draw-light (make-light "yellow"
                                      (+ (* TICKS-SECOND GREEN-LENGTH) 57)
                                      GREEN-LENGTH
                                      GREEN-LENGTH))
              (beside
               CREATE-YELLOW-LIGHT
               (text (number->string GREEN-LENGTH) TEXT-SIZE TEXT-COLOR)))

(check-expect (draw-light (make-light "yellow"
                                      TICKS-GR-YW
                                      GR-YW
                                      RED-LENGTH))
              (beside
               CREATE-RED-LIGHT
               (text (number->string GR-YW) TEXT-SIZE TEXT-COLOR)))

(check-expect (draw-light START-LIGHT)
              (beside
               CREATE-GREEN-LIGHT
               (text (number->string (light-seconds START-LIGHT)) TEXT-SIZE TEXT-COLOR)))

(define (draw-light light)
  (local [(define remainder (modulo (light-seconds light) TOTAL-LENGTH))]
    (cond [(< remainder GREEN-LENGTH)
           (display-light-timer (get-light light))] 
          [(< remainder (+ GREEN-LENGTH YELLOW-LENGTH))
           (display-light-timer (get-light light))] 
          [else
           (display-light-timer (get-light light))] 
          )))














(define BASE-AUTO-SPEED 15) 
(define TOP-AUTO-SPEED 10) 
(define START-LOA empty) 
(define YELLOW-MULT (/ 3 4))
(define RED-MULT (/ 1 2))
(define AUTO-WIDTH (image-width .))




(define-struct auto (initial-speed yellow-speed red-speed x y image))









 

(define AUTO1 (make-auto 20 (* 20 YELLOW-MULT) (* 20 RED-MULT) 40 600 AUTO-IMAGE1))
(define AUTO2 (make-auto 10 (* 10 YELLOW-MULT) (* 10 RED-MULT) 0 650 AUTO-IMAGE2))
(define AUTO3 (make-auto 25 (* 25 YELLOW-MULT) (* 25 RED-MULT) WIDTH 700 AUTO-IMAGE3))
(define AUTO4 (make-auto 15 (* 15 YELLOW-MULT) (* 15 RED-MULT) X-POS 625 AUTO-IMAGE1))








(define LOA1 empty)
(define LOA2 (cons (make-auto 20 (* 20 YELLOW-MULT) (* 20 RED-MULT) 60 500 AUTO-IMAGE1) empty))
(define LOA3 (cons (make-auto 20 (* 20 YELLOW-MULT) (* 20 RED-MULT) 60 500 AUTO-IMAGE1)
                   (cons (make-auto 15 (* 15 YELLOW-MULT) (* 15 RED-MULT) 30 550 AUTO-IMAGE2)
                         (cons (make-auto 5 (* 5 YELLOW-MULT) (* 5 RED-MULT) 20 525 AUTO-IMAGE3) empty))))
(define LOA4 (cons (make-auto 20 (* 20 YELLOW-MULT) (* 20 RED-MULT) WIDTH 500 AUTO-IMAGE1) empty))
(define LOA5 (cons (make-auto 20 (* 20 YELLOW-MULT) (* 20 RED-MULT) (+ 20 WIDTH) 500 AUTO-IMAGE1) empty))

 












(define (random-auto random-img)
  (make-auto (+ BASE-AUTO-SPEED (random TOP-AUTO-SPEED))
             (* YELLOW-MULT (+ BASE-AUTO-SPEED (random TOP-AUTO-SPEED)))
             (* RED-MULT (+ BASE-AUTO-SPEED (random TOP-AUTO-SPEED)))
             0
             (+ (* 0.5 HEIGHT) (random (* 0.4 HEIGHT)))
             (pick-image random-img)))






(check-expect (auto-at-red-light? AUTO1 GREEN-LIGHT) false)
(check-expect (auto-at-red-light? AUTO3 GREEN-LIGHT) false)
(check-expect (auto-at-red-light? AUTO4 YELLOW-LIGHT) false)
(check-expect (auto-at-red-light? AUTO3 RED-LIGHT) false)
(check-expect (auto-at-red-light? AUTO4 RED-LIGHT) true)
(define (auto-at-red-light? auto light)
  (and (string=? (light-color (get-light light)) "red")
       (>= (auto-x auto) X-POS)
       (<= (auto-x auto) (+ X-POS (/ AUTO-WIDTH 2)))))







(check-expect (after-or-green-light? AUTO1 GREEN-LIGHT) true)
(check-expect (after-or-green-light? AUTO3 GREEN-LIGHT) true)
(check-expect (after-or-green-light? AUTO3 YELLOW-LIGHT) true)
(check-expect (after-or-green-light? AUTO3 RED-LIGHT) true)
(define (after-or-green-light? auto light)
  (or (> (auto-x auto) X-POS)
      (string=? (light-color (get-light light)) "green")))







(check-expect (before-yellow-light? AUTO1 GREEN-LIGHT) false)
(check-expect (before-yellow-light? AUTO1 YELLOW-LIGHT) true)
(check-expect (before-yellow-light? AUTO3 YELLOW-LIGHT) false)
(check-expect (before-yellow-light? AUTO1 RED-LIGHT) false)
(define (before-yellow-light? auto light)
  (and (<= (auto-x auto) X-POS)
       (string=? (light-color (get-light light)) "yellow")))














(check-expect (move-auto AUTO1 GREEN-LIGHT)
              (make-auto (auto-initial-speed AUTO1)
                         (auto-yellow-speed AUTO1)
                         (auto-red-speed AUTO1)
                         (+ (auto-initial-speed AUTO1) (auto-x AUTO1))
                         (auto-y AUTO1)
                         (auto-image AUTO1)))
(check-expect (move-auto AUTO1 YELLOW-LIGHT)
              (make-auto (auto-initial-speed AUTO1)
                         (auto-yellow-speed AUTO1)
                         (auto-red-speed AUTO1)
                         (+ (auto-yellow-speed AUTO1) (auto-x AUTO1))
                         (auto-y AUTO1)
                         (auto-image AUTO1)))
(check-expect (move-auto AUTO1 RED-LIGHT)
              (make-auto (auto-initial-speed AUTO1)
                         (auto-yellow-speed AUTO1)
                         (auto-red-speed AUTO1)
                         (+ (auto-red-speed AUTO1) (auto-x AUTO1))
                         (auto-y AUTO1)
                         (auto-image AUTO1)))
(check-expect (move-auto AUTO4 RED-LIGHT)
              (make-auto (auto-initial-speed AUTO4)
                         (auto-yellow-speed AUTO4)
                         (auto-red-speed AUTO4)
                         X-POS
                         (auto-y AUTO4)
                         (auto-image AUTO4)))

(define (move-auto auto light)
  (cond
    [(auto-at-red-light? auto light)                           
     (make-auto (auto-initial-speed auto)
                (auto-yellow-speed auto)
                (auto-red-speed auto)
                X-POS                                                 
                (auto-y auto)
                (auto-image auto))]
    [(after-or-green-light? auto light)                        
     (make-auto (auto-initial-speed auto)
                (auto-yellow-speed auto)
                (auto-red-speed auto)
                (+ (auto-initial-speed auto) (auto-x auto))           
                (auto-y auto)
                (auto-image auto))]
    [(before-yellow-light? auto light)                         
     (make-auto (auto-initial-speed auto)
                (auto-yellow-speed auto)
                (auto-red-speed auto)
                (+ (auto-yellow-speed auto) (auto-x auto))            
                (auto-y auto)
                (auto-image auto))]
    [else                                                      
     (make-auto (auto-initial-speed auto)
                (auto-yellow-speed auto)
                (auto-red-speed auto)
                (+ (auto-red-speed auto) (auto-x auto))               
                (auto-y auto)
                (auto-image auto))]))






(check-expect (draw-all-autos LOA1) MTS)
(check-expect (draw-all-autos LOA2)
              (place-image (auto-image (first LOA2))
                           (auto-x (first LOA2))
                           (auto-y (first LOA2))
                           MTS))
(check-expect (draw-all-autos LOA3)
              (place-image
               (auto-image (first LOA3))
               (auto-x (first LOA3))
               (auto-y (first LOA3))
               (place-image
                (auto-image (first (rest LOA3)))
                (auto-x (first (rest LOA3)))
                (auto-y (first (rest LOA3)))
                (place-image
                 (auto-image (first (rest (rest LOA3))))
                 (auto-x (first (rest (rest LOA3))))
                 (auto-y (first (rest (rest LOA3))))
                 MTS))))
(define (draw-all-autos loa)
  (cond [(empty? loa) MTS]
        [else
         (place-image (auto-image (first loa))
                      (auto-x (first loa))
                      (auto-y (first loa))
                      (draw-all-autos(rest loa)))]))










(check-expect (greater-than-edge? LOA1) false)
(check-expect (greater-than-edge? LOA2) false)
(check-expect (greater-than-edge? LOA4) true)
(check-expect (greater-than-edge? LOA5) true)

(define (greater-than-edge? loa)
  (cond [(empty? loa) false]
        [(>= (auto-x (first loa)) WIDTH)
         true]
        [else
         false]))









(check-expect (move-all-autos LOA1 GREEN-LIGHT) empty)
(check-expect (move-all-autos LOA2 GREEN-LIGHT)
              (cons (make-auto 20 (* 20 YELLOW-MULT) (* 20 RED-MULT) (+ 60 20) 500 AUTO-IMAGE1) empty))
(check-expect (move-all-autos LOA3 YELLOW-LIGHT)
              (cons (make-auto 20 (* 20 YELLOW-MULT) (* 20 RED-MULT) (+ 60 (* 20 YELLOW-MULT)) 500 AUTO-IMAGE1)
                    (cons (make-auto 15 (* 15 YELLOW-MULT) (* 15 RED-MULT) (+ 30 (* 15 YELLOW-MULT)) 550 AUTO-IMAGE2)
                          (cons (make-auto 5 (* 5 YELLOW-MULT) (* 5 RED-MULT) (+ 20 (* 5 YELLOW-MULT)) 525 AUTO-IMAGE3) empty))))
(check-expect (move-all-autos LOA3 RED-LIGHT)
              (cons (make-auto 20 (* 20 YELLOW-MULT) (* 20 RED-MULT) (+ 60 (* 20 RED-MULT)) 500 AUTO-IMAGE1)
                    (cons (make-auto 15 (* 15 YELLOW-MULT) (* 15 RED-MULT) (+ 30 (* 15 RED-MULT)) 550 AUTO-IMAGE2)
                          (cons (make-auto 5 (* 5 YELLOW-MULT) (* 5 RED-MULT) (+ 20 (* 5 RED-MULT)) 525 AUTO-IMAGE3) empty))))
(define (move-all-autos loa light)
  (cond [(empty? loa) empty]
        [(greater-than-edge? loa)
         (move-all-autos (rest loa) light)]
        [else
         (cons (move-auto (first loa) light)
               (move-all-autos (rest loa) light))]))









(define (create-random-auto? loa)
  (cond [(= (random 20) 1)
         (cons (random-auto (random 3)) loa)]
        [else
         loa]))




(define-struct light-loa (light loa))



 

(define LIGHT-LOA1 (make-light-loa START-LIGHT LOA1))
(define LIGHT-LOA2 (make-light-loa START-LIGHT LOA2))
(define LIGHT-LOA3 (make-light-loa START-LIGHT LOA3))

(define START-LL (make-light-loa START-LIGHT START-LOA))



  



(check-expect (draw-light-and-autos LIGHT-LOA1)
              (overlay
               (beside
                (above
                 (circle LIGHT-RADIUS "outline" "red")
                 (circle LIGHT-RADIUS "outline" "yellow")
                 (circle LIGHT-RADIUS "solid" "green"))
                (text (number->string (light-seconds START-LIGHT)) TEXT-SIZE TEXT-COLOR))
               MTS))

(check-expect (draw-light-and-autos LIGHT-LOA2)
              (overlay
               (beside
                (above
                 (circle LIGHT-RADIUS "outline" "red")
                 (circle LIGHT-RADIUS "outline" "yellow")
                 (circle LIGHT-RADIUS "solid" "green"))
                (text (number->string (light-seconds START-LIGHT)) TEXT-SIZE TEXT-COLOR))
               (place-image (auto-image (first LOA2))
                            (auto-x (first LOA2))
                            (auto-y (first LOA2))
                            MTS)))

(check-expect (draw-light-and-autos LIGHT-LOA3)
              (overlay
               (beside
                CREATE-GREEN-LIGHT
                (text (number->string (light-seconds START-LIGHT)) TEXT-SIZE TEXT-COLOR))
               (place-image
                (auto-image (first LOA3))
                (auto-x (first LOA3))
                (auto-y (first LOA3))
                (place-image
                 (auto-image (first (rest LOA3)))
                 (auto-x (first (rest LOA3)))
                 (auto-y (first (rest LOA3)))
                 (place-image
                  (auto-image (first (rest (rest LOA3))))
                  (auto-x (first (rest (rest LOA3))))
                  (auto-y (first (rest (rest LOA3))))
                  MTS)))))

(define (draw-light-and-autos light-loa)
  (overlay
   (draw-light (light-loa-light light-loa))
   (draw-all-autos (light-loa-loa light-loa))))






(define (update-light-and-autos light-loa)
  (make-light-loa (update-light-ticks (light-loa-light light-loa))
                  (move-all-autos (create-random-auto? (light-loa-loa light-loa))
                                  (light-loa-light light-loa))))












(define TEXT-SIZE-BILBOARD 20)
(define TEXT-COLOR-BILBOARD "black")
(define X-BILBOARD 100)
(define Y-BILBOARD 100)




(define-struct LLLoB(light-loa lob))




(define START (make-LLLoB START-LL (cons "" empty)))






(define (add-string string1 string2)
  (string-append string1 string2))







(define (get-backspace bilboard)
  (cond [(= (string-length bilboard) 0)
          ""]
        [else
          (substring bilboard
                     0
                     (- (string-length bilboard) 1))]))







(define no-show-chars
  (list "start"
        "cancel"
        "clear"
        "rshift"
        "lshift"
        "shift"
        "control"
        "rcontrol"
        "pause"
        "capital"
        "prior"
        "next"
        "end"
        "home"
        "escape"
        "select"
        "print"
        "snapshot"
        "execute"
        "insert"
        "help"
        "right"
        "left"))







(define (in-no-show ke no-show-list)
  (cond [(empty? no-show-list) false]
        [(string=? ke (first no-show-list)) true]
        [else
         (in-no-show ke (rest no-show-list))]))












(define (handle-key-bilboard bilboard ke)
  (cond [(key=? ke "\r")
          (add-string bilboard "\n")]
        [(key=? ke "\t")
          ""]
        [(key=? ke "\b")
         (get-backspace bilboard)]
        [(in-no-show ke no-show-chars)
          bilboard]
        [else
          (add-string bilboard ke)]))













(define (remove-first-lob LLLoB)
  (cond [(empty? (LLLoB-lob LLLoB))
         (make-LLLoB
          (LLLoB-light-loa LLLoB)
          (cons "" empty))]
        [(= 1 (length (LLLoB-lob LLLoB)))
         (make-LLLoB
          (LLLoB-light-loa LLLoB)
          (cons "" empty))]
        [else
         (make-LLLoB
          (LLLoB-light-loa LLLoB)
          (rest (LLLoB-lob LLLoB)))]))















(define (handle-ke-LLLoB LLLoB ke)
  (cond [(key=? ke "down")
         (make-LLLoB
          (LLLoB-light-loa LLLoB)
          (append (cons "" empty)
                 (LLLoB-lob LLLoB)))]
        [(key=? ke "up")
         (remove-first-lob LLLoB)]
        [else
         (make-LLLoB
          (LLLoB-light-loa LLLoB)
          (cons
           (handle-key-bilboard (first (LLLoB-lob LLLoB)) ke)
           (rest (LLLoB-lob LLLoB))))]))
         






(define (update-LLLoB LLLoB)
  (make-LLLoB
   (update-light-and-autos (LLLoB-light-loa LLLoB))
   (LLLoB-lob LLLoB)))








(define (draw-LLLoB LLLoB)
  (place-image
   (text (first (LLLoB-lob LLLoB)) TEXT-SIZE-BILBOARD TEXT-COLOR-BILBOARD)
   X-BILBOARD
   Y-BILBOARD
   (draw-light-and-autos (LLLoB-light-loa LLLoB))))




(define (main LLLoB)
  (big-bang LLLoB
    (on-tick update-LLLoB)
    (to-draw draw-LLLoB)
    (on-key handle-ke-LLLoB)))