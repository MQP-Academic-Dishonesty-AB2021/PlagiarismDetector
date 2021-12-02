

#reader(lib "htdp-advanced-reader.ss" "lang")((modname |assignment 2 cow|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define BLANK (square 0 "solid" "white"))
(define COW .)
(define ANGLE 1) 


(define MTS (empty-scene WIDTH HEIGHT))

(define (main ws)
  (big-bang ws
    (on-tick tick)
    (to-draw draw)
    (on-mouse click)
    (on-key key)))




(define-struct Img (image x y))             






(define (fn-for-Img Img)
  (... (Img-image Img)
       (Img-image x)
       (Img-image y)))

(define IMG0 (make-Img BLANK 0 0))
(define IMG1 (make-Img (text "one" 20 "black") 0 0))
(define IMG2 (make-Img (text "one" 20 "black") 50 50))
(define IMG3 (make-Img (square 40 "solid" "white") 100 100))






(define LOI0 (list IMG0))
(define LOI1 empty)
(define LOI2 (list IMG1))
(define LOI3 (list IMG1 IMG2 IMG3))
  

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-Img (first loi))
                   (fn-for-loi (rest loi)))]))









(define-struct Cow (Speed Posx Posy))


(define COW0 (make-Cow 0 0 0))                        


(define COW1 (make-Cow 1 0 0))


(define COW2 (make-Cow 10 (/ WIDTH 2) (/ HEIGHT 2)))


(define COW3 (make-Cow 1 WIDTH (/ HEIGHT 2)))


(define COW4 (make-Cow 200 0 (/ HEIGHT 2)))          

(define (fn-for-Cow cow)
  (... (Cow-Speed cow)
       (Cow-Posx cow)
       (Cow-Posy cow)))







(define-struct ws (Cow Laps Ticks))
(define WS0 (make-ws COW0 0 1))
(define WS1 (make-ws COW1 0 1))
(define WS2 (make-ws COW2 1 2))
(define WS3 (make-ws COW3 0 3))
(define WS4 (make-ws COW4 0 4))

(define START WS3)

(define (fn-for-ws ws)
  (... (fn-for-Cow (ws-Cow ws))
       (ws-Laps ws)
       (ws-Ticks ws)))





(define (tick ws)
  (make-ws
   (move-cow (ws-Cow ws))
   (update-laps ws)
   (+ (ws-Ticks ws) 1)
   ))



(check-expect
 (draw WS1)
 (place-image
  (get-cow WS1)
  (Cow-Posx (ws-Cow WS1))
  (Cow-Posy (ws-Cow WS1))
  (place-image 
   (above/align
    "left"
    (beside
     (text "Distance: " 12 "red")
     (text (number->string (ws-Laps WS1))
           12
           "red")
     (text " " 12 "red")
     (text "laps" 12 "red"))
    (beside
     (text "Speed: " 12 "red")
     (text (number->string (Cow-Speed (ws-Cow WS1)))
           12
           "red")
     (text " " 12 "red")
     (text "pixels/tick" 12 "red"))) 60 50 MTS)))

(check-expect
 (draw WS2)
 (place-image
  (get-cow WS2)
  (Cow-Posx (ws-Cow WS2))
  (Cow-Posy (ws-Cow WS2))
  (place-image 
   (above/align
    "left"
    (beside
     (text "Distance: " 12 "red")
     (text (number->string (ws-Laps WS2))
           12
           "red")
     (text " " 12 "red")
     (text "laps" 12 "red"))
    (beside
     (text "Speed: " 12 "red")
     (text (number->string (Cow-Speed (ws-Cow WS2)))
           12
           "red")
     (text " " 12 "red")
     (text "pixels/tick" 12 "red"))) 60 50 MTS)))







(define (draw ws)
  (local
    [(define INFO
       (make-Img
        (above/align
         "left"
         (beside
          (text "Distance: " 12 "red")
          (text (number->string (ws-Laps ws))
                12
                "red")
          (text " " 12 "red")
          (text "laps" 12 "red"))
         (beside
          (text "Speed: " 12 "red")
          (text (number->string
                 (Cow-Speed (ws-Cow ws)))
                12
                "red")
          (text " " 12 "red")
          (text "pixels/tick" 12 "red"))) 60 50))
     (define COW (make-Img (get-cow ws)
                           (Cow-Posx (ws-Cow ws))
                           (Cow-Posy (ws-Cow ws))))]
    
    (draw-images (list COW INFO))))



(check-expect (draw-images LOI1) MTS)
(check-expect (draw-images LOI2)
              (place-image (Img-image (first LOI2))
                           (Img-x (first LOI2))
                           (Img-y (first LOI2))
                           MTS))

(check-expect (draw-images LOI3)
              (place-image
               (Img-image (first LOI3))
               (Img-x (first LOI3))
               (Img-y (first LOI3))
               (place-image (Img-image (first (rest LOI3)))
                            (Img-x (first (rest LOI3)))
                            (Img-y (first (rest LOI3)))
                            MTS)))
                                              





(define (draw-images loi)
  (cond [(empty? loi) MTS]
        [else (place-image (Img-image (first loi))
                           (Img-x (first loi))
                           (Img-y (first loi))
                           (draw-images (rest loi)))]))


  





(check-expect (get-cow WS1) (rotate ANGLE COW))
(check-expect (get-cow WS2) (rotate (* ANGLE -1) COW))

(define (get-cow ws)
  (if (and (odd? (ws-Ticks ws))
           (not (= (Cow-Speed (ws-Cow ws)) 0)))
      (rotate ANGLE COW)
      (rotate (* ANGLE -1) COW)))







(check-expect (key WS1 "s")
              (make-ws
               (make-Cow
                (+ (Cow-Speed (ws-Cow WS1)) 1)
                (Cow-Posx (ws-Cow WS1))
                (Cow-Posy (ws-Cow WS1)))
               (ws-Laps WS1)
               (ws-Ticks WS1)))

(check-expect (key WS0 "a")
              WS0)

(check-expect (key WS1 " ")
              WS1)

(check-expect (key WS1 "a")
              (make-ws
               (make-Cow
                (- (Cow-Speed (ws-Cow WS1)) 1)
                (Cow-Posx (ws-Cow WS1))
                (Cow-Posy (ws-Cow WS1)))
               (ws-Laps WS1)
               (ws-Ticks WS1)))

(define (key ws ke)
  (cond [(key=? ke "s")
         (make-ws (make-Cow
                   (+ (Cow-Speed (ws-Cow ws)) 1)
                   (Cow-Posx (ws-Cow ws))
                   (Cow-Posy (ws-Cow ws)))
                  (ws-Laps ws)
                  (ws-Ticks ws))]
        [(key=? ke "a")
         (make-ws (make-Cow
                   (if (= (Cow-Speed (ws-Cow ws)) 0)
                       0
                       (- (Cow-Speed (ws-Cow ws)) 1))
                   (Cow-Posx (ws-Cow ws))
                   (Cow-Posy (ws-Cow ws)))
                  (ws-Laps ws)
                  (ws-Ticks ws))]
        [else ws]))


(check-expect (click WS3 10 10 "button-down")
              (make-ws
               (make-Cow (Cow-Speed (ws-Cow WS3)) 10 10)
               (ws-Laps WS3)
               (ws-Ticks WS3)
               ))

(check-expect (click WS4 250 700 "button-up")
              WS4)

(check-expect (click WS4 250 700 "button-down")
              (make-ws
               (make-Cow (Cow-Speed (ws-Cow WS4)) 250 700)
               (ws-Laps WS4)
               (ws-Ticks WS4)
               ))






(define (click ws x y mouse-event)

  (if (string=? mouse-event "button-down")
      (make-ws
       (make-Cow (Cow-Speed (ws-Cow ws)) x y)
       (ws-Laps ws)
       (ws-Ticks ws)
       )
      ws))




(check-expect (move-cow COW1) (make-Cow 1 1 (Cow-Posy COW1)))
(check-expect (move-cow COW2) (make-Cow 10 (+ (/ WIDTH 2) 10)
                                        (Cow-Posy COW2)))
(check-expect (move-cow COW3) (make-Cow 1 0 (Cow-Posy COW3)))
(check-expect (move-cow COW4) (make-Cow 200 (+ (Cow-Posx COW4) 200)
                                        (Cow-Posy COW4)))

(define (move-cow cow)
  (cond [(> (+ (Cow-Posx cow) (Cow-Speed cow)) WIDTH)
             
         (make-Cow (Cow-Speed cow)
                   0
                   (Cow-Posy cow))]
        [else (make-Cow (Cow-Speed cow)
                        (+ (Cow-Speed cow)
                           (Cow-Posx cow))
                        (Cow-Posy cow))]))





(check-expect (update-laps WS1) (ws-Laps WS1))
(check-expect (update-laps WS3) (+ (ws-Laps WS3) 1))


(define (update-laps ws)
  (if (> (+ (Cow-Posx (ws-Cow ws)) (Cow-Speed (ws-Cow ws))) WIDTH)
      (+(ws-Laps ws) 1)
      (ws-Laps ws)))
         
