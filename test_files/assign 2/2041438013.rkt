

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment2_cow_audreyG_cierraO) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)



(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))

(define STARTX 0)
(define STARTY (/ HEIGHT 2))
(define ANGLE 1)

(define FONTSIZE 20)
(define FONTCOLOR "black")

(define LINEONEX 150) 
(define LINEONEY 100)












(define-struct cow (xPos yPos velocity angle lapNum))

(define START (make-cow STARTX STARTY 1 0 0))

 







(define (main cow)
  (big-bang cow
    (on-tick update-cow)
    (to-draw render-cow)
    (on-key  update-velocity-angle)
    (on-mouse teleport-cow)))









(check-expect (update-cow (make-cow STARTX STARTY 1 ANGLE 0)) (make-cow (+ 1 STARTX) STARTY 1 (- ANGLE) 0))
(check-expect (update-cow (make-cow 70 125 5 (- ANGLE) 0)) (make-cow (+ 5 70) 125 5 ANGLE 0))
(check-expect (update-cow (make-cow WIDTH 100 3 ANGLE 0)) (make-cow STARTX 100 3 (- ANGLE) 1))
(check-expect (update-cow (make-cow WIDTH 100 3 ANGLE 1)) (make-cow STARTX 100 3 (- ANGLE) 2))
(check-expect (update-cow (make-cow WIDTH STARTY 0 ANGLE 0)) (make-cow WIDTH STARTY 0 0 1))

(define (update-cow cow)
  (make-cow (change-x cow)
            (cow-yPos cow)
            (cow-velocity cow)
            (change-angle cow)
            (if (>= (cow-xPos cow) WIDTH) (+ (cow-lapNum cow) 1) (cow-lapNum cow))))




  
(check-expect (change-x (make-cow STARTX STARTY 1 ANGLE 0)) (+ 1 STARTX))
(check-expect (change-x (make-cow WIDTH 100 3 ANGLE 0)) STARTX)



(define (change-x cow)
  (if (and (>= (cow-xPos cow) WIDTH) (> (cow-velocity cow) 0))
      STARTX
      (+ (cow-xPos cow) (cow-velocity cow))))







(check-expect (change-angle (make-cow STARTX STARTY 1 ANGLE 0)) (- ANGLE))
(check-expect (change-angle (make-cow 30 100 0 (- ANGLE) 0)) 0)
(check-expect (change-angle (make-cow 200 37 3 0 2)) ANGLE)

(define (change-angle cow)
  (cond [(and (> (cow-velocity cow) 0) (= (cow-angle cow) 0)) 
                   ANGLE] 
                  [(= (cow-velocity cow) 0)
                   0]     
                  [else (- (cow-angle cow))]))
                          









(check-expect (render-cow (make-cow STARTX STARTY 1 ANGLE 0))
              (place-image (rotate ANGLE COW) STARTX STARTY
                           (place-image
                            (above/align "right"
                                         (text (string-append "Distance: " (number->string 0)) FONTSIZE FONTCOLOR)
                                         (text (string-append "Velocity (pixels/tick): " (number->string 1)) FONTSIZE FONTCOLOR))
                                          LINEONEX LINEONEY MTS)))

(define (render-cow cow)
  (place-image (rotate (cow-angle cow) COW) (cow-xPos cow) (cow-yPos cow) (render-text cow)))







(check-expect (render-text (make-cow STARTX STARTY 1 ANGLE 0))
                (place-image (above/align "right"
                                          (text (string-append "Distance: " (number->string 0)) FONTSIZE FONTCOLOR)
                                          (text (string-append "Velocity (pixels/tick): " (number->string 1)) FONTSIZE FONTCOLOR))
                                          LINEONEX LINEONEY MTS))

(define (render-text cow)
  (place-image (above/align "right"
                            (text (string-append "Distance: " (number->string (cow-lapNum cow))) FONTSIZE FONTCOLOR)
                            (text (string-append "Velocity (pixels/tick): " (number->string (cow-velocity cow))) FONTSIZE FONTCOLOR))
                            LINEONEX LINEONEY MTS))










(check-expect (update-velocity-angle (make-cow STARTX STARTY 1 ANGLE 0) "s") (make-cow STARTX STARTY (+ 1 1) ANGLE 0)) 
(check-expect (update-velocity-angle (make-cow STARTX STARTY 1 ANGLE 0) "d") (make-cow STARTX STARTY 1 ANGLE 0)) 
(check-expect (update-velocity-angle (make-cow STARTX STARTY 1 ANGLE 0) "a") (make-cow STARTX STARTY (- 1 1) ANGLE 0)) 
(check-expect (update-velocity-angle (make-cow STARTX STARTY 0 ANGLE 0) "a") (make-cow STARTX STARTY 0 0 0)) 

(define (update-velocity-angle cow keyEvent)
  (cond [(key=? "s" keyEvent) (make-cow (cow-xPos cow) (cow-yPos cow) (+ 1 (cow-velocity cow)) (cow-angle cow) (cow-lapNum cow))]
        [(= (cow-velocity cow) 0) (make-cow (cow-xPos cow) (cow-yPos cow) 0 0 (cow-lapNum cow))]
        [(key=? "a" keyEvent) (make-cow (cow-xPos cow) (cow-yPos cow) (- (cow-velocity cow) 1) (cow-angle cow) (cow-lapNum cow))]
        [else cow]))







(check-expect (teleport-cow START 40 300 "button-down") (make-cow 40 300 1 0 0))
(check-expect (teleport-cow START 40 300 "button-up") START)

(define (teleport-cow cow x y mouseEvent)
  (if (string=? mouseEvent "button-down")
      (make-cow x y (cow-velocity cow) (cow-angle cow) (cow-lapNum cow))
      cow))