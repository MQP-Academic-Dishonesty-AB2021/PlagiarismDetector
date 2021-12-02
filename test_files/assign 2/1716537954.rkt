

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 2 Cow|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)






(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))




(define-struct cow (x y speed laps ticks))





 




(define ROTATION 5)
(define START-Y (/ HEIGHT 2))
(define START (make-cow 0 START-Y 1 0 0))
(define BLANK-COW (make-cow 0 0 0 0 0))
(define FONT-SIZE 20)
(define FONT-COLOR "black")
(define COW+ (rotate 2 COW))
(define COW- (rotate -2 COW))






(define (main cow)
  (big-bang cow
    (on-tick advance-cow)
    (to-draw render)
    (on-key change-speed)
    (on-mouse teleport)))



(check-expect (advance-cow START) (make-cow 1 START-Y 1 0 1))
(check-expect (advance-cow (make-cow (+ WIDTH 10) 20 15 0 0))
              (make-cow 0 20 15 1 1))

(define (advance-cow cow)
  (if (>= (cow-x cow) WIDTH)
      (update cow)
      (make-cow (update-x cow) (cow-y cow) (cow-speed cow) (cow-laps cow) (add1 (cow-ticks cow)))))
   


(check-expect (update (make-cow (+ WIDTH 1) 20 2 0 0))
              (make-cow 0 20 2 1 1))
                        
(define (update cow)
  (make-cow 0 (cow-y cow) (cow-speed cow) (add1 (cow-laps cow)) (add1 (cow-ticks cow))))



(check-expect (update-x START) (+ (cow-x START) (cow-speed START)))

(define (update-x cow)
  (+ (cow-x cow) (cow-speed cow)))



(check-expect (render START) (place-image (choose-image START) (cow-x START)
                                          (cow-y START) (background START)))

(define (render cow)
  (place-image (choose-image cow) (cow-x cow) (cow-y cow) (background cow)))



(check-expect (choose-image BLANK-COW) COW)
(check-expect (choose-image START) COW+)
(check-expect (choose-image (make-cow 0 0 4 0 5)) COW-)

(define (choose-image cow)
  (cond [(zero? (cow-speed cow)) COW]
        [(odd? (cow-ticks cow))
         COW-]
        [else
         COW+]))



(check-expect (background START)
              (overlay/align/offset "left" "top"
                                    (above/align "left"
                                                 (speed-text START)
                                                 (laps-text START))
                                    -10 -10 MTS))

(define (background cow)
  (overlay/align/offset "left" "top"
                        (above/align "left"
                                     (speed-text cow)
                                     (laps-text cow))
                        -10 -10 MTS))



(check-expect (speed-text START) (text "speed: 1 pixels/tick" FONT-SIZE FONT-COLOR))
(check-expect (speed-text BLANK-COW) (text "speed: 0 pixels/tick" FONT-SIZE FONT-COLOR))

(define (speed-text cow)
  (text (string-append "speed: "
                       (number->string (cow-speed cow))
                       " pixels/tick")
        FONT-SIZE FONT-COLOR))



(check-expect (laps-text START) (text "laps: 0" FONT-SIZE FONT-COLOR))

(define (laps-text cow)
  (text (string-append "laps: " (number->string (cow-laps cow)))
        FONT-SIZE FONT-COLOR))



(check-expect (change-speed START "s") (make-cow (cow-x START) (cow-y START)
                                                 (add1 (cow-speed START)) (cow-laps START) (cow-ticks START)))
(check-expect (change-speed START "a") (make-cow (cow-x START) (cow-y START)
                                                 (sub1 (cow-speed START)) (cow-laps START) (cow-ticks START)))
(check-expect (change-speed BLANK-COW "a") BLANK-COW)

(define (change-speed cow a-key)
  (cond [(key=? a-key "s")
         (make-cow (cow-x cow) (cow-y cow)
                   (add1 (cow-speed cow)) (cow-laps cow) (cow-ticks cow))]
        [(and (not (zero? (cow-speed cow))) (key=? a-key "a"))
         (make-cow (cow-x cow) (cow-y cow)
                   (sub1 (cow-speed cow)) (cow-laps cow) (cow-ticks cow))]
        [else cow]))



(check-expect (teleport START 10 10 "button-down")
              (make-cow 10 10 (cow-speed START) (cow-laps START) (cow-ticks START)))
(check-expect (teleport START 10 10 "move")
              START)

(define (teleport cow x y mouse)
  (cond [(mouse=? mouse "button-down")
         (make-cow x y (cow-speed cow) (cow-laps cow) (cow-ticks cow))]
        [else cow]))


    