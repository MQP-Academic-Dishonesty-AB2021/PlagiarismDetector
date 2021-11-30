

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |cow starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)


(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))










(define-struct cow (speed xpos ypos laps angle))

 





(define (main cow)
  (big-bang cow
    (on-tick tick-cow)       
    (on-draw render)         
    (on-key keyevent)        
    (on-mouse mouse-event))) 



(define (tick-cow cow)
  (check-cow (update-cow cow)))



(check-expect (update-cow (make-cow 1 2 3 4 1)) (make-cow 1 3 3 4 -1))
(check-expect (update-cow (make-cow 0 2 3 4 1)) (make-cow 0 2 3 4 1))

(define (update-cow cow)
  (make-cow (cow-speed cow)
            (+ (cow-xpos cow) (cow-speed cow))
            (cow-ypos cow)
            (cow-laps cow)
            (if (<= (cow-speed cow) 0)
                (cow-angle cow)
                (* (cow-angle cow) -1))))




(check-expect (check-cow (make-cow 1 (+ WIDTH 1) 5 7 1)) (make-cow 1 0 5 8 1))
(check-expect (check-cow (make-cow 1 250 50 11 1)) (make-cow 1 250 50 11 1))
(check-expect (check-cow (make-cow 1 WIDTH 200 2 1)) (make-cow 1 WIDTH 200 2 1))

(define (check-cow cow)
  (if (> (cow-xpos cow) WIDTH)
      (make-cow (cow-speed cow)
                0
                (cow-ypos cow)
                (+ (cow-laps cow) 1)
                (cow-angle cow))
      cow))



(define START (make-cow 1 0 (/ HEIGHT 2) 0 1))
(define MID (make-cow 3 (/ WIDTH 2) (/ HEIGHT 2) 5 -1))
(check-expect (render START)
              (place-images (list (rotate (cow-angle START) COW)
                                  (draw-text (cow-laps START) (cow-speed START)))
                            (list (make-posn (cow-xpos START) (cow-ypos START))
                                  (make-posn 100 100))
                            MTS))
(check-expect (render MID)
              (place-images (list (rotate (cow-angle MID) COW)
                                  (draw-text (cow-laps MID) (cow-speed MID)))
                            (list (make-posn (cow-xpos MID) (cow-ypos MID))
                                  (make-posn 100 100))
                            MTS))

(define (render cow)
  (place-images (list (rotate (cow-angle cow) COW)
                      (draw-text (cow-laps cow) (cow-speed cow)))
                (list (make-posn (cow-xpos cow) (cow-ypos cow))
                      (make-posn 100 100))
                MTS))



(check-expect (draw-text 2 3) (above/align "left"
                                           (text "Distance: 2 laps" 20 "red")
                                           (text "Speed: 3 pixels/tick" 20 "red")))
(check-expect (draw-text 0 0) (above/align "left"
                                           (text "Distance: 0 laps" 20 "red")
                                           (text "Speed: 0 pixels/tick" 20 "red")))

(define (draw-text laps speed)
  (above/align "left"
               (text (string-append "Distance: " (number->string laps) " laps") 20 "red")
               (text (string-append "Speed: " (number->string speed) " pixels/tick") 20 "red")))




(check-expect (keyevent START "s") (make-cow 2 0 (/ HEIGHT 2) 0 1))
(check-expect (keyevent MID "a") (make-cow 2 (/ WIDTH 2) (/ HEIGHT 2) 5 -1))
(check-expect (keyevent MID "j") MID)

(define (keyevent cow ke)
  (cond [(key=? ke "s") (change-speed cow (+ (cow-speed cow) 1))]
        [(key=? ke "a") (change-speed cow (- (cow-speed cow) 1))]
        [else cow]))



(check-expect (change-speed (make-cow 0 500 200 5 1) 0) (make-cow 0 500 200 5 1))
(check-expect (change-speed (make-cow -1 400 100 3 -1) -1) (make-cow 0 400 100 3 -1))

(define (change-speed cow speed)
  (if (<= speed 0)
      (make-cow 0
                (cow-xpos cow)
                (cow-ypos cow)
                (cow-laps cow)
                (cow-angle cow))
      (make-cow speed
                (cow-xpos cow)
                (cow-ypos cow)
                (cow-laps cow)
                (cow-angle cow))))



(check-expect (mouse-event START 50 250 "button-down") (make-cow 1 50 250 0 1))
(check-expect (mouse-event MID 25 25 "button-down") (make-cow 3 25 25 5 -1))
(check-expect (mouse-event MID 75 75 "button-up") MID)

(define (mouse-event cow xpos ypos me)
  (cond [(mouse=? me "button-down")
         (make-cow (cow-speed cow) xpos ypos (cow-laps cow) (cow-angle cow))]
        [else cow]))

(main START)