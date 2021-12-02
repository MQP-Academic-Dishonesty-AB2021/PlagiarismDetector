

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment2-Cow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))

(define-struct cow (x y laps speed tilt))








(define (fn-for-cow cow)
  (... (cow-x cow)
       (cow-y cow)
       (cow-laps cow)
       (cow-speed cow)
       (cow-tilt cow)))


(define COW1 (make-cow (/ WIDTH 2) (/ HEIGHT 2) 0 1 3))
(define COW11 (make-cow (+ 1 (/ WIDTH 2)) (/ HEIGHT 2) 0 1 -3))

(define COW2 (make-cow (- WIDTH 5) (/ HEIGHT 2) 0 10 5))
(define COW21 (make-cow 0 (/ HEIGHT 2) 1 10 -5))

(define COW3 (make-cow (/ WIDTH 2) (/ HEIGHT 2) 0 15 4))
(define COW31 (make-cow (/ WIDTH 2) (/ HEIGHT 2) 0 5 4))

(define START COW1)







(check-expect (update-cow COW1) COW11)
(check-expect (update-cow COW2) COW21)

(define (update-cow cow)
  (if (> (+ (cow-x cow) (cow-speed cow)) WIDTH)
      (loop-cow cow)
      (move-cow cow)))







(check-expect (move-cow COW1)
              (make-cow (+ (/ WIDTH 2) (cow-speed COW1)) (/ HEIGHT 2) 0 1 -3))
(check-expect (move-cow COW2)
              (make-cow (+ (- WIDTH 5) (cow-speed COW2)) (/ HEIGHT 2) 0 10 -5))

(define (move-cow cow)
  (make-cow
   (+ (cow-x cow) (cow-speed cow))
   (cow-y cow)
   (cow-laps cow)
   (cow-speed cow)
   (if (= (cow-speed cow) 0)
       (cow-tilt cow)
       (- (cow-tilt cow)))))








(check-expect (loop-cow COW2) COW21)

(define (loop-cow cow)
  (make-cow
   0
   (cow-y cow)
   (add1 (cow-laps cow))
   (cow-speed cow)
   (- (cow-tilt cow))))






(define (draw-cow cow)
  (local
    [(define TILTED-COW
       (rotate (cow-tilt cow) COW))
     (define LAPS-TEXT
       (text (string-append "Laps completed: "
                            (number->string (cow-laps cow))) 24 "red"))
     (define SPEED-TEXT
       (text (string-append
              "Current speed : " (number->string (cow-speed cow))) 24 "red"))]
    (underlay/xy
     (underlay/xy
      (place-image
       TILTED-COW (cow-x cow) (cow-y cow) MTS) 10 10 LAPS-TEXT) 10 40 SPEED-TEXT)))







(define (handle-mouse cow x y m-e)
  (if (string=? m-e "button-down")
      (set-cow-pos cow x y)
      cow))






(check-expect (set-cow-pos COW1 10 10) (make-cow 10 10 0 1 3))
(check-expect (set-cow-pos COW2 15 30) (make-cow 15 30 0 10 5))

(define (set-cow-pos cow x y)
  (make-cow x
            y
            (cow-laps cow)
            (cow-speed cow)
            (cow-tilt cow)))






(check-expect (handle-key COW1 "a") (make-cow (/ WIDTH 2) (/ HEIGHT 2) 0 0 3))
(check-expect (handle-key COW1 "s") (make-cow (/ WIDTH 2) (/ HEIGHT 2) 0 2 3))
(check-expect (handle-key COW1 "q") (make-cow (/ WIDTH 2) (/ HEIGHT 2) 0 1 3))

(define (handle-key cow key)
  (cond [(and (string=? key "a") (> (cow-speed cow) 0)) (update-speed cow -1)]
        [(string=? key "s") (update-speed cow 1)]
        [else cow]))







(check-expect (update-speed COW1 10) (make-cow (/ WIDTH 2) (/ HEIGHT 2) 0 11 3))
(check-expect (update-speed COW3 -10) COW31)
(check-expect (update-speed COW2 0) COW2)
              

(define (update-speed cow speed)
  (make-cow
   (cow-x cow)
   (cow-y cow)
   (cow-laps cow)
   (+ (cow-speed cow) speed)
   (cow-tilt cow)))
        
(define (main ws)
  (big-bang ws
    (on-tick update-cow)
    (to-draw draw-cow)
    (on-mouse handle-mouse)
    (on-key handle-key)))