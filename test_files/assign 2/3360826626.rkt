

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |cow starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define ROTATION-VARIANCE 5)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))

(define-struct WorldState (x y speed laps))






(define START (make-WorldState 0 (/ HEIGHT 2) 1 0))
(define NOSPEED (make-WorldState 0 (/ HEIGHT 2) 0 0))
(define FASTSPEED (make-WorldState 0 (/ HEIGHT 2) 14 0))
(define EDGE (make-WorldState WIDTH (/ HEIGHT 2) 1 0))

(define (main ws)
  (big-bang ws
    (on-tick tick)
    (to-draw draw)
    (on-key key)
    (on-mouse mouse)))


(check-expect (tick START) (make-WorldState (+ (WorldState-x START) 1) (WorldState-y START) (WorldState-speed START) (WorldState-laps START)))
(check-expect (tick NOSPEED) (make-WorldState (WorldState-x NOSPEED) (WorldState-y NOSPEED) (WorldState-speed NOSPEED) (WorldState-laps NOSPEED)))
(check-expect (tick FASTSPEED) (make-WorldState (+ (WorldState-x FASTSPEED) 14) (WorldState-y FASTSPEED) (WorldState-speed FASTSPEED) (WorldState-laps FASTSPEED)))
(check-expect (tick EDGE) (make-WorldState (WorldState-speed EDGE) (WorldState-y EDGE) (WorldState-speed EDGE) (+ (WorldState-laps EDGE) 1))) 
(define (tick ws) (let ([new-x (modulo (+ (WorldState-x ws) (WorldState-speed ws)) WIDTH)]) (make-WorldState new-x (WorldState-y ws) (WorldState-speed ws) (+ (if (< new-x (WorldState-x ws)) 1 0) (WorldState-laps ws)))))



(define (draw ws) (let ([txt (text (string-append "Distance: " (number->string (WorldState-laps ws)) " laps\nSpeed: " (number->string (WorldState-speed ws)) "pixels/tick") 24 "red")]
                        [new-cow (rotate (if (> (WorldState-speed ws) 0) (- (random (* ROTATION-VARIANCE 2)) ROTATION-VARIANCE) 0) COW)])
                    (place-image txt (/ (image-width txt) 2) (/ (image-height txt) 2)
                    (place-image new-cow (WorldState-x ws) (WorldState-y ws) (place-image new-cow (+ WIDTH (WorldState-x ws)) (WorldState-y ws)
                    (place-image new-cow (WorldState-x ws) (WorldState-y ws) (place-image new-cow (- (WorldState-x ws) WIDTH) (WorldState-y ws) MTS)))))))
 

(check-expect (key START "s") (make-WorldState (WorldState-x START)  (WorldState-y START)  (+ (WorldState-speed START) 1)  (WorldState-laps START)))
(check-expect (key START "a") (make-WorldState (WorldState-x START)  (WorldState-y START)  (- (WorldState-speed START) 1)  (WorldState-laps START)))
(check-expect (key START "d") (make-WorldState (WorldState-x START)  (WorldState-y START)  (+ (WorldState-speed START) 1)  (WorldState-laps START)))
(check-expect (key START " ") (make-WorldState (WorldState-x START)  (WorldState-y START)  (WorldState-speed START)  (WorldState-laps START)))
(check-expect (key NOSPEED "a") (make-WorldState (WorldState-x NOSPEED)  (WorldState-y NOSPEED)  (WorldState-speed NOSPEED)  (WorldState-laps NOSPEED)))

(define (key ws ke) (if (or (key=? ke "s") (key=? ke "d") (key=? ke "a"))
                        (make-WorldState (WorldState-x ws) (WorldState-y ws) (+ (if (key=? ke "a") (if (zero? (WorldState-speed ws)) 0 -1) 1) (WorldState-speed ws))  (WorldState-laps ws))
                          ws))




(check-expect (mouse START 60 50 "button-down") (make-WorldState 60 50 (WorldState-speed START) (WorldState-laps START)))
(check-expect (mouse START 79 24 "button-down") (make-WorldState 79 24 (WorldState-speed START) (WorldState-laps START)))
(check-expect (mouse START 6 5 "button-up") START)

(define (mouse ws x y me) (cond [(mouse=? me "button-down") (make-WorldState x y (WorldState-speed ws) (WorldState-laps ws))][else ws]))
