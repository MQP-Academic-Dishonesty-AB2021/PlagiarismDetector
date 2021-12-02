

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment-2-warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))
(define ANGLE 30)








(define-struct ws (laps speed x y currang image))
       



(define (main ws)
        (big-bang ws
           (on-tick move-cow)
           (on-mouse relocate-cow)
           (on-key change-speed)
           (to-draw render)))

(define WS1 (make-ws 0 1 0 500 0 COW))
(define WS2 (make-ws 0 1 500 500 0 COW))
(define WS3 (make-ws 1 5 (- WIDTH 3) 500 0 COW))
(define WS4 (make-ws 0 0 500 500 0 COW))
(define WS5 (make-ws 0 3 500 500 ANGLE COW))
(define WS6 (make-ws 0 3 500 500 (- 0 ANGLE) COW))




(check-expect (move-cow WS1) (make-ws 0 1 1 500 30 (rotate 30 COW)))
(check-expect (move-cow WS2) (make-ws (ws-laps WS2) (ws-speed WS2) (+ (ws-speed WS2) (ws-x WS2)) (ws-y WS2) (+ 30 (ws-currang WS2)) (rotate 30 COW)))
(check-expect (move-cow WS3) (make-ws (+ (ws-laps WS3) 1) (ws-speed WS3) (modulo (+ (ws-speed WS3) (ws-x WS3)) WIDTH) (ws-y WS3) (+ 30 (ws-currang WS3)) (rotate 30 COW)))

(define (move-cow ws)
  (make-ws (if (>= (+ (ws-speed ws) (ws-x ws)) WIDTH)
               (+ 1 (ws-laps ws))
               (ws-laps ws))
           (ws-speed ws)
           (modulo (+ (ws-speed ws) (ws-x ws)) WIDTH)
           (ws-y ws)
           (ws-currang (rotate-cow ws))
           (ws-image (rotate-cow ws))))




(check-expect (relocate-cow WS1 300 400 "button-down") (make-ws 0 1 300 400 0 COW))
(check-expect (relocate-cow WS2 300 400 "button-down") (make-ws (ws-laps WS2) (ws-speed WS2) 300 400 (ws-currang WS2) COW))

(define (relocate-cow ws x y me)
  (if (equal? me "button-down")
      (make-ws (ws-laps ws) (ws-speed ws) x y (ws-currang ws) COW)
      ws))
  




(check-expect (change-speed WS1 "s") (make-ws 0 2 0 500 0 COW))
(check-expect (change-speed WS2 "a") (make-ws (ws-laps WS2) (- (ws-speed WS2) 1) (ws-x WS2) (ws-y WS2) (ws-currang WS2) COW))
(check-expect (change-speed WS3 "d") (make-ws (ws-laps WS3) (ws-speed WS3) (ws-x WS3) (ws-y WS3) (ws-currang WS3) COW))
(check-expect (change-speed WS4 "a") WS4)

(define (change-speed ws key)
        (cond
          [(equal? key "s") (make-ws (ws-laps ws) (+ (ws-speed ws) 1) (ws-x ws) (ws-y ws) (ws-currang ws) COW)]
          [(equal? key "a") (if (> (ws-speed ws) 0)
                                (make-ws (ws-laps ws) (- (ws-speed ws) 1) (ws-x ws) (ws-y ws) (ws-currang ws) COW)
                                ws)]
          [else ws]))






(define (render ws)
         (above
          (above
           (text (string-append "Laps: " (number->string (ws-laps ws))) 24 "red")
           (text (string-append "Speed: " (number->string (ws-speed ws)) " pixels/tick") 24 "red"))
          (place-image (ws-image ws) (ws-x ws) (ws-y ws) MTS)))





(check-expect (rotate-cow WS1) (make-ws 0 1 0 500 ANGLE (rotate 30 COW)))
(check-expect (rotate-cow WS5) (make-ws (ws-laps WS5) (ws-speed WS5) (ws-x WS5) (ws-y WS5) (- (ws-currang WS5) (* 2 ANGLE)) (rotate -30 COW)))
(check-expect (rotate-cow WS6) (make-ws (ws-laps WS6) (ws-speed WS6) (ws-x WS6) (ws-y WS6) (+ (ws-currang WS6) (* 2 ANGLE)) (rotate 30 COW)))


(define (rotate-cow ws)
  (cond
    [(= (ws-speed ws) 0) ws]
    [(= (ws-currang ws) 0) (make-ws (ws-laps ws) (ws-speed ws) (ws-x ws) (ws-y ws) ANGLE (rotate ANGLE (ws-image ws)))]
    [(= (ws-currang ws) ANGLE) (make-ws (ws-laps ws) (ws-speed ws) (ws-x ws) (ws-y ws) (- 0 ANGLE) (rotate (- 0 ANGLE) (ws-image ws)))]
    [(= (ws-currang ws) (- 0 ANGLE)) (make-ws (ws-laps ws) (ws-speed ws) (ws-x ws) (ws-y ws) ANGLE (rotate ANGLE (ws-image ws)))]))

(main WS1)