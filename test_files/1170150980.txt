

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |cow done|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))

(define ANGLE 4)

(define COLOR "red") 
(define FONTSIZE 30) 
(define DISPLAY-X-POS .20) 
(define DISPLAY-Y-POS .10) 
(define MINSPEED 0) 


(define-struct ws (x y laps speed angle))


(define START (make-ws (/ WIDTH 2) (/ HEIGHT 2) 0 1 ANGLE))


(define (main ws)
  (big-bang ws
    (on-tick update-ws)
    (to-draw render)
    (on-key key-click)
    (on-mouse mouse-click)))









(check-expect (update-ws  (make-ws WIDTH (/ HEIGHT 2) 0 1 ANGLE))
              (make-ws 0 (/ HEIGHT 2) (+ 0 1) 1 ANGLE))


(check-expect (update-ws  (make-ws (/ WIDTH 2) (/ HEIGHT 2) 0 0 0))
              (make-ws (/ WIDTH 2) (/ HEIGHT 2) 0 0 0))

(check-expect (update-ws  (make-ws (/ WIDTH 2) (/ HEIGHT 2) 0 1 ANGLE))
              (make-ws (+ (/ WIDTH 2) 1) (/ HEIGHT 2) 0 1 (* ANGLE -1)))





(define (update-ws ws)
  (cond [(>= (ws-x ws) WIDTH)
         (make-ws 0 (ws-y ws) (+ (ws-laps ws) 1) (ws-speed ws) (ws-angle ws))]
        [(= (ws-speed ws) 0)
         (make-ws (ws-x ws) (ws-y ws) (ws-laps ws) 0 0)]
        [else
         (make-ws (+(ws-x ws) (ws-speed ws))
                  (ws-y ws) (ws-laps ws) (ws-speed ws) (- (ws-angle ws)))]))









(check-expect (render (make-ws (/ WIDTH 2) (/ HEIGHT 2) 0 0 0))
              (place-image (text
                            (string-append
                             "Distance: " (number->string 0) " laps" "\n"
                             "Speed: " (number->string 0) " pixels/tick")
                            FONTSIZE COLOR )
                           (* WIDTH DISPLAY-X-POS) (* HEIGHT DISPLAY-Y-POS)
                           
                           (place-image
                            COW (/ WIDTH 2) (/ HEIGHT 2) MTS)))

(check-expect (render (make-ws (/ WIDTH 3) (/ HEIGHT 2) 25 2 ANGLE))
              (place-image (text
                            (string-append
                             "Distance: " (number->string 25) " laps" "\n"
                             "Speed: " (number->string 2) " pixels/tick")
                            FONTSIZE COLOR )
                           (* WIDTH DISPLAY-X-POS) (* HEIGHT DISPLAY-Y-POS)
                           (place-image
                            (rotate ANGLE COW) (/ WIDTH 3) (/ HEIGHT 2) MTS)))

(check-expect (render (make-ws 0 (/ HEIGHT 4) 25 2 (- ANGLE)))
              (place-image (text
                            (string-append
                             "Distance: " (number->string 25) " laps" "\n"
                             "Speed: " (number->string 2) " pixels/tick")
                            FONTSIZE COLOR )
                           (* WIDTH DISPLAY-X-POS) (* HEIGHT DISPLAY-Y-POS)
                           (place-image
                            (rotate (- ANGLE) COW) 0 (/ HEIGHT 4) MTS)))

(define (render ws)
  (place-image (text (string-append
                      "Distance: " (number->string (ws-laps ws)) " laps" "\n"
                      "Speed: " (number->string (ws-speed ws)) " pixels/tick")
                     FONTSIZE COLOR )
               (* WIDTH DISPLAY-X-POS) (* HEIGHT DISPLAY-Y-POS)
               (place-image
                (rotate (ws-angle ws) COW) (ws-x ws) (ws-y ws) MTS)))







(check-expect (key-click  (make-ws (/ WIDTH 2) (/ HEIGHT 2) 1 1 ANGLE) "s")
              (make-ws (/ WIDTH 2) (/ HEIGHT 2) 1 (+ 1 1) ANGLE))
(check-expect (key-click  (make-ws (/ WIDTH 2) (/ HEIGHT 2) 2 3 ANGLE) "a")
              (make-ws (/ WIDTH 2) (/ HEIGHT 2) 2 (- 3 1) ANGLE))
(check-expect (key-click  (make-ws (/ WIDTH 2) (/ HEIGHT 3) 3 5 ANGLE) "r")
              (make-ws (/ WIDTH 2) (/ HEIGHT 3) 3 5 ANGLE))

(check-expect (key-click  (make-ws (/ WIDTH 2) (/ HEIGHT 2) 4 0 ANGLE) "a")
              (make-ws (/ WIDTH 2) (/ HEIGHT 2) 4 0 ANGLE))


(define (key-click ws key-pressed)
  (make-ws (ws-x ws) (ws-y ws) (ws-laps ws)
           (cond [(key=? key-pressed "a")
                  (max MINSPEED (- (ws-speed ws) 1))]
                 [(key=? key-pressed "s")
                  (+ (ws-speed ws) 1)]
                 [else
                  (ws-speed ws)])
           (if (and
                (key=? key-pressed "s")
                (= 0 (ws-speed ws)))
               ANGLE
               (ws-angle ws))))







(check-expect (mouse-click
               (make-ws (/ WIDTH 2) (/ HEIGHT 2) 1 1 ANGLE)
               100 100 "button-down")
              (make-ws
               100 100 1 1 ANGLE))
(check-expect (mouse-click
               (make-ws (/ WIDTH 2) (/ HEIGHT 2) 2 2 ANGLE)
               200 200 "button-up")
              (make-ws
               (/ WIDTH 2) (/ HEIGHT 2) 2 2 ANGLE))
(check-expect (mouse-click
               (make-ws (/ WIDTH 2) (/ HEIGHT 2) 3 3 ANGLE)
               -300 300 "button-down")
              (make-ws
               0 300 3 3 ANGLE))


(define (mouse-click ws mouse-x mouse-y button-pressed)
  (if (mouse=? button-pressed "button-down")
      (make-ws (in-range mouse-x WIDTH)
               (in-range mouse-y HEIGHT)
               (ws-laps ws) (ws-speed ws) (ws-angle ws))
      ws))







(check-expect (in-range 5 10) 5)
(check-expect (in-range -20 8) 0)
(check-expect (in-range 11 10) 10)

(define (in-range num max)
  (cond [(< num 0) 0]
        [(> num max) max]
        [else num]))

