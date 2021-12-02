

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Kai Nakamura Cara Salter Assignment 2 Cow|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))







(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define COW .)
(define MTS (empty-scene WIDTH HEIGHT))
(define ROTATION-AMOUNT 2)
(define FONT-SIZE 14)
(define FONT-COLOR "black")
(define MARGIN 5)








(define LEFT "left")
(define RIGHT "right")

 












(define-struct cow (x y speed rotation laps))

(define START (make-cow 0 CTR-Y 1 LEFT 0))
(define MIDDLE (make-cow CTR-X CTR-Y 1 LEFT 0))
(define END (make-cow WIDTH CTR-Y 1 LEFT 0))

 








"Start the world with (main START)"
(define (main cow)
  (big-bang cow
    (on-tick update)
    (to-draw render)
    (on-key handle-key)
    (on-mouse handle-mouse)))




(check-expect (update START)
              (make-cow 1 CTR-Y 1 RIGHT 0))
(check-expect (update MIDDLE)
              (make-cow (+ 1 CTR-X) CTR-Y 1 RIGHT 0))
(check-expect (update END)
              (make-cow 0 CTR-Y 1 RIGHT 1))

  

(define (update cow)
  (rotate-cow (wrap-cow (move-cow cow))))





  

(check-expect (wrap-cow END)
              (make-cow 0 CTR-Y 1 LEFT 1))
(check-expect (wrap-cow START)
              START)

(define (wrap-cow cow)
  (if (>= (cow-x cow) WIDTH)
      (make-cow 0
                (cow-y cow)
                (cow-speed cow)
                (cow-rotation cow)
                (+ (cow-laps cow) 1))
      cow))





  

(check-expect (move-cow (make-cow 0 CTR-Y 1 RIGHT 0))
              (make-cow 1 CTR-Y 1 RIGHT 0))
(check-expect (move-cow (make-cow 1 CTR-Y 5 LEFT 0))
              (make-cow 6 CTR-Y 5 LEFT 0))
(check-expect (move-cow (make-cow 2 CTR-Y 0 RIGHT 0))
              (make-cow 2 CTR-Y 0 RIGHT 0))

(define (move-cow cow)
  (make-cow (+ (cow-x cow)
               (cow-speed cow))
            (cow-y cow)
            (cow-speed cow)
            (cow-rotation cow)
            (cow-laps cow)))





  

(check-expect (rotate-cow (make-cow 0 CTR-Y 1 RIGHT 0))
              (make-cow 0 CTR-Y 1 LEFT 0))
(check-expect (rotate-cow (make-cow 0 CTR-Y 1 LEFT 0))
              (make-cow 0 CTR-Y 1 RIGHT 0))
(check-expect (rotate-cow (make-cow 0 CTR-Y 0 LEFT 0))
              (make-cow 0 CTR-Y 0 LEFT 0))
(check-expect (rotate-cow (make-cow 0 CTR-Y 0 RIGHT 0))
              (make-cow 0 CTR-Y 0 RIGHT 0))

(define (rotate-cow cow)
  (cond [(= 0 (cow-speed cow))
         cow]
        [(string=? LEFT (cow-rotation cow))
         (make-cow (cow-x cow)
                   (cow-y cow)
                   (cow-speed cow)
                   RIGHT
                   (cow-laps cow))]
        [else
         (make-cow (cow-x cow)
                   (cow-y cow)
                   (cow-speed cow)
                   LEFT
                   (cow-laps cow))]))





  

(check-expect (render START) (render-text START
                                          (render-cow START MTS)))
(check-expect (render MIDDLE) (render-text START
                                           (render-cow MIDDLE MTS)))
(check-expect (render END) (render-text START
                                        (render-cow END MTS)))

(define (render cow)
  (render-text cow
               (render-cow cow MTS)))





  

(check-expect (render-cow START MTS)
              (place-image (rotate ROTATION-AMOUNT COW) 0 CTR-Y MTS))
(check-expect (render-cow MIDDLE MTS)
              (place-image (rotate ROTATION-AMOUNT COW) CTR-X CTR-Y MTS))
(check-expect (render-cow (make-cow CTR-X CTR-Y 1 RIGHT 0) MTS)
              (place-image (rotate (- ROTATION-AMOUNT) COW) CTR-X CTR-Y MTS))
(check-expect (render-cow END MTS)
              (place-image (rotate ROTATION-AMOUNT COW) WIDTH CTR-Y MTS))

(define (render-cow cow scene)
  (place-image
   (rotate (if (string=? LEFT (cow-rotation cow))
               ROTATION-AMOUNT
               (- ROTATION-AMOUNT))
           COW)
   (cow-x cow)
   (cow-y cow)
   scene))







  

(check-expect (render-text START MTS)
              (place-image/align 
               (above/align "left"
                            (text (string-append "Speed: "
                                                 (number->string
                                                  (cow-speed START))
                                                 " pixel(s) per second")
                                  FONT-SIZE
                                  FONT-COLOR)
                            (text (string-append "Laps: "
                                                 (number->string
                                                  (cow-laps START)))
                                  FONT-SIZE
                                  FONT-COLOR))
               MARGIN MARGIN "left" "top"
               MTS))

(check-expect (render-text (make-cow 0 0 1000 LEFT 100) MTS)
              (place-image/align 
               (above/align "left"
                            (text (string-append "Speed: "
                                                 (number->string 1000)
                                                 " pixel(s) per second")
                                  FONT-SIZE
                                  FONT-COLOR)
                            (text (string-append "Laps: "
                                                 (number->string 100))
                                  FONT-SIZE
                                  FONT-COLOR))
               MARGIN MARGIN "left" "top"
               MTS))

(define (render-text cow scene)
  (place-image/align 
   (above/align "left"
                (text (string-append "Speed: "
                                     (number->string (cow-speed cow))
                                     " pixel(s) per second")
                      FONT-SIZE
                      FONT-COLOR)
                (text (string-append "Laps: "
                                     (number->string (cow-laps cow)))
                      FONT-SIZE
                      FONT-COLOR))
   MARGIN MARGIN "left" "top"
   scene))







  

(check-expect (handle-key START "s") (make-cow 0 CTR-Y 2 LEFT 0))
(check-expect (handle-key START "a") (make-cow 0 CTR-Y 0 LEFT 0))
(check-expect (handle-key (make-cow 0 CTR-Y 0 LEFT 0) "a")
              (make-cow 0 CTR-Y 0 LEFT 0))

(define (handle-key cow key-event)
  (cond [(key=? key-event "s")
         (make-cow (cow-x cow)
                   (cow-y cow)
                   (add1 (cow-speed cow))
                   (cow-rotation cow)
                   (cow-laps cow))]
        [(key=? key-event "a")
         (make-cow (cow-x cow)
                   (cow-y cow)
                   (max 0
                        (sub1 (cow-speed cow)))
                   (cow-rotation cow)
                   (cow-laps cow))]
        [else cow]))






  

(check-expect (handle-mouse START 0 0 "button-down")
              (make-cow 0 0 1 LEFT 0))
(check-expect (handle-mouse MIDDLE 100 100 "button-down")
              (make-cow 100 100 1 LEFT 0))

(define (handle-mouse cow x y mouse-event)
  (cond [(mouse=? mouse-event "button-down")
         (make-cow x
                   y
                   (cow-speed cow)
                   (cow-rotation cow)
                   (cow-laps cow))]
        [else cow]))