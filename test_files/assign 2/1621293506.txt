

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Jennings-Itty Part 0 Assignment 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)












(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define ANGLE 3)

(define MTS (empty-scene WIDTH HEIGHT))
(define COUNTERX (/ WIDTH 10))
(define COUNTERY (/ HEIGHT 10))



(define-struct cow (x y speed laps tip?))







(define START (make-cow 0 (/ HEIGHT 2) 1 0 false)) 

 








(define (main cow)
  (big-bang cow               
    (on-tick   move-cow)      
    (to-draw   renderScene)   
    (on-mouse  handleMouse)   
    (on-key    handleKey)))   





(check-expect (move-cow START)
              (make-cow (+ 0 (cow-speed START)) (/ HEIGHT 2) 1 0 true))
(check-expect (move-cow (make-cow WIDTH 0 1 0 true))
              (make-cow 0 0 1 1 false))



(define (move-cow cow)
  (cond [(>= (cow-x cow) WIDTH)
         (make-cow 0
                   (cow-y cow)
                   (cow-speed cow)
                   (+ 1 (cow-laps cow))
                   (not (cow-tip? cow)))]
        [else
         (make-cow (+ (cow-x cow) (cow-speed cow))
                   (cow-y cow)
                   (cow-speed cow)
                   (cow-laps cow)
                   (not (cow-tip? cow)))]))







(define (renderScene cow)
  (place-images (list (renderCow cow) (renderText cow))
                (list (make-posn (cow-x cow) (cow-y cow))
                      (make-posn COUNTERX COUNTERY))
                MTS))





(check-expect (renderCow START)
              (rotate (* -1 ANGLE) COW))
(check-expect (renderCow (make-cow 0 0 1 0 true))
              (rotate  ANGLE COW))
(check-expect (renderCow (make-cow 0 0 0 0 true))
              COW)


(define (renderCow cow)
  (cond [(= (cow-speed cow) 0) COW]
        [else
         (if (cow-tip? cow)
             (rotate  ANGLE COW)
             (rotate (* -1 ANGLE) COW))]))



(check-expect (renderText START)
              (above (text "Speed: 1" 18 "red")
                     (text "Laps: 0" 18 "red")))
(check-expect (renderText (make-cow 0 0 0 0 true))
              (above (text "Speed: 0" 18 "red")
                     (text "Laps: 0" 18 "red")))


(define (renderText cow)
  (above (text (string-append "Speed: " (number->string (cow-speed cow)))
               18 "red")
         (text (string-append "Laps: " (number->string (cow-laps cow)))
               18 "red")))





(check-expect (handleKey START "a")
              (make-cow (cow-x START)
                        (cow-y START)
                        (- (cow-speed START) 1)
                        0
                        false))
(check-expect (handleKey START "s")
              (make-cow (cow-x START)
                        (cow-y START)
                        (+ (cow-speed START) 1)
                        0
                        false))
(check-expect (handleKey START "w") START)
(check-expect (handleKey (make-cow 0 0 0 0 false) "a")
              (make-cow 0 0 0 0 false))



(define (handleKey cow kE)
  (cond [(key=? kE "a")
         (if (= (cow-speed cow) 0)
             cow
             (make-cow (cow-x cow)
                       (cow-y cow)
                       (- (cow-speed cow) 1)
                       (cow-laps cow)
                       (cow-tip? cow)))]
        [(key=? kE "s")
         (make-cow (cow-x cow)
                   (cow-y cow)
                   (+ (cow-speed cow) 1)
                   (cow-laps cow)
                   (cow-tip? cow))]
        [else cow]))




(check-expect (handleMouse START 0 0 "button-down")
              (make-cow 0 0 (cow-speed START) 0 false))
(check-expect (handleMouse START 20 50 "button-down")
              (make-cow 20 50 (cow-speed START) 0 false))
(check-expect (handleMouse START 0 0 "button-up") START)



(define (handleMouse cow x y mE)
  (cond [(mouse=? mE "button-down")
         (make-cow x y (cow-speed cow) (cow-laps cow) (cow-tip? cow))]
        [else cow]))