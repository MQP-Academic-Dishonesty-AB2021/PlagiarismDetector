

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Lab2P1005) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))



(define-struct cow (x y dx rotation))

(define (fn-for-cow cow)
  (... (cow-x cow)
       (cow-y cow)
       (cow-dx cow)
       (cow-rotation cow)))



(define-struct world (lap cow))

(define (fn-for-world world)
  (... (world-lap world)
       (world-cow world)))


(define START
  (make-world
   0
   (make-cow
    0
    (/ HEIGHT 2)
    1
    1)))


(define (main world)
  (big-bang world
    (on-tick next)
    (on-draw render)
    (on-key key-hit)
    (on-mouse mouse-hit)))



(define FONTSIZE 20)
(define FONTCOLOR "red")




(check-expect (render START)
              (place-image
               (above/align
                "left"
                (text
                 (string-append
                  "Distance: "
                  (number->string 0)
                  " laps") FONTSIZE FONTCOLOR)
                (text
                 (string-append
                  "Speed: "
                  (number->string 1)
                  " pixels/tick ") FONTSIZE FONTCOLOR))
               100
               24
               (draw-cow START)))
(check-expect (render (make-world 2 (make-cow 10 15 20 1)))
              (place-image
               (above/align
                "left"
                (text
                 (string-append
                  "Distance: "
                  (number->string 2)
                  " laps") FONTSIZE FONTCOLOR)
                (text
                 (string-append
                  "Speed: "
                  (number->string 20)
                  " pixels/tick ") FONTSIZE FONTCOLOR))
               100
               24
               (draw-cow (make-world 2 (make-cow 10 15 20 1)))))

  

(define (render world)
  (place-image
   (above/align
    "left"
    (text
     (string-append
      "Distance: "
      (number->string (world-lap world))
      " laps") FONTSIZE FONTCOLOR)
    (text
     (string-append
      "Speed: "
      (number->string (cow-dx (world-cow world)))
      " pixels/tick ") FONTSIZE FONTCOLOR))
   100
   24
   (draw-cow world)))




(check-expect (draw-cow (make-world 2 (make-cow 10 15 20 1)))
              (place-image (rotate 1 COW)
                           10
                           15
                           MTS))
(check-expect (draw-cow START)
              (place-image (rotate 1 COW)
                           (cow-x (world-cow START))
                           (cow-y (world-cow START))
                           MTS))
  

(define (draw-cow world)
  (place-image (rotate (cow-rotation (world-cow world)) COW)
               (cow-x (world-cow world))
               (cow-y (world-cow world))
               MTS))



(check-expect (mouse-hit START 0 0 "button-down")
              (make-world 0 (make-cow 0 0 1 1)))
(check-expect (mouse-hit
               (make-world 2 (make-cow 10 15 20 1)) 100 150 "button-down")
              (make-world 2 (make-cow 100 150 20 1)))
(check-expect (mouse-hit START 0 0 "button-up")
              START)

  

(define (mouse-hit world mx my me)
  (make-world
   (world-lap world)
   (cond [(mouse=? me "button-down")
          (make-cow mx
                    my
                    (cow-dx (world-cow world))
                    (cow-rotation (world-cow world)))]
         [else (world-cow world)]))) 





(check-expect (key-hit START " ") START)
(check-expect (key-hit START "w") START)
(check-expect (key-hit START "a")
              (make-world
               0
               (make-cow (cow-x (world-cow START))
                         (cow-y (world-cow START))
                         (- 1 1)
                         (cow-rotation (world-cow START)))))
(check-expect (key-hit (make-world 2 (make-cow 100 150 0 -1)) "a")
              (make-world 2
                          (make-cow 100
                                    150
                                    0
                                    -1)))
(check-expect (key-hit START "s")
              (make-world 0
                          (make-cow (cow-x (world-cow START))
                                    (/ HEIGHT 2)
                                    (+ 1 1)
                                    1)))


  

(define (key-hit world key) 
  (make-world (world-lap world)
              (make-cow
               (cow-x (world-cow world))
               (cow-y (world-cow world))
               (cond [(key=? key "s")
                      (+ (cow-dx (world-cow world)) 1)]
                     [(key=? key "a")
                      (if (> (cow-dx (world-cow world)) 0)
                          (- (cow-dx (world-cow world)) 1)
                          0)]
                     [else
                      (cow-dx (world-cow world))])
               (cow-rotation (world-cow world)))))





(check-expect (next START)
              (make-world (check-lap START)
                          (next-cow (world-cow START))))
(check-expect (next (make-world 0
                                (make-cow WIDTH 100 2 1)))
              (make-world (check-lap
                           (make-world 0 (make-cow WIDTH 100 2 1)))
                          (next-cow (make-cow WIDTH 100 2 1))))

  

(define (next world)
  (make-world (check-lap world)
              (next-cow (world-cow world))))




(check-expect (next-cow (world-cow START))
              (make-cow (move-cow (world-cow START))
                        (cow-y (world-cow START))
                        (cow-dx (world-cow START))
                        (if (= (cow-dx (world-cow START)) 0)
                            (cow-rotation (world-cow START))
                            (jiggle-rotation
                             (cow-rotation (world-cow START))))))
(check-expect (next-cow (make-cow 100 150 0 -1))
              (make-cow (move-cow (make-cow 100 150 0 -1))
                        150
                        0
                        (if (= (cow-dx (make-cow 100 150 0 -1)) 0)
                            (cow-rotation (make-cow 100 150 0 -1))
                            (jiggle-rotation
                             (cow-rotation (make-cow 100 150 0 -1))))))


  

(define (next-cow cow)
  (make-cow (move-cow cow)
            (cow-y cow)
            (cow-dx cow)
            (if (= (cow-dx cow) 0)
                (cow-rotation cow)
                (jiggle-rotation (cow-rotation cow)))))




(check-expect (move-cow (world-cow START)) 
              (+ (cow-x (world-cow START)) (cow-dx (world-cow START))))
(check-expect (move-cow (make-cow WIDTH 100 2 1)) 0)
(check-expect (move-cow (make-cow WIDTH 100 10 1)) 0)

  

(define (move-cow cow)
  (if (> (+ (cow-x cow) (cow-dx cow)) WIDTH)
      0
      (+ (cow-x cow) (cow-dx cow))))
            



(check-expect (check-lap START) (world-lap START))
(check-expect (check-lap (make-world 2 (make-cow 0 100 1 -1))) 2)
(check-expect (check-lap (make-world 4 (make-cow WIDTH 130 1 1))) (+ 4 1))
(check-expect (check-lap (make-world 3 (make-cow WIDTH 100 0 -1))) 3) 


  

(define (check-lap world)
  (if (= (move-cow (world-cow world)) 0)
      (+ (world-lap world) 1)
      (world-lap world))) 
      



(check-expect (jiggle-rotation 0) -1)
(check-expect (jiggle-rotation -1) 1)
(check-expect (jiggle-rotation 1) -1)
(check-expect (jiggle-rotation 500) -1)

  

(define (jiggle-rotation rot)
  (if (= rot -1)
      1 
      -1))