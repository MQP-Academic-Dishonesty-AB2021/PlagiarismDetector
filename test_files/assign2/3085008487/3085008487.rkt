

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname cow_work) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define COW .)

(require 2htdp/image)
(require 2htdp/universe)

(define SCENE-WIDTH 800)
(define SCENE-HEIGHT 600)
(define TEXT-SIZE 30)
(define TEXT-COLOR "red")

(define START-X (image-width COW))
(define START-Y (/ SCENE-HEIGHT 2))
(define START-SPEED 1)

(define MTS (empty-scene SCENE-WIDTH SCENE-HEIGHT))







(define-struct cow (pos-x pos-y dx laps ticks))

(define (fn-for-cow cow)
  (... (cow-pos-x cow)
       (cow-pos-y cow)
       (cow-dx cow)
       (cow-laps cow)
       (cow-ticks cow)))





(define (tick-world state)
  (local
    { (define new-x (+ (cow-pos-x state) (cow-dx state))) }
    (if (> new-x (+ SCENE-WIDTH START-X))
        (make-cow
         (- START-X)
         (cow-pos-y state)
         (cow-dx state)
         (add1 (cow-laps state))
         (add1 (cow-ticks state)))
        (make-cow
         new-x
         (cow-pos-y state)
         (cow-dx state)
         (cow-laps state)
         (add1 (cow-ticks state)))
        )))




(define (draw-world state)
  (place-images/align
   (list
    (rotate
     (cond
       [(not (zero? (cow-dx state)))
        (cond
          [(= (modulo (cow-ticks state) 14) 0) 1]
          [else -1])]
       [else 0])
     COW)
    (text
     (format "Distance: ~v laps" (cow-laps state))
     TEXT-SIZE
     TEXT-COLOR)
    (text
     (format "Speed:    ~v pixels/tick" (cow-dx state))
     TEXT-SIZE
     TEXT-COLOR))
   (list
    (make-posn
     (cow-pos-x state)
     (cow-pos-y state))
    (make-posn 10 10)
    (make-posn 10 (+ 10 TEXT-SIZE)))
   'left
   'top
   MTS))






(define (handle-key state evt)
  (cond
    [(key=? evt "s")
     (make-cow
      (cow-pos-x state)
      (cow-pos-y state)
      (add1 (cow-dx state))
      (cow-laps state)
      (cow-ticks state))]
    [(key=? evt "a")
     (make-cow
      (cow-pos-x state)
      (cow-pos-y state)
      (max 0 (sub1 (cow-dx state)))
      (cow-laps state)
      (cow-ticks state))]
    [else state]))






(define (handle-mouse state mx my evt)
  (cond
    [(mouse=? evt "button-up")
     (make-cow
      mx
      my
      (cow-dx state)
      (cow-laps state)
      (cow-ticks state))]
    [else state]))


(define START
  (make-cow
   START-X
   START-Y
   START-SPEED
   0
   0))   




(define (main state)
  (big-bang
      state
    (on-tick tick-world)
    (to-draw draw-world)
    (on-key handle-key)
    (on-mouse handle-mouse)))
(main START)