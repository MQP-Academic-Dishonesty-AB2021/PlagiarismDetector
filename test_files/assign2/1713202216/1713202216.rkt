

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname VenatArjun_VieiraJulie_cowFinal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)



(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))


(define initial-x 0)
(define initial-y (/ HEIGHT 2))
(define initial-SPEED 1)
(define initial-angle 3)
(define initial-laps 0)


(define text-size 20)
(define text-color "red")
(define TEXT-X 100)
(define TEXT-Y 100)













(define-struct cow(x y dx angle laps))

 

(define COW1 (make-cow 20 initial-y 0 0 2))
(define COW2 (make-cow 40 80 13 initial-angle initial-laps))
(define COW3 (make-cow WIDTH 150 5 -5 4))







(define START (make-cow initial-x
                        initial-y
                        initial-SPEED
                        initial-angle
                        initial-laps))










(check-expect (decrease-speed COW1) (make-cow 20 initial-y 0 0 2))
(check-expect (decrease-speed COW2) (make-cow 40 80 (- 13 1) initial-angle initial-laps))
(check-expect (decrease-speed COW3) (make-cow (cow-x COW3) (cow-y COW3) (- (cow-dx COW3) 1) (cow-angle COW3) (cow-laps COW3)))
(define (decrease-speed cow)
  (cond [(<= (cow-dx cow) 0)
         (make-cow (cow-x cow)
                   (cow-y cow)
                   0
                   0
                   (cow-laps cow))]
        [else
         (make-cow (cow-x cow)
                   (cow-y cow)
                   (- (cow-dx cow) 1)
                   (cow-angle cow)
                   (cow-laps cow))]))










(check-expect (new-angle COW1) initial-angle)
(check-expect (new-angle COW2) initial-angle)
(check-expect (new-angle COW3) (cow-angle COW3))
(define (new-angle cow)
  (cond [(= (cow-angle cow) 0) initial-angle]
                         [else (cow-angle cow)]))












(check-expect (handle-key COW1 "a") (make-cow 20 initial-y 0 0 2))
(check-expect (handle-key COW2 "a") (make-cow 40 80 (- 13 1) initial-angle initial-laps))
(check-expect (handle-key COW1 "s") (make-cow 20 initial-y (+ 1 0) initial-angle 2))
(check-expect (handle-key COW3 "s") (make-cow WIDTH 150 (+ 5 1) -5 4))
(check-expect (handle-key COW3 "q") (make-cow (cow-x COW3) (cow-y COW3) (cow-dx COW3) (cow-angle COW3) (cow-laps COW3)))
(check-expect (handle-key COW3 "b") (make-cow (cow-x COW3) (cow-y COW3) (cow-dx COW3) (cow-angle COW3) (cow-laps COW3)))
(define (handle-key cow ke)
  (cond [(key=? ke "s")
         (make-cow (cow-x cow)
                   (cow-y cow)
                   (+ 1 (cow-dx cow))
                   (new-angle cow)
                   (cow-laps cow))]
        [(key=? ke "a")
         (decrease-speed cow)]
        [else
         (make-cow (cow-x cow)
                   (cow-y cow)
                   (cow-dx cow)
                   (cow-angle cow)
                   (cow-laps cow))]))










(check-expect (click-cow COW1 40 100 "button-down") (make-cow 40 100 0 0 2))
(check-expect (click-cow COW2 40 100 "button-down") (make-cow 40 100 13 initial-angle initial-laps))
(check-expect (click-cow COW3 80 150 "button-down") (make-cow 80 150 (cow-dx COW3) (cow-angle COW3) (cow-laps COW3)))
(check-expect (click-cow COW3 80 150 "button-up") (make-cow (cow-x COW3) (cow-y COW3) (cow-dx COW3) (cow-angle COW3) (cow-laps COW3)))
(define (click-cow cow x y me)
  (cond [(mouse=? me "button-down")
         (make-cow x
                   y
                   (cow-dx cow)
                   (cow-angle cow)
                   (cow-laps cow))]
        [else
         (make-cow (cow-x cow)
                   (cow-y cow)
                   (cow-dx cow)
                   (cow-angle cow)
                   (cow-laps cow))]))






(check-expect (count-laps COW1) (cow-laps COW1))
(check-expect (count-laps COW2) (cow-laps COW2))
(check-expect (count-laps COW3) (+ 1 (cow-laps COW3)))
(define (count-laps cow)
  (cond [(>= (cow-x cow) WIDTH)
         (+ 1 (cow-laps cow))]
        [else
         (cow-laps cow)]))








(check-expect (new-x COW1) (+ (cow-dx COW1) (cow-x COW1)))
(check-expect (new-x COW2) (+ (cow-dx COW2) (cow-x COW2)))
(check-expect (new-x COW3) initial-x)
(define (new-x cow)
  (cond [(>= (cow-x cow) WIDTH)
         initial-x]
        [else
         (+ (cow-dx cow) (cow-x cow))]))












(check-expect (move-cow COW1) (make-cow (+ 20 (cow-dx COW1)) initial-y 0 (* -1 0) 2))
(check-expect (move-cow COW2) (make-cow (+ 40 (cow-dx COW2)) 80 13 (* -1 initial-angle) initial-laps))
(check-expect (move-cow COW3) (make-cow initial-x (cow-y COW3) (cow-dx COW3) (* -1 (cow-angle COW3)) (+ 1 (cow-laps COW3))))
(define (move-cow cow)
  (make-cow (new-x cow)
            (cow-y cow)
            (cow-dx cow)
            (* -1 (cow-angle cow))
            (count-laps cow)))











(check-expect (get-distance-text COW1) "Distance: 2 laps")
(check-expect (get-distance-text COW2) "Distance: 0 laps")
(check-expect (get-distance-text COW3) (string-append "Distance: " (number->string (cow-laps COW3)) " laps"))
(define (get-distance-text cow)
  (string-append "Distance: "
                 (number->string (cow-laps cow))
                 " laps"))





(check-expect (get-speed-text COW1) "Speed: 0 pixels/tick")
(check-expect (get-speed-text COW2) "Speed: 13 pixels/tick")
(check-expect (get-speed-text COW3) (string-append "Speed: " (number->string (cow-dx COW3)) " pixels/tick"))
(define (get-speed-text cow)
  (string-append "Speed: "
                 (number->string (cow-dx cow))
                 " pixels/tick"))






(check-expect (get-text COW1) "Distance: 2 laps\nSpeed: 0 pixels/tick")
(check-expect (get-text COW2) "Distance: 0 laps\nSpeed: 13 pixels/tick")
(check-expect (get-text COW3) "Distance: 4 laps\nSpeed: 5 pixels/tick")
(define (get-text cow)
  (string-append (get-distance-text cow)
                 "\n"
                 (get-speed-text cow)))












(check-expect (draw-cow COW1)
              (place-image
               (text "Distance: 2 laps\nSpeed: 0 pixels/tick" text-size text-color)
               TEXT-X
               TEXT-Y
               (place-image COW (cow-x COW1) (cow-y COW1) MTS)))
(check-expect (draw-cow COW2)
              (place-image
               (text "Distance: 0 laps\nSpeed: 13 pixels/tick" text-size text-color)
               TEXT-X
               TEXT-Y
               (place-image
                (rotate (cow-angle COW2) COW)
                (cow-x COW2)
                (cow-y COW2)
                MTS)))
(check-expect (draw-cow COW3)
              (place-image
               (text "Distance: 4 laps\nSpeed: 5 pixels/tick" text-size text-color)
               TEXT-X
               TEXT-Y
               (place-image
                (rotate (cow-angle COW3) COW)
                (cow-x COW3)
                (cow-y COW3)
                MTS)))

(define (draw-cow cow)
  (if (= (cow-dx cow) 0)
      (place-image
       (text (get-text cow) text-size text-color)
       TEXT-X
       TEXT-Y
       (place-image COW (cow-x cow) (cow-y cow) MTS))
      (place-image
       (text (get-text cow) text-size text-color)
       TEXT-X
       TEXT-Y
       (place-image
        (rotate (cow-angle cow) COW)
        (cow-x cow)
        (cow-y cow)
        MTS))))






(define (main cow)
  (big-bang cow
    (on-tick move-cow)
    (to-draw draw-cow)
    (on-key handle-key)
    (on-mouse click-cow)))