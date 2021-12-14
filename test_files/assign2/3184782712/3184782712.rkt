

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Daniel_Sophia_assignment_2_Traffic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define LIGHT-RADIUS 80) 
(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (* LIGHT-RADIUS 2)) 
(define MTS (empty-scene WIDTH HEIGHT))
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 
(define TICKS-SECOND 28) 
(define cycleLength (+ GREEN-LENGTH RED-LENGTH YELLOW-LENGTH))
(define AUTO-IMAGE1 .)
(define AUTO-IMAGE2 (scale/xy .3 .3  .))
(define AUTO-IMAGE3 (scale/xy .5 .5.))

(define TL_red (scale/xy (/ LIGHT-RADIUS 40) (/ LIGHT-RADIUS 40).))
(define TL_yellow (scale/xy (/ LIGHT-RADIUS 40) (/ LIGHT-RADIUS 40).))
(define TL_green (scale/xy (/ LIGHT-RADIUS 40) (/ LIGHT-RADIUS 40).))
(define prob 5) 
(define SPEEDMAX 30) 
(define SPEEDMIN 10) 








(define-struct auto (x y speed img))

 





 
(define-struct alltraffic (ticks loa))






(define (fn-for-alltraffic alltraffic)
  (... (alltraffic-ticks alltraffic)
       (alltraffic-loa alltraffic)
       )
  )

(define START (make-alltraffic 0 empty))

(define (main alltraffic)
  (big-bang alltraffic
    (on-tick change-states)
    (to-draw render) 
    )
  )


(define LOA1 (list (make-auto 0 0 0 AUTO-IMAGE1)))
(define LOA2 (list (make-auto 0 0 1 AUTO-IMAGE1) (make-auto 5 5 1 AUTO-IMAGE2)))
(define LOA3 (list (make-auto 1 0 1 AUTO-IMAGE1) (make-auto 5 5 1 AUTO-IMAGE2)))
(define LOA4 (list (make-auto 60 70 30 AUTO-IMAGE1) (make-auto 5 5 0 AUTO-IMAGE1)))




(define (change-states alltraffic)
  (make-alltraffic (add1 (alltraffic-ticks alltraffic)) 
                   (updateAutos alltraffic))
  )







(define (updateAutos alltraffic)
  (if (empty? (alltraffic-loa alltraffic))
      (randomAuto prob)
      (append (randomAuto prob) (moveAutos (alltraffic-loa alltraffic) (ticks->seconds (alltraffic-ticks alltraffic))))))





(define (randomAuto probability)
  (if (< (random 100) probability)
      (list (make-auto 0 (+ Y-POS (random (/ HEIGHT 2))) (+ SPEEDMIN (random SPEEDMAX)) (pick-image (random 3))))
      empty
      )
  )



(check-expect (moveAutos LOA1 0) (append (moveAuto (first LOA1) 0) (moveAutos (rest LOA1) 0 )))
(check-expect (moveAutos empty 0) empty)
(check-expect (moveAutos (list (make-auto WIDTH 0 1 AUTO-IMAGE1)) 0) empty)

(define (moveAutos loa seconds)
  (cond [(empty? loa) empty]
        [ else (append
                (moveAuto (first loa) seconds)
                (moveAutos (rest loa) seconds))]))




(check-expect (autoBehavior (make-auto 0 0 1 AUTO-IMAGE1) 0) 1)
(check-expect (autoBehavior (make-auto (/ WIDTH 1.5) 0 1 AUTO-IMAGE1) 0) 1)
(check-expect (autoBehavior (make-auto (/ WIDTH 1.5) 0 1 AUTO-IMAGE1) cycleLength) 1)
(check-expect (autoBehavior (make-auto 0 0 1 AUTO-IMAGE1) (sub1 (+ GREEN-LENGTH YELLOW-LENGTH))) 0.75)
(check-expect (autoBehavior (make-auto 0 0 1 AUTO-IMAGE1) (sub1 cycleLength)) 0.5)
(check-expect (autoBehavior (make-auto (/ WIDTH 2) 0 1 AUTO-IMAGE1) (sub1 cycleLength)) 0)

(define (autoBehavior auto seconds)
  (cond [(or (< (/ WIDTH 2) (auto-x auto))(> GREEN-LENGTH (modulo seconds cycleLength))) 
         1
         ]
        [(> (+ GREEN-LENGTH YELLOW-LENGTH) (modulo seconds cycleLength)) 
         .75
         ]
        [else  
         (if (> (+ (auto-speed auto)(auto-x auto)) (/ WIDTH 2))
             0
             .5
             )]))




(check-expect (moveAuto (make-auto 0 0 1 AUTO-IMAGE1) 0)  (list (make-auto (round (+ 0 (* 1 1))) 0 1 AUTO-IMAGE1)))
(check-expect (moveAuto (make-auto 12 0 41 AUTO-IMAGE1) (sub1 (+ GREEN-LENGTH YELLOW-LENGTH))) (list (make-auto (round (+ 12 (* 0.75 41))) 0 41 AUTO-IMAGE1)))
(check-expect (moveAuto (make-auto 12 0 10 AUTO-IMAGE1) (sub1 cycleLength)) (list (make-auto (round (+ 12 (* 0.5 10))) 0 10 AUTO-IMAGE1)))
(check-expect (moveAuto (make-auto (/ WIDTH 2) 0 41 AUTO-IMAGE1) (sub1 cycleLength)) (list (make-auto (round (+ (/ WIDTH 2) (* 0 41))) 0 41 AUTO-IMAGE1)))
(define (moveAuto auto seconds) 
  (if (< (auto-x auto) WIDTH)
      (list (make-auto (round (+ (auto-x auto) (* (autoBehavior auto seconds) (auto-speed auto)))) (auto-y auto) (auto-speed auto) (auto-img auto)))
      empty
      )
  
  )




(check-expect (render (make-alltraffic 0 LOA1))
              (place-image
               (text (string-append (number->string (ticks->seconds 0)) " seconds") 20 "Black")
               (+ X-POS (* 2.5 LIGHT-RADIUS))
               Y-POS
               (place-image (renderLight(ticks->seconds 0))
                            X-POS
                            Y-POS
                            (place-images (renderImages LOA1)
                                          (renderPosi LOA1)
                                          MTS))))
(check-expect (render (make-alltraffic 0 empty))
              (place-image
               (text (string-append (number->string 0) " seconds") 20 "Black")
               (+ X-POS (* 2.5 LIGHT-RADIUS))
               Y-POS
               (place-image (renderLight(ticks->seconds 0))
                            X-POS
                            Y-POS MTS)))

(define (render alltraffic)
  (place-image
   (text (string-append (number->string (ticks->seconds (alltraffic-ticks alltraffic))) " seconds") 20 "Black")
   (+ X-POS (* 2.5 LIGHT-RADIUS))
   Y-POS
   (place-image
    (renderLight (ticks->seconds (alltraffic-ticks alltraffic)))
    X-POS
    Y-POS
    (place-images
     (renderImages (alltraffic-loa alltraffic))
     (renderPosi (alltraffic-loa alltraffic))
     MTS
     )
    )
   )
  )







(check-expect (renderLight 0) TL_green)
(check-expect (renderLight 0) (cond [(> GREEN-LENGTH (modulo 0 cycleLength)) TL_green]
                                    [(> (+ GREEN-LENGTH YELLOW-LENGTH) (modulo 0 cycleLength)) TL_yellow]
                                    [else TL_red]
                                    )
              )
(check-expect (renderLight 5) TL_yellow)
(check-expect (renderLight 6) TL_yellow)
(check-expect (renderLight 7) TL_red)
(check-expect (renderLight 9) TL_red)
(check-expect (renderLight 9) (cond [(> GREEN-LENGTH (modulo 9 cycleLength)) TL_green]
                                    [(> (+ GREEN-LENGTH YELLOW-LENGTH) (modulo 9 cycleLength)) TL_yellow]
                                    [else TL_red]
                                    )
              )
(check-expect (renderLight 11) TL_green)
(check-expect (renderLight 16) TL_yellow)

(define (renderLight seconds)
  (cond [(> GREEN-LENGTH (modulo seconds cycleLength)) TL_green]
        [(> (+ GREEN-LENGTH YELLOW-LENGTH) (modulo seconds cycleLength)) TL_yellow]
        [else TL_red]
        )
  )





(check-expect (renderImages empty) empty)
(check-expect (renderImages LOA1) (list AUTO-IMAGE1))
(check-expect (renderImages LOA2) (list AUTO-IMAGE1 AUTO-IMAGE2))
(define (renderImages loa)
  (if (empty? loa)
      empty
      (cons (auto-img (first loa))
            (renderImages (rest loa)))
      )
  )
  




(check-expect (renderPosi empty) empty)
(check-expect (renderPosi empty) empty)
(check-expect (renderPosi LOA1) (list (make-posn 0 0)))
(check-expect (renderPosi LOA2) (list (make-posn 0 0) (make-posn 5 5)))
(define (renderPosi loa)
  (if (empty? loa)
      empty
      (cons (make-posn
             (auto-x (first loa))
             (auto-y (first loa)))
            (renderPosi (rest loa)))
      )
  )






(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)





(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)


