

#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment2_cow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define ROT-DEG 3)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))





(define-struct cow (x y rot laps speed))

(define current-cow (make-cow 0 (/ WIDTH 2) ROT-DEG 0 1))

(define START current-cow)

(define (main cow-pen)
  (big-bang cow-pen
    (on-tick cow-tick)
    (on-key cow-key)
    (on-mouse cow-click)
    (to-draw cow-draw)
    )
  )

(define (cow-tick cow-pen)
  (if (>= (cow-x cow-pen) WIDTH)
      (reset-lap cow-pen)
      (move-cow cow-pen)
      )
  )



(define (reset-lap cow-pen)
  (make-cow 
   0 
   (cow-y cow-pen) 
   (cow-rot cow-pen) 
   (add1 (cow-laps cow-pen)) 
   (cow-speed cow-pen))
  )

(check-expect (reset-lap current-cow) (make-cow 0 (/ WIDTH 2) ROT-DEG 1 1))
(check-expect (reset-lap (make-cow 10 (/ WIDTH 2) ROT-DEG 1 1)) (make-cow 0 (/ WIDTH 2) ROT-DEG 2 1))



(define (move-cow cow-pen)
  (make-cow 
   (+ (cow-x cow-pen) (cow-speed cow-pen)) 
   (cow-y cow-pen) 
   (rotate-cow cow-pen)
   (cow-laps cow-pen) 
   (cow-speed cow-pen))
  )

(check-expect (move-cow current-cow) (make-cow 1 (/ WIDTH 2) -3 0 1))
(check-expect (move-cow (make-cow 0 (/ WIDTH 2) 3 0 6)) (make-cow 6 (/ WIDTH 2) -3 0 6))



(define (rotate-cow cow-pen)
  (if (<= (cow-speed cow-pen) 0)
    (cow-rot cow-pen)
    (* (cow-rot cow-pen) -1)
  )
)

(check-expect (rotate-cow current-cow) -3)
(check-expect (rotate-cow (make-cow 0 (/ WIDTH 2) 3 0 1)) -3)
(check-expect (rotate-cow (make-cow 0 (/ WIDTH 2) 3 0 0)) 3)

(define (cow-key cow-pen key)
  (cond
    [(key=? key "s") (make-cow 
                      (cow-x cow-pen) 
                      (cow-y cow-pen) 
                      (cow-rot cow-pen) 
                      (cow-laps cow-pen) 
                      (add1 (cow-speed cow-pen)))
                     ]
    [(key=? key "a") (make-cow 
                      (cow-x cow-pen) 
                      (cow-y cow-pen) 
                      (cow-rot cow-pen) 
                      (cow-laps cow-pen) 
                      (sub-speed (cow-speed cow-pen)))
                     ]
    [else cow-pen]))



(define (sub-speed cow-speed)
  (if (<= cow-speed 0)
      cow-speed
      (sub1 cow-speed)))

(check-expect (sub-speed 5) 4)
(check-expect (sub-speed 1) 0)
(check-expect (sub-speed 0) 0)

(define (cow-click cow-pen x-coord y-coord event)

  (cond [(mouse=? event "button-down") 
          (make-cow 
            x-coord
            y-coord
            (cow-rot cow-pen) 
            (cow-laps cow-pen) 
            (cow-speed cow-pen))
        ]
        [else cow-pen]
  )
)

(define (cow-draw cow-pen)
  (place-images
    (list (text (string-append "Speed: " (number->string (cow-speed cow-pen)) " pixels/tick\n" "Laps: " (number->string (cow-laps cow-pen))) 30 "black") (rotate (cow-rot cow-pen) COW))
    (list (make-posn (/ WIDTH 2) 40) (make-posn (cow-x cow-pen) (cow-y cow-pen)))
    MTS
  )
)