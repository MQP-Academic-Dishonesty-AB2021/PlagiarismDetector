

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Daniel_Sophia_Assingment_2_Cow_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 600)
(define WIDTH 800)
(define COW .)

(define MTS (empty-scene WIDTH HEIGHT))









(define-struct cow (x y speed laps rotationDirection))

 

(define (main cow)
  (big-bang cow
    (on-tick move-cow)
    (to-draw render)
    (on-mouse change-pos)
    (on-key change-speed) 
    )
  )
   

(define START (make-cow 0 (/ HEIGHT 2) 3 0 false)) 







(check-expect (move-cow (make-cow 0 0 0 0 false)) (make-cow 0 0 0 0 true))
(check-expect (move-cow (make-cow 0 0 1 0 false)) (make-cow 1 0 1 0 true))
(check-expect (move-cow (make-cow 15 10 4 7 true)) (make-cow 19 10 4 7 false))
(check-expect (move-cow (make-cow (- WIDTH 10) 4 10 0 false)) (make-cow WIDTH 4 10 0 true))
(check-expect (move-cow (make-cow  WIDTH 0 1 0 false)) (make-cow 0 0 1 1 true))


(define (move-cow cow)
  (if (>= (cow-x cow) WIDTH)
      (make-cow 0 (cow-y cow) (cow-speed cow) (+ (cow-laps cow) 1) (not (cow-rotationDirection cow)))
      (make-cow (+ (cow-x cow) (cow-speed cow)) (cow-y cow) (cow-speed cow) (cow-laps cow) (not (cow-rotationDirection cow)))
      )
  )







(define fontSize (/ HEIGHT 30))
(define Color "BLACK")
(define (render cow)
  (place-image (rotate-helper cow) (cow-x cow) (cow-y cow)
               (place-image (text (string-append "Speed: "(number->string (cow-speed cow)) " pixels/tick") fontSize "BLACK") (/ WIDTH 6) (/ HEIGHT 12)
                            (place-image (text (string-append "Laps: " (number->string (cow-laps cow))) fontSize "BLACK") (/ WIDTH 6) (/ HEIGHT 6)  MTS)
                            )
               )
  )








(check-expect (rotate-helper (make-cow 0 0 1 0 false)) (rotate 2 COW))
(check-expect (rotate-helper (make-cow 0 0 1 0 true)) (rotate -2 COW))
(check-expect (rotate-helper (make-cow 0 0 0 0 true)) (rotate 0 COW))

(define (rotate-helper cow)
  (if (= (cow-speed cow) 0)
      COW
      (if (cow-rotationDirection cow)
          (rotate -2 COW)
          (rotate 2 COW)
          )
      )
  )






(check-expect (change-pos (make-cow 0 0 0 0 true) 0 0 "button-down") (make-cow 0 0 0 0 true))
(check-expect (change-pos (make-cow 0 0 0 0 true) 2 7 "button-down") (make-cow 2 7 0 0 true))
(check-expect (change-pos (make-cow 0 0 0 0 true) 0 0 "move") (make-cow 0 0 0 0 true))
(check-expect (change-pos (make-cow 0 0 0 0 true) 0 78 "enter") (make-cow 0 0 0 0 true))

(define (change-pos cow x y mouseE )
  (if (mouse=? mouseE "button-down")
      (make-cow x y (cow-speed cow) (cow-laps cow) (cow-rotationDirection cow))
      cow
      )
  )







(check-expect (change-speed (make-cow 0 0 7 0 true) "e") (make-cow 0 0 7 0 true))
(check-expect (change-speed (make-cow 0 0 0 0 true) "a") (make-cow 0 0 0 0 true))
(check-expect (change-speed (make-cow 0 0 1 0 true) "a") (make-cow 0 0 0 0 true))
(check-expect (change-speed (make-cow 0 0 0 0 true) "s") (make-cow 0 0 1 0 true))

(define (change-speed cow keyE)
  (make-cow (cow-x cow)
            (cow-y cow)
            (+ (cow-speed cow) (speed-mod cow keyE)) 
            (cow-laps cow)
            (cow-rotationDirection cow)
            )
  )






(check-expect (speed-mod (make-cow 0 0 0 0 true) "e") 0)
(check-expect (speed-mod (make-cow 0 0 0 0 true) "a") 0)
(check-expect (speed-mod (make-cow 0 0 1 0 true) "a") -1)
(check-expect (speed-mod (make-cow 0 0 0 0 true) "s") 1)

(define (speed-mod cow keyE)
  (cond
    [(and (key=? keyE "a") (> (cow-speed cow )0)) -1]
    [(key=? keyE "s") 1]
    [else 0]
    )
  )