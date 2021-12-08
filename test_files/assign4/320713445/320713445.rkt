

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe commented|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)
(require net/sendurl)










(define SIZE 500)
(define START-DIFF 0) 
(define PEN-WIDTH 10)
(define EDGE-WIDTH 40)



(define MTS (empty-scene SIZE SIZE))
(define SQUARE-SIZE (/ (- SIZE (* EDGE-WIDTH 2)) 3))
(define TEXT-SIZE (- (* SIZE .345) 55.5))







(define-struct WorldState (board look wait stop text ads))







(define START (make-WorldState (make-list 9 0) START-DIFF 0 #f empty empty))


(define (main ws)
  (big-bang ws
    (on-tick tick)
    (to-draw draw)
    (on-key key)
    (on-mouse mouse)))

(define WIN-COND (list
                  (list 0 1 2)
                  (list 3 4 5)
                  (list 6 7 8)
                  (list 0 3 6)
                  (list 1 4 7)
                  (list 2 5 8)
                  (list 0 4 8)
                  (list 6 4 2)))

(define GAME-END (list "You Won" "You Lost" "You Tie"))


(define (tick ws)
  (local [(define (AI-move board diff)
            (local [(define (find-open _) (filter (λ (n) (zero? (list-ref board n))) (build-list 9 identity)))]
              (list-set2 board (if (= diff 0) (list-ref (find-open 0) (random (length (find-open 0)))) (minimax board diff)) 2)))]
  
    (let ([win (check-win (WorldState-board ws))]
          [txt (NiceText-ticks (WorldState-text ws))])
      (make-WorldState (if (and (= (WorldState-wait ws) 1) (zero? win) (not (WorldState-stop ws))) (AI-move (WorldState-board ws) (WorldState-look ws))
                           (WorldState-board ws))
                       (WorldState-look ws) (- (WorldState-wait ws) (sign (WorldState-wait ws)))
                       (positive? win) (if (and (positive? win) (not (WorldState-stop ws)))
                                           (cons (static-NiceText (list-ref GAME-END (sub1 win)) (/ SIZE 2) (/ SIZE 2)) txt) txt) (WorldState-ads ws)))))


(define (draw ws)
  (local
    [(define (draw-board img) (local [(define (get-board-pos _) (build-list 4 (λ (n)
                                                                                (let ([x-pos (+ EDGE-WIDTH (* SQUARE-SIZE (+ (modulo n 2) 1)))])
                                                                                  (run-if reverse (list x-pos EDGE-WIDTH x-pos (- SIZE EDGE-WIDTH)) (> n 1))))))
                                      (define pen (make-pen "goldenrod" PEN-WIDTH "solid" "round" "round"))]
                                (foldl (λ (el img) (apply (λ(a b c d) (add-line img a b c d pen)) el)) img (get-board-pos 0))))
     (define (draw-XO ws img) (foldl (λ (el img) (place-image (text (list-ref (list "" "X" "O") (second el)) TEXT-SIZE "red")
                                                              (+ EDGE-WIDTH (* (+ (modulo (first el) 3) .5) SQUARE-SIZE)) (+ EDGE-WIDTH
                                                                                                                             (* (+ (quotient (first el) 3) .5) SQUARE-SIZE)) img))
                                     img
                                     (build-list 9 (λ (n) (list n (list-ref (WorldState-board ws) n))))))]

    (NiceText-draws (WorldState-text ws) (draw-button (draw-board (draw-XO ws MTS))))))




(define (key ws ke) (let ([try (string->number ke)])
                      (if (false? try) ws (make-WorldState (WorldState-board ws) try
                                                           (WorldState-wait ws) (WorldState-stop ws) (WorldState-text ws) (WorldState-ads ws)))))

(check-expect (key START "6") (make-WorldState (WorldState-board START) 6 0 #f (WorldState-text START) (WorldState-ads START))) 
(check-expect (key START "9") (make-WorldState (WorldState-board START) 9 0 #f (WorldState-text START) (WorldState-ads START))) 
(check-expect (key START "u") (make-WorldState (WorldState-board START) START-DIFF 0 #f (WorldState-text START) (WorldState-ads START))) 
(check-expect (key START "") (make-WorldState (WorldState-board START) START-DIFF 0 #f (WorldState-text START) (WorldState-ads START))) 




(define (mouse ws x y m)
  (if (mouse=? m "button-down")
      (local
        [(define (mouse-to-board pos) (floor (/ (- (floor pos) EDGE-WIDTH -1) SQUARE-SIZE)))
         (define xpos (mouse-to-board x))
         (define pos (+ (* (mouse-to-board y) 3) (if (and (>= xpos 0) (< xpos 3)) xpos 69)))
         (define change (and (zero? (WorldState-wait ws)) (not (WorldState-stop ws)) (>= pos 0) (< pos 9) (zero? (list-ref (WorldState-board ws) pos))))
         (define (new-move _)
           (if change (cons
                       (rand-NiceText (list-ref move-text (random (length move-text))) SIZE SIZE)
                       (WorldState-text ws)) (WorldState-text ws)))
         (define (set-board ws y x)
           (if change
               (list-set2 (WorldState-board ws) pos 1)
               (WorldState-board ws)))]
        (make-WorldState (set-board ws x y) (WorldState-look ws) (if change 2 (WorldState-wait ws)) (WorldState-stop ws) (click-button (new-move 0) x y) (WorldState-ads ws)))
      ws))
                             





















(define (minimax board diff)
  
  
  (local [(define-struct move (index board))
          (define-struct result (index avg))
          (define (to-sign res) (if (< res 2) (- res) (- 3 res))) 
          (define (get-moves board turn) 
            (map (λ (i) (make-move i (list-set2 board i (if turn 2 1))))
                 (filter (λ (i) (zero? (list-ref board i)))
                         (build-list 9 identity))))
          (define (find move diff turn) 
            (let ([try  (check-win (move-board move))]) 
              (if (or (positive? try) (zero? diff)) (make-result (move-index move) (to-sign try)) 
                  (letrec
                      ([results (map (λ (n) (find n (sub1 diff) (not turn))) (get-moves board turn))] 
                       [sorted (quicksort results (λ (a b) ((if turn > <) (result-avg a) (result-avg b))))]
                       [poss (filter (λ (n) (zero? (result-avg n))) sorted)])
                    (if (zero? (result-avg (first sorted))) (list-ref poss (random (length poss))) (first sorted))))))]
    (result-index (find (make-move 0 board) diff #t))))


(check-expect (minimax (list 2 2 0 0 0 0 0 0 0) 1) 2)
(check-expect (minimax (list 2 0 0 2 0 0 0 0 0) 1) 6)
(check-expect (minimax (list 0 0 2 0 2 0 0 0 0) 1) 6)

(check-expect (minimax (list 1 1 0 0 0 0 0 0 0) 2) 2)
(check-expect (minimax (list 1 0 0 1 0 0 0 0 0) 2) 6)
(check-expect (minimax (list 0 0 1 0 1 0 0 0 0) 2) 6)

(check-expect (minimax (list 1 1 0 2 2 0 0 0 0) 2) 5)








(define (check-win board)
  (local [(define (check n) (if (= (list-ref board (first n)) (list-ref board (second n)) (list-ref board (third n))) (list-ref board (first n)) 0))
          (define winner (filter positive? (map check WIN-COND)))]
    (if (empty? winner) (if (= (length (filter positive? board)) 9) 3 0) (first winner))))




(define (list-set2 lox pos val) (append (take lox pos) (list val)  (drop lox (add1 pos))))
(check-expect (list-set2 (list 0 1 2) 0 15) (list 15 1 2))
(check-expect (list-set2 (list 0 1 2) 1 13) (list 0 13 2))
(check-expect (list-set2 (list 0 1 2) 2 15) (list 0 1 15))








(define (rand min max) (+ min (* (random 1000) 1/1000 (- max min))))




(define (rand-sign num) (* (- (* (random 2) 2) 1) num))





(define (string-split str char) (foldr (λ (str los)
                                         (if (char=? str char) (cons "" los)
                                             (cons (string-append (string str) (first los)) (rest los))))
                                       (list "") (string->list str)))






(define (sign num) (cond
                     [(positive? num) 1]
                     [(negative? num) -1]
                     [else 0]))



(define (run-if fn value bool) (if bool (fn value) value))













(define SHOW-NICE-TEXT #t)
(define TEXT-CLEARANCE 0)
(define NICETEXT-SIZE (* SIZE .12))



(define-struct NiceText (text x y rot d-rot hue d-hue size d-size fade d-fade))
















(define (rand-NiceText txt width height)
  (local [(define (get-side side sincos img nt) (* (side img) (abs (sincos (* (NiceText-rot nt) pi 1/180))) .5))
          (define (find-maxes nt xmax ymax) (let
                                                ([img (text (NiceText-text nt) (NiceText-size nt) 'black)])
                                              (if (positive? (NiceText-fade nt))
                                                  (find-maxes (NiceText-tick nt)
                                                              (max xmax (+ (get-side image-width cos img nt) (get-side image-height sin img nt)))
                                                              (max ymax (+ (get-side image-width sin img nt) (get-side image-height cos img nt))))
                                                  (list (+ xmax TEXT-CLEARANCE) (+ ymax TEXT-CLEARANCE)))))
          (define test (rand-pos-NiceText txt 0 0))
          (define maxes (find-maxes test 0 0))]
    (make-NiceText (NiceText-text test) (rand (first maxes) (- width (first maxes)))
                   (rand (second maxes) (- height (second maxes)))
                   (NiceText-rot test) (NiceText-d-rot test) (NiceText-hue test)
                   (NiceText-d-hue test) (NiceText-size test) (NiceText-d-size test) 
                   (NiceText-fade test) (NiceText-d-fade test))))




(define (rand-pos-NiceText text xpos ypos) (make-NiceText text xpos ypos (rand 270 450)
                                                          (rand-sign (rand .1 1)) (random 360) (rand 1 5) NICETEXT-SIZE 1 100 -5))

(define (static-NiceText text xpos ypos) (make-NiceText text xpos ypos 0 0 (random 360) 25 NICETEXT-SIZE 0 100 0))
  


  


(define (NiceText-draws lont img) (if SHOW-NICE-TEXT (foldl NiceText-draw img lont) img))




(define (NiceText-ticks lont) (filter (λ (nt) (positive? (NiceText-fade nt))) (map NiceText-tick lont)))
  


(define (NiceText-draw nt img) (place-image
                                (rotate (NiceText-rot nt) (text (NiceText-text nt) (NiceText-size nt)
                                                                (hue-to-color (NiceText-hue nt) (NiceText-fade nt))))
                                (NiceText-x nt) (NiceText-y nt) img))



(define (NiceText-tick nt) (make-NiceText (NiceText-text nt) (NiceText-x nt) (NiceText-y nt)
                                          (+ (NiceText-rot nt) (NiceText-d-rot nt)) (NiceText-d-rot nt)
                                          (+ (NiceText-hue nt) (NiceText-d-hue nt)) (NiceText-d-hue nt)
                                          (+ (NiceText-size nt) (NiceText-d-size nt)) (NiceText-d-size nt)
                                          (+ (NiceText-fade nt) (NiceText-d-fade nt)) (NiceText-d-fade nt)))


(define (draw-button img) (if SHOW-NICE-TEXT 
                              (place-image (overlay BUTTON (rectangle (image-width BUTTON) (/ EDGE-WIDTH 2) 'solid 'red))
                                           (/ (image-width BUTTON) 2) (- SIZE (/ EDGE-WIDTH 4))
                                           (place-image (square (/ EDGE-WIDTH 2) 'solid 'red) (- SIZE (/ EDGE-WIDTH 4)) (- SIZE (/ EDGE-WIDTH 4)) img)) img))



(define (click-button txt x y)
  (if (and (< x (image-width BUTTON)) (> y (- SIZE (/ EDGE-WIDTH 2))))
      (cons (rand-NiceText (list-ref encourage-text (random (length encourage-text))) SIZE SIZE) txt)
      (if (and (> x (- SIZE (/ EDGE-WIDTH 2))) (> y (- SIZE (/ EDGE-WIDTH 2)))
               (false? (send-url mystery))) 0 txt)))

(define BUTTON (text "Encouragement Button" (/ EDGE-WIDTH 2) 'black))






(define (name-to-color name)
  (first (image->color-list (square 1 "solid" name))))
(check-expect (name-to-color 'red) (make-color 255 0 0))
(check-expect (name-to-color 'blue) (make-color 0 0 255))
(check-expect (name-to-color 'goldenrod) (make-color 218 165 32))














































(define (hue-to-color hue fade)
  (local [(define (to-rgb hue) (letrec
                                   ([u (+ (/ hue 360) 1)]
                                    [h (- u (floor u))])
                                 (ceiling (* 255
                                             (cond
                                               [(< (* h 6) 1) (* h 6)]
                                               [(< (* h 2) 1) 1]
                                               [(< (* h 3) 2) (* (- 2/3 h) 6)]
                                               [else 0])))))]
    (make-color (to-rgb (+ hue 120))
                (to-rgb hue)
                (to-rgb (- hue 120))
                (floor (* fade 2.55)))))


(check-expect (hue-to-color 0 100) (name-to-color 'red))
(check-expect (hue-to-color 120 100) (name-to-color 'green))
(check-expect (hue-to-color 240 100) (name-to-color 'blue))


(check-expect (hue-to-color 50 100) (make-color 255 213 0))
(check-expect (hue-to-color 100 100) (make-color 85 255 0))
(check-expect (hue-to-color 150 100) (make-color 0 255 128))
(check-expect (hue-to-color 200 100) (make-color 0 170 255))
(check-expect (hue-to-color 250 100) (make-color 43 0 255))
(check-expect (hue-to-color 300 100) (make-color 255 0 255))
(check-expect (hue-to-color 350 100) (make-color 255 0 43))
(check-expect (hue-to-color 400 100) (make-color 255 170 0))
(check-expect (hue-to-color 450 100) (make-color 128 255 0))
(check-expect (hue-to-color 500 100) (make-color 0 255 85))


(check-expect (hue-to-color 500 100) (make-color 0 255 85))



                                    
(define move-text (string-split "Nice Move!|Great Move!|Wow!|Amazing!|Dominating!|Superb!" #\|))
(define encourage-text (string-split "Nice Job!|Doing Great!|Wow!|Amazing!" #\|))
(define mystery "http://youtu.be/HXcSGuYUkDg")


