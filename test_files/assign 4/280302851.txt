

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Tic_Tac_Toe_Assignment4_final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))
(define SPACE-SIZE (round (/ SIZE 3)))
(define BLANK (square SPACE-SIZE "outline" "white"))
(define X (text "X" SPACE-SIZE "red"))
(define O (text "O" SPACE-SIZE "blue"))
(define PENDEFAULT (make-pen "black" (* SIZE .05) "solid" "round" "round"))
(define GAMEBOARD (add-line
                   (add-line
                    (add-line
                     (add-line MTS 0 (* SIZE .65) SIZE (* SIZE .65) PENDEFAULT)
                     0 (* SIZE .30) SIZE (* SIZE .30) PENDEFAULT)
                    (* SIZE .65) 0 (* SIZE .65) SIZE PENDEFAULT)
                   (* SIZE .30) 0 (* SIZE .30) SIZE PENDEFAULT))






(define-struct Game (boxes turn diff))








(define (main game)
  (big-bang game
    (on-tick run-ai)
    (to-draw render)
    (on-mouse run-player)
    (on-key change-diff)))


(define START (make-Game (list BLANK BLANK BLANK
                               BLANK BLANK BLANK
                               BLANK BLANK BLANK) X 1))

(define GAME1 (make-Game (list X O X
                               BLANK BLANK BLANK
                               BLANK O X) O 1))

(define GAME2 (make-Game (list X O X
                               BLANK X BLANK
                               BLANK O BLANK) O 1))

(define GAME3 (make-Game (list X X O
                               BLANK X X
                               X O O) O 1))




(check-expect (change-diff START "8")
              (make-Game (list BLANK BLANK BLANK
                               BLANK BLANK BLANK
                               BLANK BLANK BLANK) X 8))
(check-expect (change-diff START "0")
              START)
(check-expect (change-diff START "9")
              START)

(define (change-diff game ke)
  (if (or (> (string->number ke) 8)
          (< (string->number ke) 1))
      game    
      (make-Game (Game-boxes game) (Game-turn game) (string->number ke))))








 












(define (run-ai game)
  (if (and (image=? (Game-turn game) O)
           (not (check-win? (Game-boxes game) X))
           (not (empty? (indexes-of (Game-boxes game) BLANK))))
      
      (local [(define next-vals (map (lambda (boxes)
                                       (eval-boards (Game-diff game) boxes))
                                     (next-boards (Game-boxes game) O)))
              (define max-val (foldr max (first next-vals) (rest next-vals)))]
        (make-Game (first (shuffle (filter (lambda (boxes)
                                             (= max-val
                                                (eval-boards (Game-diff game)
                                                             boxes)))
                                           (next-boards
                                            (Game-boxes game)
                                            O)))) X (Game-diff game)))
      game))







(check-expect (update-game-square GAME3 3 O) (list X X O
                                                   O X X
                                                   X O O))
(check-expect (update-game-square START 1 X)
              (list BLANK X BLANK
                    BLANK BLANK BLANK
                    BLANK BLANK BLANK))

(define (update-game-square game pos player)
  (list-set (Game-boxes game) pos player))




(check-expect (draw-turns (list X O BLANK
                               BLANK BLANK BLANK
                               BLANK BLANK BLANK) GAMEBOARD)
              (place-image X (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo 0 3)))
                           (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ 0 3))))
              (place-image O (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo 1 3)))
                           (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ 1 3))))
              (place-image BLANK (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo 2 3)))
                           (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ 2 3))))
              (place-image BLANK (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo 3 3)))
                           (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ 3 3))))
              (place-image BLANK (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo 4 3)))
                           (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ 4 3))))
              (place-image BLANK (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo 5 3)))
                           (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ 5 3))))
              (place-image BLANK (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo 6 3)))
                           (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ 6 3))))
              (place-image BLANK (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo 7 3)))
                           (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ 7 3))))
              (place-image BLANK (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo 8 3)))
                           (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ 8 3)))) GAMEBOARD))))))))))


(define (draw-turns loi scene)
  (local [(define (img-x turn)
            (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (modulo turn 3))))
          (define (img-y turn)
            (+ (/ SPACE-SIZE 2) (* SPACE-SIZE (floor (/ turn 3))))
            )
          (define (place-turns loi index)
            (cond [(empty? loi) scene]                   
                  [else (place-image (first loi) (img-x index) (img-y index)                
                                     (place-turns (rest loi) (add1 index) ))]))]
    (place-turns loi 0)))





(check-expect (check-win? (list BLANK X BLANK
                                BLANK X BLANK
                                BLANK X BLANK) X) true)
(check-expect (check-win? (list BLANK X BLANK
                                BLANK X BLANK
                                BLANK X BLANK) O) false)
(check-expect (check-win? (list X X X
                                BLANK BLANK BLANK
                                BLANK BLANK BLANK) X) true)
(check-expect (check-win? (list X BLANK BLANK
                                BLANK X BLANK
                                BLANK BLANK X) X) true)
(check-expect (check-win? (list BLANK O BLANK
                                BLANK O BLANK
                                BLANK O BLANK) O) true)
(check-expect (check-win? (list BLANK O BLANK
                                BLANK O BLANK
                                BLANK O BLANK) X) false)
(check-expect (check-win? (list O O O
                                BLANK BLANK BLANK
                                BLANK BLANK BLANK) O) true)
(check-expect (check-win? (list O BLANK BLANK
                                BLANK O BLANK
                                BLANK BLANK O) O) true)
(check-expect (check-win? (list BLANK BLANK BLANK
                                BLANK BLANK BLANK
                                BLANK BLANK BLANK) O) false)
(check-expect (check-win? (list O BLANK BLANK
                                BLANK O BLANK
                                BLANK BLANK BLANK) O) false)


(define (check-win? loi plyr)
  (local [(define winReq
            (list (list 0 1 2)
                  (list 3 4 5)
                  (list 6 7 8)
                  (list 0 3 6)
                  (list 1 4 7)
                  (list 2 5 8)
                  (list 0 4 8)
                  (list 2 4 6)))
          (define (check-win loi winReq)
            (cond [(empty? winReq) false]
                  [(and (member (first (first winReq)) (indexes-of loi plyr))
                        (member (first (rest (first winReq))) (indexes-of loi plyr))
                        (member (first (rest (rest (first winReq)))) (indexes-of loi plyr)))
                   true]
                  [else
                   (check-win loi (rest winReq))]))]
    (check-win loi winReq))
          
                  
  )





(define (render game)
  (local[(define (win-draw img)
           (cond [(check-win? (Game-boxes game) X)
                  (place-image (text "Player Wins!" (/ SIZE 10) "green")
                               (/ SIZE 2) (/ SIZE 2) img)]
                 [(check-win? (Game-boxes game) O)
                  (place-image (text "Comp Wins!" (/ SIZE 10) "green")
                               (/ SIZE 2) (/ SIZE 2) img)]
                 [(not (member BLANK (Game-boxes game)))
                  (place-image (text "Draw" (/ SIZE 10) "green")
                               (/ SIZE 2) (/ SIZE 2) img)]
                 [else img]))]
    (win-draw (draw-turns (Game-boxes game) GAMEBOARD))
   
    ))





(check-expect (run-player START 0 0 "button-down")
              (make-Game (list X BLANK BLANK
                               BLANK BLANK BLANK
                               BLANK BLANK BLANK) O 1))
(check-expect (run-player START 265 265 "button-down")
              (make-Game (list BLANK BLANK BLANK
                               BLANK BLANK BLANK
                               BLANK BLANK X) O 1))

(define (run-player game x y mouse)
  (local [(define (click-pos x y)
            (cond [(string=? mouse "button-down")
                   (+ (floor (/ x (* SIZE .3)))
                      (* (floor (/ y (* SIZE .3))) 3))]
                  [else
                   game]))
          (define (place-on-click x y)
            (cond [(string=? mouse "button-down")
                   (make-Game
                    (update-game-square game (click-pos x y) X) O
                    (Game-diff game))]
                  [else
                   game]))]
    (place-on-click x y)))








(check-expect (eval-boards 1 (list X X O
                                 BLANK X X
                                 X O O))
              0)
(check-expect (eval-boards 2 (list X X O
                                 BLANK BLANK X
                                 X X O))
              -1)
 

(define (eval-boards d boxes)
  (cond [(= d 0) 0]
        [(check-win? boxes X) -1] 
        [(check-win? boxes O) 1]
        [(empty? (indexes-of boxes BLANK)) 0]
        [else
         (local [(define next-vals (map (lambda (boxes)
                                          (eval-boards (- d 1) boxes))
                                        (next-boards boxes X)))]
           (foldr min (first next-vals) (rest next-vals)))
         ]))
        





(check-expect (next-boards (list X X O
                                 BLANK X X
                                 X O O) O)
              (list (list X X O
                          O X X
                          X O O))) 
                            
(define (next-boards boxes turn)
  (map (lambda (idx)
         (list-set boxes idx turn))
       (indexes-of boxes BLANK)
       ))
