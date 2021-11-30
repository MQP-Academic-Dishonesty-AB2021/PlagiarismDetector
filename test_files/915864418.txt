

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment_4_revised) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 899) 
(define MTS (empty-scene SIZE SIZE))
(define LINE-THICKNESS (quotient SIZE 20))
(define PADDING (quotient SIZE 20)) 
(define PEN (make-pen "goldenrod" LINE-THICKNESS "solid" "round" "round")) 
(define BOARD-IMG
  (add-line (add-line (add-line (add-line MTS
                                          PADDING (/ SIZE 3)
                                          (- SIZE PADDING) (/ SIZE 3) PEN) 
                                PADDING (* 2 (/ SIZE 3))
                                (- SIZE PADDING) (* 2 (/ SIZE 3)) PEN) 
                      (* 2 (/ SIZE 3)) PADDING (* 2 (/ SIZE 3))
                      (- SIZE PADDING) PEN) 
            (/ SIZE 3) PADDING (/ SIZE 3) (- SIZE PADDING) PEN) 
  )
(define TEXT-SIZE (quotient SIZE 4))
(define X (text "X" TEXT-SIZE "red"))
(define O (text "O" TEXT-SIZE "blue"))
(define PLAYER-PIECE 1)
(define AI-PIECE 2)
(define WIN-CONDITIONS 
  (list
   (list 0 1 2)
   (list 3 4 5)
   (list 6 7 8)
   (list 0 3 6)
   (list 1 4 7)
   (list 2 5 8)
   (list 0 4 8)
   (list 2 4 6)
   )
)

(define EMPTY-BOARD (list 0 0 0 0 0 0 0 0 0))
(define BOARD1 (list 1 1 1 0 0 0 0 0 0))
(define BOARD2 (list 2 0 0 0 2 0 0 0 2))
(define BOARD3 (list 1 1 2 2 2 1 1 2 1))
(define BOARD4 (list 2 0 0 0 1 0 0 0 0))














(define-struct ws (board difficulty winner enemy-turn?))

(define START (make-ws EMPTY-BOARD 4 0 false))



(define (main ws)
  (big-bang ws
    (on-tick tick)
    (on-key key-press)
    (on-mouse mouse)
    (to-draw draw)
    ))




(define (tick ws)
  (if (and (ws-enemy-turn? ws) (= 0 (ws-winner ws)))
      (check-for-winner (make-ws
                     (new-board (ws-board ws) (enemy-move ws) AI-PIECE)
                     (ws-difficulty ws)
                     0
                     false))
      ws)
)

(check-expect (tick (make-ws EMPTY-BOARD 0 0 false)) (make-ws EMPTY-BOARD 0 0 false))
(check-satisfied (tick (make-ws EMPTY-BOARD 0 0 true))
                 (λ (ws) (= (length (filter positive? (ws-board ws))) 1)))
(check-expect (tick (make-ws BOARD1 0 1 true)) (make-ws BOARD1 0 1 true))





(define (enemy-move ws)
  (local [
          (define (next-turn turn)
            (if (= turn 1) 2 1)
          )
          
          
          
          
          (define-struct board-tree (win-state move children))
          
          
          (define-struct board-move-pair (board move))
          
          (define (generate-next-moves board turn)
            (map (λ (n)
                   (make-board-move-pair (new-board board n turn) n))
                 (filter (λ (n) (valid-move? n board))
                         (build-list 9 identity)))
          )
          
          (define (generate-game-tree--board board move turn depth)
            (local [(define winner (get-winner board))]
            [make-board-tree winner move
                             (if (and (= winner 0) (> depth 0)) 
                                 (generate-game-tree--lob
                                  (generate-next-moves board (next-turn turn))
                                  (next-turn turn) (- depth 1))
                                 empty)
                             ])
          )
          
          (define (generate-game-tree--lob lob turn depth)
            (map (λ (bmpair) (generate-game-tree--board (board-move-pair-board bmpair)
                                                        (board-move-pair-move bmpair)
                                                        turn depth)) lob)
          )
          
          (define (utility value)
            (cond [(= value 0) 0]
                  [(= value 1) -1]
                  [(= value 2) 1]
                  [(= value 3) 0])
          )
          
          (define (value-of--bt bt turn)
            (if (empty? (board-tree-children bt))
                (utility (board-tree-win-state bt))
                (value-of--lobt (board-tree-children bt) (next-turn turn))
            )
          )
          
          (define (value-of--lobt lobt turn)
            (local [(define fn (if (= turn 1) max min))]
              [
               apply fn (map (λ (bt) (value-of--bt bt turn)) lobt)
               ]
            )
          )

          (define (random-valid-move board)
            (local [(define valid-moves (filter (λ (n) (valid-move? n board))
                                                (build-list 9 identity)))]
              [list-ref valid-moves (random (length valid-moves))])
          )
          
          (define (choose-move lobt)
            (if (empty? lobt)
                (random-valid-move (ws-board ws))
                (board-tree-move (argmax (λ (bt) 
                                           (max (value-of--bt bt 1))
                                           )
                                         (shuffle lobt)))
                
                
                )
          )
          ]
          
    [choose-move (board-tree-children (generate-game-tree--board
                                       (ws-board ws) -1 1 (ws-difficulty ws)))])
)
(check-expect (enemy-move (make-ws (list 1 2 1 1 1 2 2 2 0) 0 0 true)) 8) 
(check-expect (enemy-move (make-ws (list 2 0 0 0 2 0 0 0 0) 1 0 true)) 8) 
(check-expect (enemy-move (make-ws (list 1 0 0 1 0 0 0 0 0) 2 0 true)) 6) 
(check-expect (enemy-move (make-ws (list 1 1 0 0 2 0 0 2 0) 6 0 true)) 2) 



(check-expect (enemy-move (make-ws (list 1 0 0 0 0 0 0 0 0) 9 0 true)) 4) 




(define (key-press ws key)
  (local [(define keynum (string->number key))]
    (cond [(number? keynum)
           (make-ws (ws-board ws) keynum (ws-winner ws) (ws-enemy-turn? ws))]
          [(string=? key "rshift") 
           (make-ws EMPTY-BOARD (ws-difficulty ws) 0 false)]
          [else
           ws])
  )
)

(check-expect (key-press (make-ws EMPTY-BOARD 2 0 false) "0") (make-ws EMPTY-BOARD 0 0 false))
(check-expect (key-press (make-ws BOARD1 3 0 false) "1") (make-ws BOARD1 1 0 false))
(check-expect (key-press (make-ws BOARD3 3 0 false) "3") (make-ws BOARD3 3 0 false))





(define (mouse ws x y event)
  (if (and (equal? event "button-down") (= (ws-winner ws) 0))
      (local [(define move 
                (local [(define pos (+ (quotient x (quotient SIZE 3))
                                       (* 3 (quotient y (quotient SIZE 3)))))]
                  [if (valid-move? pos (ws-board ws))
                      pos
                      false])
                )]
        [if (false? move)
            ws
            (check-for-winner (make-ws (new-board (ws-board ws) move PLAYER-PIECE)
                                       (ws-difficulty ws) 0 true))])
      ws)
  )
(check-expect (mouse (make-ws EMPTY-BOARD 0 0 false) 0 0 "not button down") 
              (make-ws EMPTY-BOARD 0 0 false))
(check-expect (mouse (make-ws EMPTY-BOARD 0 0 false) 
                     (quotient SIZE 2) (quotient SIZE 2) "button-down")
              (make-ws (list 0 0 0 0 1 0 0 0 0) 0 0 true))
(check-expect (mouse (make-ws (list 1 1 0 2 2 0 0 0 0) 0 0 false)
                     (* 5 (quotient SIZE 6)) (quotient SIZE 6) "button-down")
              (make-ws (list 1 1 1 2 2 0 0 0 0) 0 1 true))





(define (valid-move? move board)
  (= 0 (list-ref board move))
)
(check-expect (valid-move? 2 EMPTY-BOARD) true)
(check-expect (valid-move? 0 BOARD1) false)
(check-expect (valid-move? 5 BOARD3) false)
(check-expect (valid-move? 3 BOARD4) true)






(define (new-board board move piece)
  (list-set board move piece)
  )
(check-expect (new-board EMPTY-BOARD 4 PLAYER-PIECE) (list 0 0 0 0 PLAYER-PIECE 0 0 0 0))
(check-expect (new-board BOARD1 2 0) (list 1 1 0 0 0 0 0 0 0))
(check-expect (new-board BOARD4 8 PLAYER-PIECE) (list 2 0 0 0 1 0 0 0 PLAYER-PIECE))





(define (check-for-winner ws)
  (make-ws (ws-board ws) (ws-difficulty ws) (get-winner (ws-board ws)) (ws-enemy-turn? ws))
)

(check-expect (check-for-winner (make-ws EMPTY-BOARD 0 0 false)) (make-ws EMPTY-BOARD 0 0 false))
(check-expect (check-for-winner (make-ws BOARD1 0 0 false)) (make-ws BOARD1 0 1 false))
(check-expect (check-for-winner (make-ws BOARD2 0 0 false)) (make-ws BOARD2 0 2 false))
(check-expect (check-for-winner (make-ws BOARD3 0 0 false)) (make-ws BOARD3 0 3 false))
(check-expect (check-for-winner (make-ws BOARD4 0 0 false)) (make-ws BOARD4 0 0 false))





(define (get-winner board)
  (local [
          (define (win? winner b)
            (ormap (λ (lon)
                     (andmap (λ (n)
                        (= (list-ref b n) winner)) lon)
                     ) WIN-CONDITIONS)
          )
          (define (board-full? b)
            (andmap positive? b) 
          )
          ]
    [cond [(win? 1 board) 1]
          [(win? 2 board) 2]
          [(board-full? board) 3]
          [else 0]
    ])
)
(check-expect (get-winner EMPTY-BOARD) 0)
(check-expect (get-winner BOARD1) 1)
(check-expect (get-winner BOARD2) 2)
(check-expect (get-winner BOARD3) 3) 
(check-expect (get-winner BOARD4) 0)




(define (draw ws)
  (local [(define (get-piece piece)
            (if (= 1 piece) X O)
            )
          
          
          
          (define (render-pieces board)
            (local [(define (piece-x i) (+ (/ SIZE 6) (* (/ SIZE 3) (modulo i 3))))
                    (define (piece-y i) (+ (/ SIZE 6) (* (/ SIZE 3) (quotient i 3))))
                    (define (render-piece i piece img) 
                      (place-image
                       (get-piece piece)
                       (piece-x i)
                       (piece-y i)
                       img))
                    (define (render-pieces--inner board i)
                      (if (empty? board)
                          BOARD-IMG
                          (if (= 0 (first board))
                              (render-pieces--inner (rest board) (+ i 1))
                              (render-piece i (first board)
                                            (render-pieces--inner (rest board) (+ i 1))))
                          )
                      )]
              [render-pieces--inner board 0])
            )

          
          
          
          (define (render-win winner img)
            (if (= winner 0)
                img
                (place-image (text (cond [(= winner 1) "X Wins"]
                                         [(= winner 2) "O Wins"]
                                         [else "It's a tie"])
                                   TEXT-SIZE "green")
                             (/ SIZE 2) (/ SIZE 2)
                             img
                             ))
            )]
          (render-win (ws-winner ws) (render-pieces (ws-board ws)))))

(check-expect (draw START) BOARD-IMG)

(check-expect (draw (make-ws BOARD1 0 0 false)) 
                    (place-image X (/ SIZE 6) (/ SIZE 6)
                                 (place-image X (/ (* 3 SIZE) 6) (/ SIZE 6)
                                     (place-image X (/ (* 5 SIZE) 6) (/ SIZE 6)
                                           BOARD-IMG))))
(check-expect (draw (make-ws BOARD2 0 0 false))
              (place-image O (/ SIZE 6) (/ SIZE 6)
                           (place-image O (/ (* SIZE 3) 6) (/ (* SIZE 3) 6)
                           (place-image O (/ (* 5 SIZE) 6) (/ (* SIZE 5) 6)
                                        BOARD-IMG))))

(check-expect (draw (make-ws BOARD1 0 1 false))
                    (place-image (text "X Wins"
                                   TEXT-SIZE "green") (/ SIZE 2) (/ SIZE 2)
                                 (place-image X (/ SIZE 6) (/ SIZE 6)
                                              (place-image X (/ (* 3 SIZE) 6) (/ SIZE 6)
                                                           (place-image X (/ (* 5 SIZE) 6)
                                                                        (/ SIZE 6)
                                                                        BOARD-IMG)))))