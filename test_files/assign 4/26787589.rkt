

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname blah) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)



(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))

(define MOVE-COLOR "RED")
(define TEXT-SIZE (round (/ SIZE 10)))
(define TEXT-COLOR "black")
(define COLUMN-LINE (rectangle (/ SIZE 3) SIZE "outline" MOVE-COLOR))
(define ROW-LINE (rectangle SIZE (/ SIZE 3) "outline" MOVE-COLOR))
(define O-MOVE (circle (/ SIZE 8) "outline" MOVE-COLOR))
(define X-MOVE (overlay (line (/ SIZE 4) (/ SIZE 4) MOVE-COLOR)
                        (rotate 90 (line (/ SIZE 4) (/ SIZE 4) MOVE-COLOR))))
(define GRID (overlay (beside COLUMN-LINE
                              COLUMN-LINE
                              COLUMN-LINE)
                      (above ROW-LINE
                             ROW-LINE
                             ROW-LINE)))

(define-struct cell (index val))




 




 

(define-struct tic-tac-toe (status difficulty turn board))






 

(define c0 (make-cell 0 ""))
(define c1 (make-cell 1 ""))
(define c2 (make-cell 2 ""))
(define c3 (make-cell 3 ""))
(define c4 (make-cell 4 ""))
(define c5 (make-cell 5 ""))
(define c6 (make-cell 6 ""))
(define c7 (make-cell 7 ""))
(define c8 (make-cell 8 ""))


(define tictactoe1 (make-tic-tac-toe 0 0 false (list c0 c1 c2 c3 c4 c5 c6 c7 c8)))
(define tictactoe2 (make-tic-tac-toe 0 0 true (list (make-cell 0 "X") c1 c2 c3 c4 c5 c6 c7 c8)))
(define tictactoe3 (make-tic-tac-toe 0 0 true (list (make-cell 0 "X") c1 (make-cell 2 "O") c3 (make-cell 4 "X") c5 c6 c7 c8)))
(define tictactoe3.2 (make-tic-tac-toe 0 0 false (list (make-cell 0 "X") c1 (make-cell 2 "O") (make-cell 3 "O") (make-cell 4 "X") c5 c6 c7 c8)))
(define tictactoe3.3 (make-tic-tac-toe 1 0 false (list (make-cell 0 "X") c1 (make-cell 2 "O") (make-cell 3 "O") (make-cell 4 "X") c5 c6 c7 (make-cell 8 "X"))))
(define tictactoe4.2 (make-tic-tac-toe 0 0 true (list (make-cell 0 "O") (make-cell 1 "O") (make-cell 2 "O") c3 (make-cell 4 "X") (make-cell 5 "X") (make-cell 6 "X") c7 c8)))
(define tictactoe4 (make-tic-tac-toe 0 0 true (list (make-cell 0 "X") (make-cell 1 "X") (make-cell 2 "X") c3 (make-cell 4 "O") c5 (make-cell 6 "O") c7 c8)))
(define tictactoe5 (make-tic-tac-toe 0 0 false (list (make-cell 0 "O") c1 c2 c3 (make-cell 4 "O") (make-cell 5 "X") (make-cell 6 "X") (make-cell 7 "X") (make-cell 8 "O"))))
(define tictactoe6 (make-tic-tac-toe 0 0 true (list (make-cell 0 "X") c1 c2 c3 (make-cell 4 "X") (make-cell 5 "O") (make-cell 6 "O") (make-cell 7 "O") (make-cell 8 "X"))))
(define tictactoe7 (make-tic-tac-toe 1 0 true (list (make-cell 0 "X") c1 c2 c3 (make-cell 4 "X") (make-cell 5 "O") (make-cell 6 "O") (make-cell 7 "O") (make-cell 8 "X"))))
(define tictactoe8 (make-tic-tac-toe 2 0 false (list (make-cell 0 "O") c1 c2 c3 (make-cell 4 "O") (make-cell 5 "X") (make-cell 6 "X") (make-cell 7 "X") (make-cell 8 "O"))))
(define tictactoe9 (make-tic-tac-toe 3 0 false (list (make-cell 0 "O") (make-cell 1 "O") (make-cell 2 "X") (make-cell 3 "X") (make-cell 4 "X") (make-cell 5 "O") (make-cell 6 "O") (make-cell 7 "O") (make-cell 8 "X"))))
(define tictactoe10 (make-tic-tac-toe 0 2 true (list (make-cell 0 "X") (make-cell 1 "O") (make-cell 2 "X") (make-cell 3 "X") (make-cell 4 "O") c5 c6 c7 c8)))
(define tictactoe11 (make-tic-tac-toe 2 2 true (list (make-cell 0 "X") (make-cell 1 "O") (make-cell 2 "X") (make-cell 3 "X") (make-cell 4 "O") c5 c6 (make-cell 7 "O") c8)))
(define tictactoe12 (make-tic-tac-toe 0 1 true (list (make-cell 0 "X") (make-cell 1 "O") (make-cell 2 "X") (make-cell 3 "X") (make-cell 4 "O") c5 c6 c7 c8)))
(define tictactoe13 (make-tic-tac-toe 2 1 true (list (make-cell 0 "X") (make-cell 1 "O") (make-cell 2 "X") (make-cell 3 "X") (make-cell 4 "O") c5 c6 (make-cell 7 "O") c8)))
(define tictactoe14 (make-tic-tac-toe 0 2 true (list (make-cell 0 "X") c1 (make-cell 2 "X") (make-cell 3 "X") (make-cell 4 "O") c5 c6 c7 (make-cell 8 "O"))))
(define tictactoe15 (make-tic-tac-toe 0 2 false (list (make-cell 0 "X") (make-cell 1 "O") (make-cell 2 "X") (make-cell 3 "X") (make-cell 4 "O") c5 c6 c7 (make-cell 8 "O"))))

(define START (make-tic-tac-toe 0 0 false (list c0 c1 c2 c3 c4 c5 c6 c7 c8)))


(define (main game)
  (big-bang game
    (on-tick computer-move)
    (to-draw render-board)
    (on-mouse user-move)
    (on-key change-bot))
  )





(check-expect (change-bot tictactoe1 "0") tictactoe1) 
(check-expect (change-bot tictactoe2 "0") (make-tic-tac-toe 0 0 true (tic-tac-toe-board tictactoe2))) 
(check-expect (change-bot tictactoe1 "1") (make-tic-tac-toe 0 1 false (tic-tac-toe-board tictactoe1)))
(check-expect (change-bot tictactoe1 "2") (make-tic-tac-toe 0 2 false (tic-tac-toe-board tictactoe1)))
(check-expect (change-bot tictactoe1 "a") tictactoe1)
(check-expect (change-bot tictactoe1 "5") tictactoe1) 
(define (change-bot game keyE)
  (if (not (false? (memf (λ (n) (key=? keyE (number->string n))) (list 0 1 2))))
      (make-tic-tac-toe (tic-tac-toe-status game)
                        (string->number keyE)
                        (tic-tac-toe-turn game)
                        (tic-tac-toe-board game))
      game
      )
  )





(check-expect (computer-move tictactoe1) tictactoe1) 
(check-expect (computer-move tictactoe7) tictactoe7) 
(check-expect (computer-move tictactoe8) tictactoe8) 
(check-expect (computer-move tictactoe9) tictactoe9) 
(check-expect (computer-move tictactoe10) tictactoe11) 
(check-expect (computer-move tictactoe12) tictactoe13) 
(check-expect (computer-move tictactoe14) tictactoe15) 
(define (computer-move game)
  (local [(define (thinking-ahead loc diff)
            (cond [(= diff 0) (random 8)]
                  [(< 0 (check-for-win true game 0)) (check-for-win true game 0)]
                  [(= diff 2)
                   (if (< 0 (check-for-win false game 0))
                       (check-for-win false game 0)
                       (thinking-ahead loc (sub1 diff)))]
                  [else (thinking-ahead loc (sub1 diff))]
                  ))
          (define (check-for-win player game acc)
            (cond [(< 0 (tic-tac-toe-status (update-game game player acc))) acc]
                  [(> acc 8) -1]
                  [else (check-for-win player game (add1 acc))]
                  )
            )
          ] 
    (cond [(or (< 0 (tic-tac-toe-status game)) (false? (tic-tac-toe-turn game))) game]
          [else (update-game game true (thinking-ahead (tic-tac-toe-board game) (tic-tac-toe-difficulty game)))]
          )
    )
  )




(check-expect (render-board tictactoe1) (place-image GRID (/ SIZE 2) (/ SIZE 2) MTS)) 
(check-expect (render-board tictactoe3) (place-images (list X-MOVE O-MOVE X-MOVE) 
                                                      (get-images-pos (tic-tac-toe-board tictactoe3))
                                                      (place-image GRID (/ SIZE 2) (/ SIZE 2) MTS)))
(check-expect (render-board tictactoe7) (place-image (text "Winner is X" TEXT-SIZE TEXT-COLOR) (/ SIZE 2) (/ SIZE 2) 
                                                     (place-images (get-images (tic-tac-toe-board tictactoe7))
                                                                   (get-images-pos (tic-tac-toe-board tictactoe7))
                                                                   (place-image GRID (/ SIZE 2) (/ SIZE 2) MTS)))) 
(check-expect (render-board tictactoe8) (place-image (text "Winner is O" TEXT-SIZE TEXT-COLOR) (/ SIZE 2) (/ SIZE 2) 
                                                     (place-images (get-images (tic-tac-toe-board tictactoe8))
                                                                   (get-images-pos (tic-tac-toe-board tictactoe8))
                                                                   (place-image GRID (/ SIZE 2) (/ SIZE 2) MTS)))) 
(check-expect (render-board tictactoe9) (place-image (text "Tie" TEXT-SIZE TEXT-COLOR) (/ SIZE 2) (/ SIZE 2) 
                                                     (place-images (get-images (tic-tac-toe-board tictactoe9))
                                                                   (get-images-pos (tic-tac-toe-board tictactoe9))
                                                                   (place-image GRID (/ SIZE 2) (/ SIZE 2) MTS)))) 
(define (render-board game)
  (local [(define (print-win winner?)
            (place-image (text (winning-statement winner?) TEXT-SIZE TEXT-COLOR) (/ SIZE 2) (/ SIZE 2) render-rest))
          (define (winning-statement winner)
            (cond [(= 1 winner) "Winner is X"]
                  [(= 2 winner) "Winner is O"]
                  [(= 3 winner) "Tie"]))
          (define render-rest (place-images (get-images (tic-tac-toe-board game)) (get-images-pos (tic-tac-toe-board game))
                                            (place-image GRID (/ SIZE 2) (/ SIZE 2)
                                                         MTS)))
          ]
    (if (not (= (tic-tac-toe-status game) 0))
        (print-win (tic-tac-toe-status game))
        render-rest
        )
    ))






(check-expect (get-images (tic-tac-toe-board tictactoe1)) empty) 
(check-expect (get-images (tic-tac-toe-board tictactoe2)) (list X-MOVE)) 
(check-expect (get-images (tic-tac-toe-board tictactoe3)) (list X-MOVE O-MOVE X-MOVE)) 
(define (get-images loc)
  (local [(define (list-parse loc)
            (cond [(empty? loc) empty]
                  [else
                   (append (get-cell-image (first loc))
                           (list-parse (rest loc)))]
                  ) )
          (define (get-cell-image cell)
            (cond [(string=? (cell-val cell) "X")(list X-MOVE)]
                  [(string=? (cell-val cell) "O") (list O-MOVE)]
                  [else empty]
                  ))]
    (list-parse loc))
  )





(check-expect (get-images-pos empty) empty) 
(check-expect (get-images-pos (tic-tac-toe-board tictactoe1)) empty) 
(check-expect (get-images-pos (tic-tac-toe-board tictactoe3)) (list (make-posn (/ SIZE 6) (/ SIZE 6)) 
                                                                    (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                                                    (make-posn (* 3 (/ SIZE 6)) (* 3 (/ SIZE 6)))))                                                               
(define (get-images-pos loc)
  (local [(define (list-parse loc)
            (cond [(empty? loc) empty]
                  [else
                   (append (get-cell-pos (first loc))
                           (list-parse (rest loc)))]
                  ))
          (define (get-cell-pos cell)
            (if (string=? (cell-val cell) "")
                empty
                (list (make-posn (+ (/ SIZE 6) (* (/ SIZE 3) (modulo (cell-index cell) 3))) (+ (/ SIZE 6) (* (/ SIZE 3) (floor (/ (cell-index cell) 3))))))))]
    (list-parse loc))
  )





(check-expect (user-move tictactoe1 0 0 "button-down") tictactoe2)  
(check-expect (user-move tictactoe1 2 7 "button-down") (make-tic-tac-toe 0 0 true
                                                                         (cons (make-cell 0 "X")
                                                                               (rest (tic-tac-toe-board tictactoe1))))) 
(check-expect (user-move tictactoe1 0 0 "enter") tictactoe1) 
(check-expect (user-move tictactoe3.2 299 299 "button-down") tictactoe3.3) 
(define (user-move game x y mouseE)
  (if (and (mouse=? mouseE "button-down") (false? (tic-tac-toe-turn game)) (= 0 (tic-tac-toe-status game)))
      (update-game game false (+ (floor (/ x (/ SIZE 3))) (* 3 (floor (/ y (/ SIZE 3))))))
      game))




(check-expect (update-game tictactoe1 false 0) tictactoe2) 
(check-expect (update-game tictactoe2 true 0) tictactoe2) 
(check-expect (update-game tictactoe3 true 3) tictactoe3.2) 
(check-expect (update-game tictactoe3.2 false 8) tictactoe3.3) 
(define (update-game game player index)
  (local [(define (search-loc loc acc)
            (cond [(empty? loc) empty]
                  [(= acc index) (cons (update-cell (first loc)) (rest loc))]
                  [else (cons (first loc) (search-loc (rest loc) (add1 acc)))]
                  )
            )
          (define (update-cell cell)
            (if (string=? "" (cell-val cell))
                (if player
                    (make-cell (cell-index cell) "O")
                    (make-cell (cell-index cell) "X")
                    )
                cell))
          (define new-board? (search-loc (tic-tac-toe-board game) 0)) 
          (define (compare board1 board2)
            (cond [(and (empty? board1) (empty? board2)) true]
                  [else
                   (if (string=? (cell-val (first board1)) (cell-val (first board2)))
                       (compare (rest board1) (rest board2))
                       false)
                   ]))
          (define runWin?
            (win? (if (not (false? player)) "O" "X") new-board?))
          ]
    (if (false? (compare new-board? (tic-tac-toe-board game)))
        (if (= runWin? 0)
            (make-tic-tac-toe 0 (tic-tac-toe-difficulty game) (false? player) new-board?)
            (make-tic-tac-toe runWin? (tic-tac-toe-difficulty game) player new-board?))
        game)))






(check-expect (win? "X" (tic-tac-toe-board tictactoe1)) 0) 
(check-expect (win? "O" (tic-tac-toe-board tictactoe1)) 0)
(check-expect (win? "O" (tic-tac-toe-board tictactoe4.2)) 2)
(check-expect (win? "X" (tic-tac-toe-board tictactoe4)) 1)
(check-expect (win? "O" (tic-tac-toe-board tictactoe5)) 2)
(check-expect (win? "X" (tic-tac-toe-board tictactoe6)) 1)
(check-expect (win? "X" (tic-tac-toe-board tictactoe9)) 3)
(define (win? player board)
  (local [(define (search-loc loc index acc)
            (cond [(= acc index) (cell-val (first loc))]
                  [else (search-loc (rest loc) index (+ 1 acc))]
                  )
            )
          (define (check loi)
            (string=? player
                      (search-loc board (first loi) 0)
                      (search-loc board (first (rest loi)) 0)
                      (search-loc board (first (rest (rest loi))) 0)))
          (define rows
            (or (check (list 0 1 2))
                (check (list 3 4 5))
                (check (list 6 7 8))  
                ))
          (define columns
            (or (check (list 0 3 6))
                (check (list 1 4 7))
                (check (list 2 5 8))  
                ))
          (define diagonal
            (or (check (list 0 4 8))
                (check (list 2 4 6)) 
                ))
          ]
    (if (or diagonal columns rows)
        (if (string=? "O" player)
            2
            1)
        (if (not (false? (memf (λ (n) (string=? "" (cell-val n))) board)))
            0
            3
            )
        )
    )
  )