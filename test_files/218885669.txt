

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 900) 
(define MTS (empty-scene SIZE SIZE))






(define divideFactor 20)
(define MARGIN (/ SIZE divideFactor))


(define middle (/ SIZE 2))
(define oneThird (* MARGIN (/ (- divideFactor 2) 3)))
(define posnForLines
  (list
   (make-posn middle (+ MARGIN oneThird ))
   (make-posn middle (+ MARGIN (* oneThird 2)))
   (make-posn (+ MARGIN oneThird) middle)
   (make-posn (+ MARGIN (* oneThird 2)) middle)
   )
  )


(define cpen (make-pen "Chocolate" 10 "solid" "round" "round"))
(define linX (line (* MARGIN (- divideFactor 2)) 0 cpen ))
(define linY (line  0 (* MARGIN (- divideFactor 2))  cpen ))
(define lines (list linX linX linY linY))


(define oneSixth (* MARGIN (/ (- divideFactor 2) 6)))
(define middleLeft/top (+ MARGIN oneSixth))
(define middleRight/bottom (+ MARGIN (* 5 oneSixth)))

(define posnForX&O
  (list
   (make-posn middleLeft/top middleLeft/top) 
   (make-posn middle middleLeft/top)
   (make-posn middleRight/bottom middleLeft/top)
   (make-posn middleLeft/top middle)
   (make-posn middle middle)
   (make-posn middleRight/bottom middle)
   (make-posn middleLeft/top  middleRight/bottom)
   (make-posn middle  middleRight/bottom)
   (make-posn middleRight/bottom  middleRight/bottom) 
   )
  )



(define winText (text "You Won!" (/ SIZE 6) "Chartreuse"))
(define loseText (text "You lose" (/ SIZE 6) "Pale Violet Red"))
(define tieText (text "You tie" (/ SIZE 6) "Dim Gray"))





(define-struct gameState (board awareness playerTurn? gameOver?))
















 
  













(define emptyBoard (list "" "" ""
                         "" "" ""
                         "" "" ""))
(define START (make-gameState emptyBoard 0 true "ongoing"))

(define (main gameState)
  (big-bang gameState    
    (on-tick   tock)     
    (to-draw   render)   
    (on-mouse  handle-mouse)      
    (on-key    handle-key)))    






(define (tock gameState)
  (local [(define random-pos (random 9))] 
    (make-gameState (AI-turn (gameState-board gameState) (gameState-playerTurn? gameState)
                             (gameState-gameOver? gameState) random-pos
                             (gameState-awareness gameState) )
                    (gameState-awareness gameState)
                    (hasAIMoved
                     (gameState-board gameState)
                     (AI-turn (gameState-board gameState) (gameState-playerTurn? gameState)
                              (gameState-gameOver? gameState) random-pos
                              (gameState-awareness gameState))
                     (gameState-playerTurn? gameState)
                     )
                    (updateGameOver (gameState-board gameState))
                    )
    )
  )




(check-expect (render START)
              (place-images
               (append lines (list (genAwarenessText 0))
                       (generateX&O emptyBoard))
               (append posnForLines
                       (list (make-posn MARGIN (- SIZE MARGIN))) posnForX&O)
               MTS))
(check-expect (render (make-gameState (list "x" "" "o"
                                            "" "" ""
                                            "x" "" "") 4 false "ongoing"))
              (place-images
               (append lines (list (genAwarenessText 4))
                       (generateX&O (list "x" "" "o"
                                          "" "" ""
                                          "x" "" "")))
               (append posnForLines
                       (list (make-posn MARGIN (- SIZE MARGIN))) posnForX&O)
               MTS))
(check-expect (render (make-gameState (list "x" "x" "x"
                                            "o" "o" ""
                                            "" "" "") 7 false "won"))
              (place-images
               (append (list (genResultText "won"))
                       (list (genAwarenessText 7))
                       lines (generateX&O (list "x" "x" "x"
                                                "o" "o" ""
                                                "" "" "")))
               (append (list (make-posn middle middle)) (list (make-posn MARGIN (- SIZE MARGIN)))
                       posnForLines posnForX&O)
               MTS))


(define (render gameState)
  (if (equal? (gameState-gameOver? gameState) "ongoing")
      
      (place-images
       (append lines (list (genAwarenessText (gameState-awareness gameState) ))
               (generateX&O (gameState-board gameState)))
       (append posnForLines (list (make-posn MARGIN (- SIZE MARGIN))) posnForX&O)
       MTS)
      
      (place-images
       (append (list (genResultText (gameState-gameOver? gameState)))
               (list (genAwarenessText (gameState-awareness gameState) ))
               lines (generateX&O (gameState-board gameState)))
       (append (list (make-posn middle middle)) (list (make-posn MARGIN (- SIZE MARGIN)))
               posnForLines posnForX&O)
       MTS)
      )
  )




(check-expect (genResultText "won") winText)
(check-expect (genResultText "lost") loseText)
(check-expect (genResultText "tie") tieText)



(define (genResultText gameOver?)
  (cond
    [(equal? gameOver? "won") winText]
    [(equal? gameOver? "lost") loseText]
    [(equal? gameOver? "tie") tieText]
    )
  )




(check-expect (generateX&O(list "o" "x" ""
                                "x" "" "x"
                                "o" "" "o"))
              (list (text "o" (/ SIZE 6) (whatColor? "o"))
                    (text "x" (/ SIZE 6) (whatColor? "x"))
                    (text "" (/ SIZE 6) (whatColor? ""))
                    (text "x" (/ SIZE 6) (whatColor? "x"))
                    (text "" (/ SIZE 6) (whatColor? ""))
                    (text "x" (/ SIZE 6) (whatColor? "x"))
                    (text "o" (/ SIZE 6) (whatColor? "o"))
                    (text "" (/ SIZE 6) (whatColor? ""))
                    (text "o" (/ SIZE 6) (whatColor? "o")))
              )
(check-expect (generateX&O empty)empty)



(define (generateX&O board)
  (cond
    [(empty? board) empty]
    [else
     (cons
      (text (first board) (/ SIZE 6) (whatColor? (first board)))
      (generateX&O (rest board))
      )
     ]
    )
  )





(check-expect (whatColor? "x")"red")
(check-expect (whatColor? "o")"blue")


(define (whatColor? cell)
  (if (string=? cell "x")
      
      "red"
      
      "blue"
      )
  )




(check-expect (genAwarenessText 0) (text "0" (/ SIZE 10) "black"))
(check-expect (genAwarenessText 5) (text "5" (/ SIZE 10) "black"))
(check-expect (genAwarenessText 2) (text "2" (/ SIZE 10) "black"))
(check-expect (genAwarenessText 9) (text "9" (/ SIZE 10) "black"))


(define (genAwarenessText awareness)
  (text (number->string awareness) ( / SIZE 10) "black")
  )








(check-expect (handle-mouse  (make-gameState (list "" "" ""
                                                   "" "" ""
                                                   "" "" "") 0 true "ongoing") 0 0 "button-down")
              (make-gameState (list "x" "" ""
                                    "" "" ""
                                    "" "" "") 0 false "ongoing"))
(check-expect (handle-mouse  (make-gameState (list "" "x" ""
                                                   "" "" ""
                                                   "" "" "") 0 false "ongoing") 0 0 "button-down")
              (make-gameState (list "" "x" ""
                                    "" "" ""
                                    "" "" "") 0 false "ongoing"))
(check-expect (handle-mouse  (make-gameState (list "" "x" ""
                                                   "o" "" ""
                                                   "" "" "") 0 true "ongoing")
                             (/ SIZE 2) 0 "button-down")
              (make-gameState (list "" "x" ""
                                    "o" "" ""
                                    "" "" "") 0 true "ongoing"))
(check-expect (handle-mouse  (make-gameState (list "" "x" ""
                                                   "o" "" ""
                                                   "" "" "") 0 true "ongoing")
                             (/ SIZE 2) (/ SIZE 2) "button-down")
              (make-gameState (list "" "x" ""
                                    "o" "x" ""
                                    "" "" "") 0 false "ongoing"))
(check-expect (handle-mouse  (make-gameState (list "" "x" ""
                                                   "o" "" ""
                                                   "" "" "") 0 true "ongoing")
                             (/ SIZE 2) (/ SIZE 2) "drag")
              (make-gameState (list "" "x" ""
                                    "o" "" ""
                                    "" "" "") 0 true "ongoing"))


(define (handle-mouse gameState posX posY mouseEvent)
  (if (and (equal? "ongoing"(gameState-gameOver? gameState))
           (equal? (gameState-playerTurn? gameState) true))
      
      (cond
        [(mouse=? mouseEvent "button-down")
         (make-gameState
          (player-click (gameState-board gameState) posX posY)
          (gameState-awareness gameState)
          (has-player-moved? gameState posX posY)
          (gameState-gameOver? gameState)

          )
         ]
        [else gameState]
        )
      
      gameState
      )
  )








(check-expect (handle-key START "0") START)

(check-expect (handle-key START "1")
              (make-gameState(gameState-board START) 1
                             (gameState-playerTurn? START)
                             (gameState-gameOver? START)))

(check-expect (handle-key START "2")
              (changeAwareness START 2))

(check-expect (handle-key START "3")
              (changeAwareness START 3))

(check-expect (handle-key START "4")
              (changeAwareness START 4))

(check-expect (handle-key START "5")
              (changeAwareness START 5))

(check-expect (handle-key START "6")
              (changeAwareness START 6))

(check-expect (handle-key START "7")
              (changeAwareness START 7))

(check-expect (handle-key START "8")
              (changeAwareness START 8))

(check-expect (handle-key START "9")
              (changeAwareness START 9))

(check-expect (handle-key (make-gameState empty 1 false "won") "r")START)


(check-expect (handle-key START "s")START)


(define (handle-key gameState key)
  (cond
    [(key=? key "0") (changeAwareness gameState 0)]
    [(key=? key "1") (changeAwareness gameState 1)]
    [(key=? key "2") (changeAwareness gameState 2)]
    [(key=? key "3") (changeAwareness gameState 3)]
    [(key=? key "4") (changeAwareness gameState 4)]
    [(key=? key "5") (changeAwareness gameState 5)]
    [(key=? key "6") (changeAwareness gameState 6)]
    [(key=? key "7") (changeAwareness gameState 7)]
    [(key=? key "8") (changeAwareness gameState 8)]
    [(key=? key "9") (changeAwareness gameState 9)]
    [(key=? key "r") START]
    [else gameState]
    )
  )




(check-expect (changeAwareness START 4)
              (make-gameState emptyBoard 4 true "ongoing"))
(check-expect (changeAwareness
               (make-gameState (list "" "x" ""
                                     "" "o" ""
                                     "" "" "") 5 true "ongoing") 1)
              (make-gameState (list "" "x" ""
                                    "" "o" ""
                                    "" "" "") 1 true "ongoing"))
(check-expect (changeAwareness START 0)
              START)


(define (changeAwareness gameState newAwareness)
  (make-gameState
   (gameState-board gameState)
   newAwareness
   (gameState-playerTurn? gameState)
   (gameState-gameOver? gameState)
   )
  )






(check-expect (updateGameOver
               (list "o" "x" "o"
                     "o" "x" "x"
                     "x" "x" "o")
               )
              "won"
              )
(check-expect (updateGameOver
               (list "o" "x" "o"
                     "o" "x" "x"
                     "o" "o" "x")
               )
              "lost"
              )
(check-expect (updateGameOver
               (list "o" "x" "o"
                     "o" "x" "x"
                     "x" "o" "o")
               )
              "tie"
              )
(check-expect (updateGameOver
               (list "o" "x" "o"
                     "o" "" ""
                     "x" "x" "o")
               )
              "ongoing"
              )


(define (updateGameOver board)
  (local [(define didPlayerWin? (whoWon? board))]
    (cond
      [(equal? true didPlayerWin?) "won"]
      [(false? didPlayerWin?) "lost"]
      [(false? (member "" board)) "tie"]
      [else "ongoing"]
      )
    )
  )







(check-expect (whoWon? (list "o" "x" "o"
                             "o" "x" "o"
                             "x" "o" "x")) 0)

(check-expect (whoWon? (list "" "" ""
                             "" "" ""
                             "" "" "")) 0)

(check-expect (whoWon? (list "x" "x" "x"
                             "o" "x" "o"
                             "x" "o" "x")) true)

(check-expect (whoWon? (list "o" "x" "o"
                             "x" "x" "x"
                             "x" "o" "x")) true)

(check-expect (whoWon? (list "o" "x" "o"
                             "x" "o" "o"
                             "x" "x" "x")) true)

(check-expect (whoWon? (list "x" "x" "o"
                             "x" "x" "o"
                             "x" "o" "x")) true)

(check-expect (whoWon? (list "o" "x" "o"
                             "o" "x" "x"
                             "x" "x" "o")) true)

(check-expect (whoWon? (list "o" "x" "x"
                             "o" "x" "x"
                             "x" "o" "x")) true)

(check-expect (whoWon? (list "o" "x" "x"
                             "o" "x" "o"
                             "x" "o" "x")) true)

(check-expect (whoWon? (list "x" "o" "o"
                             "o" "x" "o"
                             "x" "o" "x")) true)



(check-expect (whoWon? (list "o" "o" "o"
                             "o" "x" "o"
                             "x" "o" "x")) false)

(check-expect (whoWon? (list "o" "x" "o"
                             "o" "o" "o"
                             "x" "o" "x")) false)

(check-expect (whoWon? (list "o" "x" "o"
                             "x" "o" "x"
                             "o" "o" "o")) false)

(check-expect (whoWon? (list "o" "x" "o"
                             "o" "x" "o"
                             "o" "o" "x")) false)

(check-expect (whoWon? (list "o" "o" "x"
                             "o" "o" "x"
                             "x" "o" "o")) false)

(check-expect (whoWon? (list "o" "x" "o"
                             "o" "x" "o"
                             "x" "o" "o")) false)

(check-expect (whoWon? (list "o" "x" "o"
                             "x" "o" "o"
                             "o" "o" "x")) false)

(check-expect (whoWon? (list "o" "x" "o"
                             "o" "o" "x"
                             "x" "o" "o")) false)



(define (whoWon? board)
  (cond
    
    [(cellsEqual? (list-ref board 0) (list-ref board 1) (list-ref board 2))
     (if (string=? (list-ref board 0) "x") true false)
     ]
    
    [(cellsEqual? (list-ref board 3) (list-ref board 4) (list-ref board 5))
     (if (string=? (list-ref board 3) "x") true false)
     ]
    
    [(cellsEqual? (list-ref board 6) (list-ref board 7) (list-ref board 8))
     (if (string=? (list-ref board 6) "x") true false)
     ]
    
    [(cellsEqual? (list-ref board 0) (list-ref board 3) (list-ref board 6))
     (if (string=? (list-ref board 0) "x") true false)
     ]
    
    [(cellsEqual? (list-ref board 1) (list-ref board 4) (list-ref board 7))
     (if (string=? (list-ref board 1) "x") true false)
     ]
    
    [(cellsEqual? (list-ref board 2) (list-ref board 5) (list-ref board 8))
     (if (string=? (list-ref board 2) "x") true false)
     ]
    
    [(cellsEqual? (list-ref board 0) (list-ref board 4) (list-ref board 8))
     (if (string=? (list-ref board 0) "x") true false)
     ]
    
    [(cellsEqual? (list-ref board 2) (list-ref board 4) (list-ref board 6))
     (if (string=? (list-ref board 2) "x") true false)
     ]
    [else 0]
    
    )
  )







(check-expect (cellsEqual? "x" "x" "x") true)
(check-expect (cellsEqual? "x" "y" "x") false)
(check-expect (cellsEqual? "x" "y" "") false)
(check-expect (cellsEqual? "" "" "") false)


(define (cellsEqual? cell1 cell2 cell3)
  (and
   (false?(string=? cell1 ""))
   (string=? cell1 cell2 cell3)
   )
  )








(check-expect (changeBoard emptyBoard 0 "o") (list "o" "" ""
                                                   "" "" ""
                                                   "" "" ""))
(check-expect (changeBoard emptyBoard 0 "x") (list "x" "" ""
                                                   "" "" ""
                                                   "" "" ""))
(check-expect (changeBoard emptyBoard 4 "o") (list "" "" ""
                                                   "" "o" ""
                                                   "" "" ""))
(check-expect (changeBoard (list "" "x" "o"
                                 "" "x" "o"
                                 "x" "" "")
                           8 "o")
              (list "" "x" "o"
                    "" "x" "o"
                    "x" "" "o"))
(check-expect (changeBoard (list "o" "" "o"
                                 "" "x" "o"
                                 "x" "x" "")
                           3 "x")
              (list "o" "" "o"
                    "x" "x" "o"
                    "x" "x" ""))


(define (changeBoard board indexToChange newString )
  (cond
    [(empty? board) empty]
    [(= indexToChange 0)
     (cons newString
           (changeBoard (rest board) (- indexToChange 1) newString)
           )
     ]
    [else
     (cons
      (first board)
      (changeBoard (rest board) (- indexToChange 1) newString)
      )
     ]
    )
  )








(check-expect (player-click emptyBoard 0 0)
              (list "x" "" ""
                    "" "" ""
                    "" "" ""))
(check-expect (player-click emptyBoard (/ SIZE 2) (/ SIZE 2))
              (list "" "" ""
                    "" "x" ""
                    "" "" ""))
(check-expect (player-click (list "" "x" "o"
                                  "x" "" ""
                                  "" "" "o")
                            (/ SIZE 2) (/ SIZE 2))
              (list "" "x" "o"
                    "x" "x" ""
                    "" "" "o"))
(check-expect (player-click (list "" "x" "o"
                                  "x" "" ""
                                  "" "" "o")
                            (/ SIZE 2) (/ SIZE 2))
              (list "" "x" "o"
                    "x" "x" ""
                    "" "" "o"))
(check-expect (player-click (list "" "x" "o"
                                  "x" "x" ""
                                  "o" "" "o")
                            (/ SIZE 2) (/ SIZE 2))
              (list "" "x" "o"
                    "x" "x" ""
                    "o" "" "o"))



(define (player-click board posX posY)
  (local
    [
     (define margin+1/3 (+ MARGIN oneThird))
     (define margin+2/3 (+ MARGIN (* 2 oneThird)))
     ]
    (cond
      
      [(incords? posX posY (make-posn 0 0)
                 (make-posn margin+1/3 margin+1/3))
       (tryToPlaceX board 0)
       ]
      
      [(incords? posX posY (make-posn margin+1/3 0)
                 (make-posn margin+2/3 margin+1/3))
       (tryToPlaceX board 1)
       ]
      
      [(incords? posX posY (make-posn margin+2/3 0)
                 (make-posn SIZE margin+1/3))
       (tryToPlaceX board 2)
       ]
      
      [(incords? posX posY (make-posn 0 margin+1/3)
                 (make-posn margin+1/3 margin+2/3))
       (tryToPlaceX board 3)
       ]
      
      [(incords? posX posY (make-posn margin+1/3 margin+1/3)
                 (make-posn margin+2/3 margin+2/3))
       (tryToPlaceX board 4)
       ]
      
      [(incords? posX posY (make-posn margin+2/3 margin+1/3)
                 (make-posn SIZE margin+2/3))
       (tryToPlaceX board 5)
       ]
      
      [(incords? posX posY (make-posn 0 margin+2/3)
                 (make-posn margin+1/3 SIZE))
       (tryToPlaceX board 6)
       ]
      
      [(incords? posX posY (make-posn margin+1/3 margin+2/3)
                 (make-posn margin+2/3 SIZE))
       (tryToPlaceX board 7)
       ]
      
      [(incords? posX posY (make-posn margin+2/3 margin+2/3)
                 (make-posn SIZE SIZE))
       (tryToPlaceX board 8)
       ]
      [else board]
      )
    )
  )





(check-expect (tryToPlaceX (list "" "" "x"
                                 "" "o" ""
                                 "" "" "") 2)
              (list "" "" "x"
                    "" "o" ""
                    "" "" ""))
              
(check-expect (tryToPlaceX (list "" "" "x"
                                 "" "o" ""
                                 "" "" "") 3)
              (list "" "" "x"
                    "x" "o" ""
                    "" "" ""))
(check-expect (tryToPlaceX (list "x" "o" "x"
                                 "o" "o" ""
                                 "x" "" "") 8)
              (list "x" "o" "x"
                    "o" "o" ""
                    "x" "" "x"))



(define (tryToPlaceX board index)
  (if (equal? (list-ref board index) "")
      
      (changeBoard board index "x")
      
      board
      )
  )









(check-expect (incords? 0 0 (make-posn 0 0) (make-posn 10 10)) true)
(check-expect (incords? 50 20 (make-posn 30 10) (make-posn 40 30)) false)
(check-expect (incords? 100 100 (make-posn 30 30) (make-posn 150 150)) true)
(check-expect (incords? 100 100 (make-posn 30 30) (make-posn 150 40)) false)
(check-expect (incords? 50 50 (make-posn 60 20) (make-posn 70 60)) false)
(check-expect (incords? 50 50 (make-posn 20 60) (make-posn 60 70)) false)
(check-expect (incords? 200 200 (make-posn 100 100) (make-posn 250 150)) false)



(define (incords? x y pos-top-left pos-bottom-right)
  (and (and (>= x (posn-x pos-top-left))
            (< x (posn-x pos-bottom-right)))
       (and (>= y (posn-y pos-top-left))
            (< y (posn-y pos-bottom-right)))))







(check-expect (has-player-moved? (make-gameState (list "" "" ""
                                                       "" "" ""
                                                       "" "" "") 0 true "ongoing")
                                 0 0) false)
(check-expect (has-player-moved? (make-gameState (list "o" "" ""
                                                       "" "" "x"
                                                       "" "" "") 0 true "ongoing")
                                 (/ SIZE 2) 0) false)
(check-expect (has-player-moved? (make-gameState (list "o" "o" ""
                                                       "" "" "x"
                                                       "" "" "x") 0 true "ongoing")
                                 (/ SIZE 2) 0) true)
(check-expect (has-player-moved? (make-gameState (list "o" "x" "o"
                                                       "o" "" "x"
                                                       "" "" "x") 0 true "ongoing")
                                 (/ SIZE 2) 0) true) 


(define (has-player-moved? gameState posX posY)
  (equal? (gameState-board gameState)
          (player-click (gameState-board gameState) posX posY)))








(check-expect (hasAIMoved emptyBoard emptyBoard false) false)
(check-expect (hasAIMoved emptyBoard emptyBoard true) true)
(check-expect (hasAIMoved (list "" "x" ""
                                "" "" ""
                                "" "" "")
                          (list "" "x" ""
                                "" "" ""
                                "" "" "")
                          false) false)
(check-expect (hasAIMoved (list "" "x" ""
                                "" "" ""
                                "" "" "")
                          (list "" "x" "o"
                                "" "" ""
                                "" "" "")
                          false) true)



(define (hasAIMoved oldBoard newBoard isPlayerTurn?)
  (cond
    [(equal? isPlayerTurn? #true) true]
    [else (not(equal? oldBoard newBoard))]
    )
  )















(check-expect (AI-turn (list "x" "o" "x"
                             "" "o" "x"
                             "" "o" "") true "lost" 3 0)
              (list "x" "o" "x"
                    "" "o" "x"
                    "" "o" ""))

(check-expect (AI-turn (list "x" "o" "x"
                             "" "" ""
                             "" "o" "") true "ongoing" 3 0)
              (list "x" "o" "x"
                    "" "" ""
                    "" "o" ""))

(check-expect (AI-turn (list "x" "o" "x"
                             "" "" "x"
                             "" "o" "") false "ongoing" 3 0)
              (list "x" "o" "x"
                    "o" "" "x"
                    "" "o" ""))

(check-expect (AI-turn (list "x" "o" "x"
                             "" "" "x"
                             "" "o" "") false "ongoing" 3 1)
              (list "x" "o" "x"
                    "" "o" "x"
                    "" "o" ""))

(check-expect (AI-turn (list "x" "o" "x"
                             "" "x" ""
                             "" "o" "") false "ongoing" 3 1)
              (list "x" "o" "x"
                    "o" "x" ""
                    "" "o" ""))

(check-expect (AI-turn (list "x" "o" "x"
                             "" "" "x"
                             "" "o" "") false "ongoing" 3 2)
              (list "x" "o" "x"
                    "" "o" "x"
                    "" "o" ""))

(check-expect (AI-turn (list "x" "o" "x"
                             "" "" "x"
                             "o" "" "") false "ongoing" 3 2)
              (list "x" "o" "x"
                    "" "" "x"
                    "o" "" "o"))

(check-expect (AI-turn (list "x" "o" "x"
                             "" "" ""
                             "" "" "") false "ongoing" 3 2)
              (list "x" "o" "x"
                    "o" "" ""
                    "" "" ""))

(check-expect (AI-turn (list "x" "" ""
                             "" "" ""
                             "" "" "") false "ongoing" 3 8)
              (list "x" "" ""
                    "" "o" ""
                    "" "" ""))


(define (AI-turn board isPlayerTurn? gameOver? random-pos awareness)
  (cond
    [(or (equal? isPlayerTurn? #true) (not(equal? (updateGameOver board) "ongoing"))) board]
    
    [else
     (cond
       [(= awareness 0) (randomMove board random-pos)]
       
       [(= awareness 1) (if (false? (easyAwarenessAI board ))
                            (randomMove board random-pos)
                            (easyAwarenessAI board )
                            )
                        ]
       [(= awareness 2) (if (false? (mediumAwarenessAI board ))
                            (randomMove board random-pos)
                            (mediumAwarenessAI board )
                            )
                        ]
       [(>= awareness 3) (adaptiveAwarenessAI board awareness "o")]
       [else (randomMove board random-pos)] 
       )
     ]
    )
  )







(check-expect (randomMove (list "x" "o" "x"
                                "" "" ""
                                "" "" "") 3)
              (list "x" "o" "x"
                    "o" "" ""
                    "" "" ""))

(check-expect (randomMove (list "" "x" "o"
                                "o" "x" ""
                                "" "" "") 2)
              (list "" "x" "o"
                    "o" "x" ""
                    "" "" ""))
(check-expect (randomMove (list "" "" ""
                                "o" "x" ""
                                "" "" "") 2)
              (list "" "" "o"
                    "o" "x" ""
                    "" "" ""))



(define (randomMove board random-pos)
  (if (string=? (list-ref board random-pos) "")
      (changeBoard board random-pos "o")
      board
      )
  )




(check-expect (easyAwarenessAI (list "" "" ""
                                     "o" "o" ""
                                     "" "" "") )
              (list "" "" ""
                    "o" "o" "o"
                    "" "" ""))
(check-expect (easyAwarenessAI (list "" "x" "x"
                                     "o" "o" ""
                                     "" "" "") )
              (list "" "x" "x"
                    "o" "o" "o"
                    "" "" ""))
(check-expect (easyAwarenessAI (list "" "" "x"
                                     "o" "o" "x"
                                     "" "" "") )
              false)




(define (easyAwarenessAI board )
  (local
    [(define possableMoves (generatePermutations board "o"))
     (define (checkMoves possableMoves )
       (cond
         [(empty? possableMoves) false]
         [(equal? (updateGameOver (first possableMoves)) "lost") (first possableMoves)]
         [else
          (checkMoves (rest possableMoves) )
          ]
         )
       )
     ]
    (checkMoves possableMoves )
    )
  )




(check-expect (generatePermutations (list "x" "" "o"
                                          "" "o" ""
                                          "x" "" "x") "o")
              (list (list "x" "o" "o"
                          "" "o" ""
                          "x" "" "x")
                    (list "x" "" "o"
                          "o" "o" ""
                          "x" "" "x")
                    (list "x" "" "o"
                          "" "o" "o"
                          "x" "" "x")
                    (list "x" "" "o"
                          "" "o" ""
                          "x" "o" "x")))

(check-expect (generatePermutations (list "" "x" "o"
                                          "" "" ""
                                          "" "" "") "x")
              (list (list "x" "x" "o"
                          "" "" ""
                          "" "" "")
                    (list "" "x" "o"
                          "x" "" ""
                          "" "" "")
                    (list "" "x" "o"
                          "" "x" ""
                          "" "" "")
                    (list "" "x" "o"
                          "" "" "x"
                          "" "" "")
                    (list "" "x" "o"
                          "" "" ""
                          "x" "" "")
                    (list "" "x" "o"
                          "" "" ""
                          "" "x" "")
                    (list "" "x" "o"
                          "" "" ""
                          "" "" "x")))



(define (generatePermutations board playerSymbol)
  (local [
          (define (genPerm board Index)
            (cond
              [(= Index 9) empty]
              [(string=? (list-ref board Index) "")
               (cons (changeBoard board Index playerSymbol) (genPerm board (+ Index 1)))]
              [else (genPerm board (+ Index 1))]
              )
            )
          ]
    (genPerm board 0)
    )
  )






(check-expect (mediumAwarenessAI (list "" "" ""
                                       "o" "o" ""
                                       "" "" "") )
              (list "" "" ""
                    "o" "o" "o"
                    "" "" ""))
(check-expect (mediumAwarenessAI (list "" "x" "x"
                                       "o" "o" ""
                                       "" "" "") )
              (list "" "x" "x"
                    "o" "o" "o"
                    "" "" ""))
(check-expect (mediumAwarenessAI (list "" "" "x"
                                       "o" "o" "x"
                                       "" "" "") )
              (list "" "" "x"
                    "o" "o" "x"
                    "" "" "o"))

(check-expect (mediumAwarenessAI (list "" "x" "x"
                                       "o" "" ""
                                       "" "" ""))
              (list "o" "x" "x"
                    "o" "" ""
                    "" "" ""))

(check-expect (mediumAwarenessAI (list "" "o" "x"
                                       "o" "x" ""
                                       "" "" "") )
              (list "" "o" "x"
                    "o" "x" ""
                    "o" "" ""))

(check-expect (mediumAwarenessAI (list "" "x" "x"
                                       "o" "o" ""
                                       "" "" "") )
              (list "" "x" "x"
                    "o" "o" "o"
                    "" "" ""))



(define (mediumAwarenessAI board )
  (local
    [(define possableMoves (generatePermutations board "o"))
     (define canWin (easyAwarenessAI board ))
     (define possablePlayerMoves (generatePermutations board "x"))
     
     (define (checkMoves possableMoves )
       (if (false? canWin)
           (tryToBlock possablePlayerMoves 0)
           canWin 
           )
       )
     (define (tryToBlock possablePlayerMoves playerMoveIndex)
       
       (cond
         [(empty? possablePlayerMoves) false]
         [(equal? (updateGameOver (first possablePlayerMoves)) "won")
          (list-ref possableMoves playerMoveIndex)]
         [else
          (tryToBlock (rest possablePlayerMoves) (+ 1 playerMoveIndex))
          ]
         )
       )
     
       
     ]
    (checkMoves possableMoves )
    )
  )



(define-struct boardPermutation (board nextBoards))






 








(check-expect (adaptiveAwarenessAI
               (list "x" "" ""
                     "" "" ""
                     "" "" "") 8 "o")
              (list "x" "" ""
                    "" "o" ""
                    "" "" "")
              )

(check-expect (adaptiveAwarenessAI
               (list "x" "" "x"
                     "" "o" ""
                     "" "" "") 7 "o")
              (list "x" "o" "x"
                    "" "o" ""
                    "" "" "")
              )



(define (adaptiveAwarenessAI board dept player)
  (local
    [
     (define gameTree (generateGameTree board dept))
     (define valuesOfNextMoves (evaulateNextMoves gameTree))
     ]
    (if (equal? player "o")
        
        (boardPermutation-board (list-ref (boardPermutation-nextBoards gameTree)
                                          (findElementIndex
                                           valuesOfNextMoves
                                           (findLowestValue valuesOfNextMoves
                                                            (first valuesOfNextMoves))
                                           0
                                           )
                
                                          )
                                )
        
        (boardPermutation-board (list-ref (boardPermutation-nextBoards gameTree)
                                          (findElementIndex
                                           valuesOfNextMoves
                                           (findMaxValue valuesOfNextMoves
                                                         (first valuesOfNextMoves))
                                           0
                                           )
                
                                          )
                                )
        )
    )
  )







(check-expect (findLowestValue (list 3 6 4) 3) 3)
(check-expect (findLowestValue (list 6 3 4) 6) 3)
(check-expect (findLowestValue (list 4 6 3) 4) 3)
(check-expect (findLowestValue (list 4 6 5) 4) 4)
(check-expect (findLowestValue (list 10 52 74.86 44.5) 10) 10)



(define (findLowestValue list lowest)
  (cond
    [(empty? list) lowest]
    [(< (first list) lowest)(findLowestValue (rest list) (first list)) ]
    [else
     (findLowestValue (rest list) lowest)
     ]
    )
  )





(check-expect (findMaxValue (list 3 6 4) 3) 6)
(check-expect (findMaxValue (list 6 3 4) 6) 6)
(check-expect (findMaxValue (list 4 6 3) 4) 6)
(check-expect (findMaxValue (list 4 3 5) 4) 5)
(check-expect (findMaxValue (list 10 52 74.86 44.5) 10) 74.86)


(define (findMaxValue list highest)
  (cond
    [(empty? list) highest]
    [(> (first list) highest)(findMaxValue (rest list) (first list)) ]
    [else
     (findMaxValue (rest list) highest)
     ]
    )
  )




(check-expect (findElementIndex (list 4 1 5.4) 4 0) 0)
(check-expect (findElementIndex (list 4 1 5.4) 1 0) 1)
(check-expect (findElementIndex (list 4 1 5.4) 5.4 0) 2)


(define (findElementIndex list value index)
  (cond
    [(empty? list) false]
    [(equal? (first list) value) index]
    [else (findElementIndex (rest list) value (add1 index))]
    )
  )







(check-expect (evaulateNextMoves
               (generateGameTree
                (list "x" "o" "x"
                      "" "x" "o"
                      "" "" "") 8))
              (list 2/11 1/11 2/11 1/11))

(check-expect (evaulateNextMoves
               (generateGameTree
                (list "x" "o" "x"
                      "o" "x" "o"
                      "o" "x" "") 8))
              (list 1))


(check-expect (evaulateNextMoves
               (generateGameTree
                (list "o" "x" "o"
                      "x" "o" "x"
                      "x" "x" "") 8))
              (list -1))


(check-expect (evaulateNextMoves
               (generateGameTree
                (list "o" "x" "o"
                      "" "o" "x"
                      "" "" "") 8))
              (list -1 -1)) 


 
(define (evaulateNextMoves gameTree)
  (local
    [
     (define VALUE 1)
     (define (findValues boards)
       (cond
         [(empty? boards) empty]
         [(equal? (first boards) empty ) empty]
         [else
          (cons
           (generateValue (first boards) 0 1)
           (findValues (rest boards) )
           )
          ]
         )
       )
     
     (define (generateValue aboardPermutation value modif)
       (if (or
            (string=? "ongoing" (updateGameOver (boardPermutation-board aboardPermutation)))
            (string=? "tie" (updateGameOver (boardPermutation-board aboardPermutation))))
           
           (fn-for-nextBoards (boardPermutation-nextBoards aboardPermutation )value modif)
           
           (cond
             [(string=? "won" (updateGameOver (boardPermutation-board aboardPermutation)))
              (fn-for-nextBoards (boardPermutation-nextBoards aboardPermutation )
                                 (+ 0 (/ 1 modif)) modif)
              ]
             [(string=? "lost" (updateGameOver (boardPermutation-board aboardPermutation)))
              (fn-for-nextBoards (boardPermutation-nextBoards aboardPermutation )
                                 (- (/ 1 modif)) modif)
              ]
             [else (error "how get here")]
             )
           ))

     (define (fn-for-nextBoards nextBoards value modif)
       (cond
         [(empty? nextBoards) value]
         [else
          (+
           (generateValue (first nextBoards) value (+ modif 10))
           (fn-for-nextBoards (rest nextBoards) value modif))]
         )
       )]
    (findValues (boardPermutation-nextBoards gameTree))
    )
  )













(check-expect (generateGameTree (list "o" "x" "x"
                                      "" "o" "x"
                                      "" "" "") 8)
              (make-boardPermutation
               (list "o" "x" "x" "" "o" "x" "" "" "")
               (list (make-boardPermutation (list "o" "x" "x" "" "o" "x" "" "" "o") '())))
              )

(check-expect (generateGameTree
               (list "o" "x" "o"
                     "" "" "x"
                     "" "" "x") 2)
              (make-boardPermutation
               (list "o" "x" "o" "" "" "x" "" "" "x")
               (list
                (make-boardPermutation (list "o" "x" "o" "o" "" "x" "" "" "x") '())
                (make-boardPermutation (list "o" "x" "o" "" "o" "x" "" "" "x") '())
                (make-boardPermutation (list "o" "x" "o" "" "" "x" "o" "" "x") '())
                (make-boardPermutation (list "o" "x" "o" "" "" "x" "" "o" "x") '())))
              )

(check-expect (generateGameTree
               (list "o" "x" "o"
                     "" "" "x"
                     "" "" "x") 3)
              (make-boardPermutation
               (list "o" "x" "o" "" "" "x" "" "" "x")
               (list
                (make-boardPermutation
                 (list "o" "x" "o" "o" "" "x" "" "" "x")
                 (list
                  (make-boardPermutation (list "o" "x" "o" "o" "x" "x" "" "" "x") '())
                  (make-boardPermutation (list "o" "x" "o" "o" "" "x" "x" "" "x") '())
                  (make-boardPermutation (list "o" "x" "o" "o" "" "x" "" "x" "x") '())))
                (make-boardPermutation
                 (list "o" "x" "o" "" "o" "x" "" "" "x")
                 (list
                  (make-boardPermutation (list "o" "x" "o" "x" "o" "x" "" "" "x") '())
                  (make-boardPermutation (list "o" "x" "o" "" "o" "x" "x" "" "x") '())
                  (make-boardPermutation (list "o" "x" "o" "" "o" "x" "" "x" "x") '())))
                (make-boardPermutation
                 (list "o" "x" "o" "" "" "x" "o" "" "x")
                 (list
                  (make-boardPermutation (list "o" "x" "o" "x" "" "x" "o" "" "x") '())
                  (make-boardPermutation (list "o" "x" "o" "" "x" "x" "o" "" "x") '())
                  (make-boardPermutation (list "o" "x" "o" "" "" "x" "o" "x" "x") '())))
                (make-boardPermutation
                 (list "o" "x" "o" "" "" "x" "" "o" "x")
                 (list
                  (make-boardPermutation (list "o" "x" "o" "x" "" "x" "" "o" "x") '())
                  (make-boardPermutation (list "o" "x" "o" "" "x" "x" "" "o" "x") '())
                  (make-boardPermutation (list "o" "x" "o" "" "" "x" "x" "o" "x") '())))))
              )


(define (generateGameTree board depth)
  (local
    [
     (define (makeboardPermutation aboardPermutation dept)
       (if  (or (= dept 1)
                (not
                 (string=? "ongoing" (updateGameOver (boardPermutation-board aboardPermutation)))
                 )
                )
            
            (make-boardPermutation
             (boardPermutation-board aboardPermutation)
             empty)
            
            (make-boardPermutation
             (boardPermutation-board aboardPermutation)
             (generateBoardtree (createBoardPermutation
                                 (boardPermutation-board aboardPermutation)
                                 )
                                dept
                                )
             )
            )
       )
     (define (generateBoardtree nextBoards dept)
       (cond
         [(empty? nextBoards) empty]
         [else
          (cons
           (makeboardPermutation (first nextBoards) (- dept 1))
           (generateBoardtree (rest nextBoards) dept))]
         )
       )]
    (makeboardPermutation (make-boardPermutation board empty) depth)
    )
  )





(check-expect (createBoardPermutation emptyBoard)
              (list
               (make-boardPermutation (list "x" "" "" "" "" "" "" "" "") empty)
               (make-boardPermutation (list "" "x" "" "" "" "" "" "" "") empty)
               (make-boardPermutation (list "" "" "x" "" "" "" "" "" "") empty)
               (make-boardPermutation (list "" "" "" "x" "" "" "" "" "") empty)
               (make-boardPermutation (list "" "" "" "" "x" "" "" "" "") empty)
               (make-boardPermutation (list "" "" "" "" "" "x" "" "" "") empty)
               (make-boardPermutation (list "" "" "" "" "" "" "x" "" "") empty)
               (make-boardPermutation (list "" "" "" "" "" "" "" "x" "") empty)
               (make-boardPermutation (list "" "" "" "" "" "" "" "" "x") empty)))
(check-expect (createBoardPermutation (list "" "" "x"
                                            "" "" ""
                                            "o" "x" ""))
              (list
               (make-boardPermutation (list "o" "" "x" "" "" "" "o" "x" "") empty)
               (make-boardPermutation (list "" "o" "x" "" "" "" "o" "x" "") empty)
               (make-boardPermutation (list "" "" "x" "o" "" "" "o" "x" "") empty)
               (make-boardPermutation (list "" "" "x" "" "o" "" "o" "x" "") empty)
               (make-boardPermutation (list "" "" "x" "" "" "o" "o" "x" "") empty)
               (make-boardPermutation (list "" "" "x" "" "" "" "o" "x" "o") empty)))



(define (createBoardPermutation board )
  (if (string=? "ongoing" (updateGameOver board))
      
      (if (isXturn? board)
          
          (createBoardList (keepOnlyDoneBoards (generatePermutations board "x")0)  )
          
            
          
          (createBoardList (keepOnlyDoneBoards (generatePermutations board "o")0) )
          )
      
      empty
      )
  )






(check-expect (keepOnlyDoneBoards (generatePermutations (list "x" "x" ""
                                                              "" "" ""
                                                              "" "" "") "x") 0)
              (list(list "x" "x" "x"
                         "" "" ""
                         "" "" ""))
              )
(check-expect (keepOnlyDoneBoards (generatePermutations (list "o" "o" ""
                                                              "" "" ""
                                                              "" "" "") "o") 0)
              (list(list "o" "o" "o"
                         "" "" ""
                         "" "" ""))
              )
(check-expect (keepOnlyDoneBoards (generatePermutations (list "x" "" ""
                                                              "" "" ""
                                                              "" "" "") "o") 0)
              (generatePermutations (list "x" "" ""
                                          "" "" ""
                                          "" "" "") "o")
              )


(define (keepOnlyDoneBoards boards doneBoards)
  (local [
          (define orgboards boards)
          (define (lookThoughBords boards doneBoards)
            (cond
              [(and (empty? boards) (= doneBoards 0)) orgboards]
              [(empty? boards) empty]
              [(not(string=? "ongoing" (updateGameOver (first boards))))
               (cons (first boards)(lookThoughBords (rest boards) (add1 doneBoards)))
               ]
              [else (lookThoughBords (rest boards) doneBoards)]
              )
            )
          ]
    (lookThoughBords boards doneBoards)
    )
  )





(check-expect (createBoardList (list (list "" "x" ""
                                           "" "" ""
                                           "" "" "")))
              (list (make-boardPermutation (list "" "x" ""
                                                 "" "" ""
                                                 "" "" "") empty)))
(check-expect (createBoardList (list (list "" "x" ""
                                           "" "" ""
                                           "" "" "")
                                     emptyBoard))
              (list (make-boardPermutation (list "" "x" ""
                                                 "" "" ""
                                                 "" "" "") empty)
                    (make-boardPermutation emptyBoard empty)))


(define (createBoardList boardlist ) 
  (cond
    [(empty? boardlist) empty]
    [else
     (cons
      (make-boardPermutation (first boardlist) empty)
      (createBoardList (rest boardlist)) 
      )
     ]
    )
  )







(check-expect (isXturn? (list "" "x" "x"
                              "o" "o" ""
                              "" "" "")) true)

(check-expect (isXturn? (list "" "x" "x"
                              "o" "o" ""
                              "" "" "x")) false)




(define (isXturn? board )
  (equal? (list (first (countX board 0 0))) (rest (countX board 0 0)))
  )





(check-expect (countX (list "" "x" "x"
                            "o" "o" ""
                            "" "" "") 0 0)
              (list 2 2))

(check-expect (countX (list "" "x" "x"
                            "o" "o" ""
                            "o" "x" "x") 0 0)
              (list 4 3))


(define (countX board count countO)
  (cond
    [(empty? board) (list count countO)]
    [(equal? "x" (first board)) (countX (rest board) (add1 count) countO)]
    [(equal? "o" (first board)) (countX (rest board)  count (add1 countO))]
    [else (countX (rest board) count countO)]
    )
  )