

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Combinde) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))

(define playerMarker (text "X" (/ SIZE 6) "red"))
(define cpuMarker (text "O" (/ SIZE 6) "blue"))
(define playerWin (text "Player WINS" (/ SIZE 6) "green"))
(define cpuWin (text "CPU WINS" (/ SIZE 6) "green"))
(define gameTied (text "TIED GAME" (/ SIZE 6) "green"))

(define closeVal (* 1 (/ SIZE 5)))
(define centerVal (/ SIZE 2))
(define farVal   (* 4 (/ SIZE 5)))

(define xCords (list closeVal centerVal farVal closeVal centerVal farVal closeVal centerVal farVal))
(define yCords (list closeVal closeVal closeVal centerVal centerVal centerVal farVal farVal farVal))



(define pen1 (make-pen "orange" (/ SIZE 20) "solid" "round" "round"))
(define boardMTS (add-line
                  (add-line
                   (add-line
                    (add-line MTS
                              (* 1 (/ SIZE 3)) (/ SIZE 8) (* 1 (/ SIZE 3)) (* 7 (/ SIZE 8)) pen1) 
                    (* 2 (/ SIZE 3)) (/ SIZE 8) (* 2 (/ SIZE 3)) (* 7 (/ SIZE 8)) pen1) 
                   
                   (/ SIZE 8) (* 1 (/ SIZE 3)) (* 7 (/ SIZE 8)) (* 1 (/ SIZE 3)) pen1) 
                  (/ SIZE 8) (* 2 (/ SIZE 3)) (* 7 (/ SIZE 8)) (* 2 (/ SIZE 3)) pen1))


(define-struct boardState (board playerTurn? gameStatus cpuInt))









(define BS1 (make-boardState 
             (list "B" "B" "B"
                   "B" "B" "B"
                   "B" "B" "B") true 0 0)) 

(define START BS1)

(define BS2 (make-boardState 
             (list "B" "B" "B" 
                   "B" "X" "B"
                   "B" "B" "B") false 0 0)) 

(define BS3 (make-boardState 
             (list "B" "B" "B" 
                   "O" "X" "B"
                   "O" "B" "X") true 0 0)) 

(define BS4 (make-boardState 
             (list "B" "B" "X" 
                   "O" "X" "B"
                   "O" "B" "X") false 0 0)) 

(define BS5 (make-boardState 
             (list "O" "X" "O" 
                   "B" "X" "B"
                   "X" "O" "X") false 0 0)) 

(define BS6 (make-boardState 
             (list "X" "B" "B" 
                   "B" "X" "B"
                   "O" "O" "X") false 1 0)) 

(define BS7 (make-boardState 
             (list "X" "X" "X" 
                   "B" "B" "B"
                   "O" "O" "B") false 1 0)) 

(define BS8 (make-boardState 
             (list "X" "O" "X" 
                   "B" "O" "X"
                   "X" "O" "B") true 2 0)) 

(define BS9 (make-boardState 
             (list "X" "X" "B" 
                   "B" "X" "X"
                   "O" "O" "O") true 2 0)) 

(define BS10 (make-boardState 
              (list "X" "X" "O" 
                    "O" "O" "X"
                    "X" "X" "O") false 3 0)) 

(define BS11 (make-boardState 
              (list "O" "O" "X" 
                    "X" "X" "O"
                    "O" "X" "X") false 3 0)) 
              
             
(define (fn-for-boardState bs) 
  (... 
   (boardState-board bs)
   (boardState-playerTurn? bs)
   (boardState-gameStatus bs) ))
   
   
(define (main ws)
  (big-bang ws
    (on-tick  updateBoard)  
    (to-draw  drawBoard)    
    (on-mouse mouseUpdate)  
    (on-key   keyUpdate)))  





(define ROWS      (list (list 0 1 2) (list 3 4 5) (list 6 7 8)))
(define COLUMNS   (list (list 0 3 6) (list 1 4 7) (list 2 5 8)))
(define DIAGONALS (list (list 0 4 8) (list 2 4 6)))

(define UNITS (append ROWS COLUMNS DIAGONALS))
  







(check-expect (getUnit BS1 0) (list "B" "B" "B")) 
(check-expect (getUnit BS1 7) (list "B" "B" "B")) 
(check-expect (getUnit BS8 3) (list "X" "B" "X")) 
(check-expect (getUnit BS8 6) (list "X" "O" "B")) 
(check-expect (getUnit BS4 1) (list "O" "X" "B")) 
(check-expect (getUnit BS3 0) (list "B" "B" "B")) 
(check-expect (getUnit BS9 2) (list "O" "O" "O")) 
(check-expect (getUnit BS10 7) (list "O" "O" "X"))
(check-expect (getUnit BS10 1) (list "O" "O" "X"))
(check-expect (getUnit BS11 2) (list "O" "X" "X"))

(define (getUnit bs index)
  (map (λ (i) (list-ref (boardState-board bs) i)) (list-ref UNITS index)))









(check-expect (computeGameStatus BS1) 0)
(check-expect (computeGameStatus BS2) 0)
(check-expect (computeGameStatus BS3) 0)
(check-expect (computeGameStatus BS4) 0)
(check-expect (computeGameStatus BS5) 0)
(check-expect (computeGameStatus BS6) 1)
(check-expect (computeGameStatus BS7) 1)
(check-expect (computeGameStatus BS8) 2)
(check-expect (computeGameStatus BS9) 2)
(check-expect (computeGameStatus BS10) 3)
(check-expect (computeGameStatus BS11) 3)


(define (computeGameStatus bs)
  (cond [(checkWin? bs "X") 1] 
        [(checkWin? bs "O") 2] 
        [(= (countEmptySpaces bs) 0) 3]  
        [else 0])) 
        






(check-expect (checkWin? BS1 "X") false) 
(check-expect (checkWin? BS6 "X") true) 
(check-expect (checkWin? BS7 "O") false) 
(check-expect (checkWin? BS10 "X") false) 
(check-expect (checkWin? BS10 "O") false) 
(check-expect (checkWin? BS6 "O") false) 
(check-expect (checkWin? BS3 "X") false) 

(define (checkWin? bs marker)
  
  (ormap (lambda (i) (wonUnit? (getUnit bs i) marker)) (build-list (length UNITS) identity)))








(check-expect (wonUnit? (list "B" "B" "B") "O") false)
(check-expect (wonUnit? (list "X" "X" "X") "O") false)
(check-expect (wonUnit? (list "O" "O" "X") "O") false)
(check-expect (wonUnit? (list "O" "O" "O") "O") true)
(check-expect (wonUnit? (list "O" "O" "O") "X") false)
(check-expect (wonUnit? (list "O" "O" "X") "X") false)
(check-expect (wonUnit? (list "X" "X" "X") "X") true)

(define (wonUnit? unit marker)
  (andmap (λ (s) (string=? s marker)) unit))









(check-expect (countEmptySpaces BS1) 9)
(check-expect (countEmptySpaces BS3) 5)
(check-expect (countEmptySpaces BS4) 4)
(check-expect (countEmptySpaces BS6) 4)
(check-expect (countEmptySpaces BS8) 2)
(check-expect (countEmptySpaces BS10) 0)

(define (countEmptySpaces bs)
  (length (getEmptySpaces bs)))

 






(check-expect (getEmptySpaces BS1) (list 0 1 2 3 4 5 6 7 8)) 
(check-expect (getEmptySpaces BS11) empty) 
(check-expect (getEmptySpaces BS4) (list 0 1 5 7))
(check-expect (getEmptySpaces BS6) (list 1 2 3 5))
(check-expect (getEmptySpaces BS8) (list 3 8))
(check-expect (getEmptySpaces BS9) (list 2 3))
(check-expect (getEmptySpaces BS2) (list 0 1 2 3 5 6 7 8)) 

(define (getEmptySpaces bs)
  
  (filter (lambda (i) (string=? (list-ref (boardState-board bs) i) "B")) (build-list (length (boardState-board bs)) identity)) )

 








(define (computerTurn bs)
  (placeOnBoard "O" (computerPlacePos bs) bs))









(check-expect (placeOnBoard "X" 4 BS1) BS2)
(check-expect (placeOnBoard "X" 2 BS3) BS4)
(check-expect (placeOnBoard "O" 0 BS4) 
              (make-boardState 
               (list "O" "B" "X" 
                     "O" "X" "B"
                     "O" "B" "X") (not (boardState-playerTurn? BS4)) (boardState-gameStatus BS4) (boardState-cpuInt BS4)))
(check-expect (placeOnBoard "O" 5 BS5) 
              (make-boardState 
               (list "O" "X" "O" 
                     "B" "X" "O"
                     "X" "O" "X") true 0 0))
(check-expect (placeOnBoard "X" 8 BS1) 
              (make-boardState 
               (list "B" "B" "B" 
                     "B" "B" "B"
                     "B" "B" "X") (not (boardState-playerTurn? BS1)) (boardState-gameStatus BS1) (boardState-cpuInt BS1)))

(define (placeOnBoard marker index bs)
  (make-boardState (newBoard marker index (boardState-board bs))
                   (not (boardState-playerTurn? bs))
                   (boardState-gameStatus bs)
                   (boardState-cpuInt bs)))









(define (newBoard marker targetIndex board)
  (local [(define (newBoard-inner marker targetIndex board currentIndex)
            (cond [(empty? board) empty]
                  [(= targetIndex currentIndex) (cons marker (newBoard-inner marker targetIndex (rest board) (add1 currentIndex)))]
                  [else (cons (first board) (newBoard-inner marker targetIndex (rest board) (add1 currentIndex)))]))]
    (newBoard-inner marker targetIndex board 0)))
                  
                   










(define (computerPlacePos bs)
  (local [(define intLvl (boardState-cpuInt bs))]
    (cond [(= intLvl 0) (list-ref (getEmptySpaces bs) (random (length (getEmptySpaces bs))))]
          [else
           (minMaxMove bs)])))












(define (updateBoard bs)
  (if (= (computeGameStatus bs) 0)
      (if (boardState-playerTurn? bs)
          bs 
          (computerTurn bs)) 
      (make-boardState (boardState-board bs)
                       (boardState-playerTurn? bs)
                       (computeGameStatus bs)
                       (boardState-cpuInt bs))))








(define (drawBoard bs)
  (local [(define (drawBoard-inner board xVals yVals)
            (cond [(empty? board) boardMTS]
                  [else
                   (place-image/align (chooseImage (first board)) (first xVals) (first yVals) "center" "center"
                                      (drawBoard-inner (rest board) (rest xVals) (rest yVals)))]))
          (define dbi (drawBoard-inner (boardState-board bs) xCords yCords))]
         
  
    (cond [(= (computeGameStatus bs) 0) dbi]
          [(= (computeGameStatus bs) 1) 
           (place-image/align playerWin (/ SIZE 2) (/ SIZE 2) "center" "center" dbi)]
          [(= (computeGameStatus bs) 2)
           (place-image/align cpuWin (/ SIZE 2) (/ SIZE 2) "center" "center" dbi)]
          [else 
           (place-image/align gameTied (/ SIZE 2) (/ SIZE 2) "center" "center" dbi)])))







  

  
  






(check-expect (chooseImage "X") playerMarker)
(check-expect (chooseImage "O") cpuMarker)
(check-expect (chooseImage "B") empty-image)

(define (chooseImage marker)
  (cond [(string=? marker "X") playerMarker]
        [(string=? marker "O") cpuMarker]
        [else empty-image]))








(check-expect (mouseUpdate BS1 1 1 "button-up")
              (make-boardState 
             (list "X" "B" "B"
                   "B" "B" "B"
                   "B" "B" "B") false 0 0))
(check-expect (mouseUpdate BS3 1 (/ SIZE 2) "button-up") BS3)
(check-expect (mouseUpdate BS1 (/ SIZE 2) 1 "button-down") BS1)
(check-expect (mouseUpdate BS4 (- SIZE 1) 1 "button-up") BS4)
(check-expect (mouseUpdate BS6 (- SIZE 1) (/ SIZE 2) "button-up") BS6)

(define (mouseUpdate bs x-cor y-cor me)
  (if (and (mouse=? me "button-up") (boardState-playerTurn? bs) (= (boardState-gameStatus bs) 0))
      (local [(define index (isValid? (boardState-board bs) (clickToIndex x-cor y-cor)))]
        (if (number? index)
            (placeOnBoard "X" index bs)
            bs))
      bs))





(check-expect (isValid? (boardState-board BS1) 0) 0)
(check-expect (isValid? (boardState-board BS3) 2) 2)
(check-expect (isValid? (boardState-board BS4) 6) false)
(check-expect (isValid? (boardState-board BS7) 0) false)
(check-expect (isValid? (boardState-board BS2) 4) false)
(check-expect (isValid? (boardState-board BS8) 8) 8)
(check-expect (isValid? (boardState-board BS9) 2) 2) 

(define (isValid? board index)
  (if (string=? (list-ref board index) "B")
      index
      false))








(check-expect (clickToIndex 1                   1) 0)
(check-expect (clickToIndex (/ SIZE 2)          1) 1)
(check-expect (clickToIndex (- SIZE 1)          1) 2)
(check-expect (clickToIndex 1          (/ SIZE 2)) 3)
(check-expect (clickToIndex (/ SIZE 2) (/ SIZE 2)) 4)
(check-expect (clickToIndex (- SIZE 1) (/ SIZE 2)) 5)
(check-expect (clickToIndex 1          (- SIZE 1)) 6)
(check-expect (clickToIndex (/ SIZE 2) (- SIZE 1)) 7)
(check-expect (clickToIndex (- SIZE 1) (- SIZE 1)) 8)


(define (clickToIndex x-cor y-cor)
  (cond [(< y-cor (* 1 (/ SIZE 3)))
         (cond [(< x-cor (* 1 (/ SIZE 3))) 0]
               [(> x-cor (* 2 (/ SIZE 3))) 2]
               [else 1])]
        [(> y-cor (* 2 (/ SIZE 3)))
         (cond [(< x-cor (* 1 (/ SIZE 3))) 6]
               [(> x-cor (* 2 (/ SIZE 3))) 8]
               [else 7])]
        [else
         (cond [(< x-cor (* 1 (/ SIZE 3))) 3]
               [(> x-cor (* 2 (/ SIZE 3))) 5]
               [else 4])]))






(check-expect (keyUpdate BS1 "2") 
              (make-boardState (boardState-board BS1)
                       (boardState-playerTurn? BS1)
                       (computeGameStatus BS1)
                       2))

(check-expect (keyUpdate BS9 "9") 
              (make-boardState (boardState-board BS9)
                       (boardState-playerTurn? BS9)
                       (computeGameStatus BS9)
                       9))

(check-expect (keyUpdate BS2 "A") BS2)
              
(define (keyUpdate bs ke)
  (cond [(number? (string->number ke))
         (make-boardState (boardState-board bs)
                       (boardState-playerTurn? bs)
                       (computeGameStatus bs)
                       (string->number ke))]
        [else bs]))
















(define-struct gameTree (gameState score isMaxsTurn? nextGameStates))













(define (getScore aGameTree)
  (if (isEndNode? aGameTree)
      (scoreFromState (gameTree-gameState aGameTree))
      (if (gameTree-isMaxsTurn? aGameTree)
          (apply max (getScoresList (gameTree-nextGameStates aGameTree)))
          (apply min (getScoresList (gameTree-nextGameStates aGameTree))))))





(define (isEndNode? aGameTree)
  (or (isEndGameState?(gameTree-gameState aGameTree)) (empty? (gameTree-nextGameStates aGameTree))))

(define (isEndGameState? aGameState) 
  (not (= 0 (computeGameStatus aGameState))))










(define (scoreFromState aGameState)
  (local [(define gameVal  (computeGameStatus aGameState))]
    (cond [(= gameVal 1) 1] 
          [(= gameVal 2) 0] 
          [else 0.5])))     
  




(define (getScoresList listOfGameTree)
  (cond [(empty? listOfGameTree) empty]
        [else
         (cons (getScore (first listOfGameTree))
               (getScoresList (rest listOfGameTree)))]))










(define (generateNextGameStates aGameState isMaxTurn?)
  (gameStatesFromOpenSpots aGameState (getEmptySpaces aGameState) isMaxTurn?))






(define (updateGameStatus aGameState)
  (make-boardState (boardState-board aGameState) (boardState-playerTurn? aGameState) (computeGameStatus aGameState) (boardState-cpuInt aGameState)))










(define (gameStatesFromOpenSpots aGameState openSpots isMaxTurn?)
  (cond [(empty? openSpots) empty]
        [else
         (local [(define playerSymbol (if isMaxTurn? "X" "O"))]
           (cons (updateGameStatus (placeOnBoard playerSymbol (first openSpots) aGameState))
                 (gameStatesFromOpenSpots aGameState (rest openSpots) isMaxTurn?)))]))








(define (generateMovesTree aGameState isMaxsTurn? depth)
  (updateListChildren (gameStateListToGameTreeList (generateNextGameStates aGameState isMaxsTurn?) isMaxsTurn?) depth))








(define (gameStateListToGameTreeList gsl isMaxsTurn?)
  (cond [(empty? gsl) empty]
        [else
         (cons
          (gameStateToGameTree (first gsl) isMaxsTurn?)
          (gameStateListToGameTreeList (rest gsl) isMaxsTurn?))]))





(define (gameStateToGameTree aGameState isMaxsTurn?)
  (make-gameTree aGameState 0.5 (not isMaxsTurn?) empty))






  
(define (updateListChildren allMoves depth)
  (cond [(empty? allMoves) empty]
        [else
         
         
         (if (= depth 1)
             (cons (first allMoves)
                   (updateListChildren  (rest allMoves) depth)) 
      
             (cons (make-gameTree
                    (gameTree-gameState    (first allMoves))
                    (gameTree-score        (first allMoves))
                    (gameTree-isMaxsTurn?  (first allMoves)) 
             
                    (generateMovesTree
                     (gameTree-gameState   (first allMoves))
                     (gameTree-isMaxsTurn? (first allMoves))
                     (- depth 1)) ) 
       
                   (updateListChildren  (rest allMoves) depth)))])) 
      








(define (indexOfMax lon)
  (index (apply max lon) lon))

(define (indexOfMin lon)
  (index (apply min lon) lon))






(define (index num lon)
  (local [(define (indexOf num lon currentIndex)
            (cond [(empty? lon) "Tried to get index of something that wasn't in the list"]
                  [else
                   (if (= num (first lon))
                       currentIndex
                       (indexOf num (rest lon) (add1 currentIndex)))]))]
    (indexOf num lon 0)))


(define (minMaxMove aGameState)
  (local [
          (define turn (boardState-playerTurn? aGameState))
          (define gameTree1 (make-gameTree aGameState 0 turn (generateMovesTree aGameState turn (boardState-cpuInt aGameState))))]
    (if (boardState-playerTurn? aGameState)
    (list-ref (getEmptySpaces aGameState) (indexOfMax (getScoresList (gameTree-nextGameStates gameTree1))))
    (list-ref (getEmptySpaces aGameState) (indexOfMin (getScoresList (gameTree-nextGameStates gameTree1)))))))



(define BS12 (make-boardState 
              (list "B" "B" "B" 
                    "B" "X" "B"
                    "O" "X" "B") false 0 2)) 


(define BS13 (make-boardState 
              (list "B" "B" "B" 
                    "O" "X" "B"
                    "X" "B" "B") false 0 9)) 

(define BS14 (make-boardState 
              (list "B" "B" "B" 
                    "O" "X" "B"
                    "B" "B" "B") true 0 9)) 

(define BS15 (make-boardState 
              (list "X" "B" "B" 
                    "O" "B" "B"
                    "B" "B" "B") true 0 9)) 

(define BS16 (make-boardState 
              (list "B" "B" "B" 
                    "B" "B" "B"
                    "B" "B" "B") false 0 1))















