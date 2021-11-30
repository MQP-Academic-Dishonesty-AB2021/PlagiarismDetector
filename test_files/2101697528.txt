

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 4 v2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 400) 
(define MTS (empty-scene SIZE SIZE))




(define FONT-SIZE (round (/ SIZE 4))) 
(define X (text "X" FONT-SIZE "red"))
(define O (text "O" FONT-SIZE "blue"))

(define LINE-PEN (make-pen "black" (round (/ SIZE 30)) "solid" "round" "round"))
(define HORIZ-LINE (line (* 0.9 SIZE) 0 LINE-PEN))
(define VERT-LINE (line 0 (* 0.9 SIZE) LINE-PEN))

(define BOX-SIDE (/ SIZE 3)) 


(define EMPTY-BOARD
  (place-images (list
                 HORIZ-LINE
                 HORIZ-LINE
                 VERT-LINE
                 VERT-LINE)
                (list 
                 (make-posn (/ SIZE 2) (/ SIZE 3))
                 (make-posn (/ SIZE 2) (* 2 (/ SIZE 3)))
                 (make-posn (/ SIZE 3) (/ SIZE 2))
                 (make-posn (* 2 (/ SIZE 3)) (/ SIZE 2)))
                MTS))

(define B false) 
(define BLANK false)


(define ROWS
  (list (list 0 1 2)
        (list 3 4 5)
        (list 6 7 8)))

(define COLS
  (list (list 0 3 6)
        (list 1 4 7)
        (list 2 5 8)))

(define DIAG
  (list (list 0 4 8)
        (list 2 4 6)))

(define SOLUTIONS
  (append ROWS COLS DIAG))






























 


(define LOP-1 empty)
(define LOP0 (list B B B
                   B B B
                   B B B)) 

(define LOP1 (list  B   B   B
                    "X"  B   B
                    B   B   B)) 

(define LOP2 (list   B   B   B
                     "X"  B   B
                     "O"  B   B)) 

(define LOP3 (list   "X"    B   "X"
                     "X"    B    B
                     "O"   "O"   B)) 

(define LOP4 (list   "X"    B   "X"
                     "X"   "O"   B
                     "O"    B    B)) 

(define LOP5 (list   "X"    B    B
                     "X"   "O"  "X"
                     "O"    B    B)) 

(define LOP6 (list   "X"    B    B
                     "X"   "O"   B
                     B     B    B)) 



(define LOPSX1 (list "X" "X" "X"
                     B   B   B
                     "O" "O"  B)) 

(define LOPSX2 (list B  B  B
                     "X" "X" "X"
                     "O" "O" B)) 

(define LOPSX3 (list "O" "O" B
                     B B B
                     "X" "X" "X")) 

(define LOPSX4 (list "X" "O" B
                     "O" "X" B
                     B   B  "X")) 

(define LOPSO1 (list "O" "X" "X"
                     "O" "X" B
                     "O" B B)) 

(define LOPSO2 (list "X" "O" "X"
                     "X" "O" B
                     B   "O" B)) 

(define LOPSO3 (list "X" B "O"
                     "X" "X" "O"
                     B B    "O")) 

(define LOPSO4 (list "X" B "O"
                     "X" "O" "X"
                     "O" B  B)) 





(define LOPS1 (list "O" "X" "O"
                    "X" "X" "X"
                    "O" "X" "O")) 

(define LOPS2 (list "X" "X" "O"
                    "O" "X" "X"
                    "X" "O" "O")) 












(define-struct board (lop player-turn difficulty))

 



(define BD0
  (make-board
   LOP0 true 0)) 

(define BD1
  (make-board
   LOP1 false 0)) 

(define BD2
  (make-board 
   LOP2 true 0)) 

(define BD3
  (make-board 
   LOP3 false 1)) 

(define BD4
  (make-board 
   LOP4 false 1)) 

(define BD5
  (make-board 
   LOP5 false 1)) 

(define BD6
  (make-board
   LOP6 false 2)) 



(define BDSX1
  (make-board
   LOPSX1 false 0)) 

(define BDSX2
  (make-board
   LOPSX2 false 0)) 

(define BDSX3
  (make-board
   LOPSX3 false 0)) 

(define BDSX4
  (make-board
   LOPSX4 false 0)) 

(define BDSO1
  (make-board
   LOPSO1 true 0)) 

(define BDSO2
  (make-board
   LOPSO2 true 0)) 

(define BDSO3
  (make-board
   LOPSO3 true 0)) 

(define BDSO4
  (make-board
   LOPSO4 true 0)) 



(define BDS1
  (make-board
   LOPS1 false 0)) 

(define BDS2
  (make-board
   LOPS2 false 0)) 



(define START BD0)












(define (main bd)
  (big-bang bd
    (on-tick next-turn)
    (to-draw render-board)
    (on-mouse mouse-handler)
    (on-key key-handler)
    (stop-when stop-game? render-last)))










(check-expect (next-turn BD0) BD0) 
(check-expect (next-turn BD3) (make-board (list   "X"    B   "X"
                                                  "X"    B    B
                                                  "O"   "O"   "O") true 1))
(check-expect (next-turn BD5)
              (make-board (list   "X"    B    "O"
                                  "X"   "O"  "X"
                                  "O"    B    B) true 1))

(check-expect (next-turn (make-board (list    B    "X"   B
                                              B    "O"  "X"
                                              "X"    B   "O") false 1))
              (make-board (list   "O"    "X"   B
                                  B    "O"  "X"
                                  "X"    B   "O") true 1))

(check-expect (next-turn BD6)
              (make-board (list   "X"    B    B
                                  "X"   "O"   B
                                  "O"     B    B) true 2))

(check-expect (next-turn (make-board (list   "X"    B    B
                                             "O"   "X"   B
                                             "O"     B    B) false 2))
              (make-board (list   "X"    B    B
                                  "O"   "X"   B
                                  "O"     B  "O") true 2))       

(define (next-turn bd)
  (cond
    [(board-player-turn bd) bd]
    [(not (board-player-turn bd)) (computer-turn bd)]))












(check-expect (computer-turn BD3)
              (make-board (list   "X"    B   "X"
                                  "X"    B    B
                                  "O"   "O"   "O") true 1))
(check-expect (computer-turn BD5)
              (make-board (list   "X"    B    "O"
                                  "X"   "O"  "X"
                                  "O"    B    B) true 1))

(check-expect (computer-turn (make-board (list    B    "X"   B
                                                  B    "O"  "X"
                                                  "X"    B   "O") false 1))
              (make-board (list   "O"    "X"   B
                                  B    "O"  "X"
                                  "X"    B   "O") true 1))

(check-expect (computer-turn BD6)
              (make-board (list   "X"    B    B
                                  "X"   "O"   B
                                  "O"     B    B) true 2))

(check-expect (computer-turn (make-board (list   "X"    B    B
                                                 "O"   "X"   B
                                                 "O"     B    B) false 2))
              (make-board (list   "X"    B    B
                                  "O"   "X"   B
                                  "O"     B  "O") true 2))                 


(define (computer-turn bd)
  (local [
          
          

          (define (choose-random lon)            
            (local [
                    
                    
                    (define random-position (random (length lon)))
                    (define (choose-random lon pos)
                      (if (= random-position pos)
                          (first lon)
                          (choose-random (rest lon) (+ pos 1))))]
              (choose-random lon 0)))


          
          
          
          

          
          
          
          
          
          

          (define (win-in-1 bd player)
            (local [
                    
                    (define (produces-game-over? index)
                      (not (false? (game-over (insert-piece bd index player)))))
                    (define possibilities (indexes-of (board-lop bd) BLANK))
                    (define (win-in-1-in lon)
                      (cond
                        [(empty? lon) false]
                        [else
                         (if (produces-game-over? (first lon))
                             (first lon)
                             (win-in-1-in (rest lon)))]))]
              (win-in-1-in possibilities)))
          ]
    
    (insert-piece bd
                  (cond
                    [(= (board-difficulty bd) 0) (choose-random (indexes-of (board-lop bd) BLANK))]
                    [(= (board-difficulty bd) 1)

                     (local [(define win-move (win-in-1 bd "O"))]
                     
                       (if (not (false? win-move))
                           win-move
                           (choose-random (indexes-of (board-lop bd) BLANK))))]

                    [(= (board-difficulty bd) 2)

                     (local [(define win-move (win-in-1 bd "O"))
                             (define block-move (win-in-1 bd "X"))]
                     
                       (cond
                         [(not (false? win-move)) win-move]
                         [(not (false? block-move)) block-move]
                         [else (choose-random (indexes-of (board-lop bd) BLANK))]))])
                  "O")))











(check-expect (insert-piece BD0 0 "X")
              (make-board (list "X" B  B
                                B  B  B
                                B  B  B) false 0))

(check-expect (insert-piece (make-board (list "X" B  B
                                              B "O" B
                                              B  B  B) true 0) 8 "X")
              (make-board (list "X" B  B
                                B "O" B
                                B  B "X" ) false 0))

(check-expect (insert-piece (make-board (list "X" B  B
                                              B "O" B
                                              B  B "X") false 0) 5 "O")
              (make-board (list "X" B  B
                                B "O""O"
                                B  B "X" ) true 0))

(define (insert-piece bd pos player)
  (local [
          
          
          
          (define (new-lop lop pos player)
            
            (local [(define (new-lop lop pos player current-pos)
                      (cond [(empty? lop) empty]
                            [else
                             (if (= current-pos pos)
                                 (cons player
                                       (new-lop (rest lop) pos player (add1 current-pos)))
                                 (cons (first lop)
                                       (new-lop (rest lop) pos player (add1 current-pos))))]))]
              (new-lop lop pos player 0)))
          ]
    
    (make-board (new-lop (board-lop bd) pos player)
                (not (board-player-turn bd))
                (board-difficulty bd))))









(check-expect (render-board BD0) EMPTY-BOARD)

(check-expect (render-board BD1)
              (place-images
               (list X EMPTY-BOARD)
               (list
                (make-posn (/ SIZE 6) (/ SIZE 2))
                (make-posn (/ SIZE 2) (/ SIZE 2)))
               MTS))

(check-expect (render-board BD2)
              (place-images
               (list X O EMPTY-BOARD)
               (list
                (make-posn (/ SIZE 6) (/ SIZE 2))
                (make-posn (/ SIZE 6) (+ (* 2 BOX-SIDE) (/ SIZE 6)))
                (make-posn (/ SIZE 2) (/ SIZE 2)))
               MTS))


(define (render-board aboard)
  (local [
          (define (get-image square)
            (if (string=? "X" square) X O))
          (define (render-square board context)
            (cond
              [(empty? board) EMPTY-BOARD]
              [else
               (if (false? (first board))
                   (render-square (rest board) (add1 context))
                   (place-image (get-image (first board)) 
                                (+ (* (modulo context 3) (/ SIZE 3)) (/ SIZE 6))
                                (+ (* (floor (/ context 3)) (/ SIZE 3)) (/ SIZE 6))
                                (render-square (rest board) (add1 context))))]))]
    (render-square (board-lop aboard) 0)))









(check-expect (render-last BDSX1)
              (place-image (text "X WINS" FONT-SIZE "green") (/ SIZE 2) (/ SIZE 2)
                           (render-board BDSX1)))
(check-expect (render-last BDSO1)
              (place-image (text "O WINS" FONT-SIZE "green") (/ SIZE 2) (/ SIZE 2)
                           (render-board BDSO1)))
(check-expect (render-last BDS2)
              (place-image (text "DRAW" FONT-SIZE "green") (/ SIZE 2) (/ SIZE 2)
                           (render-board BDS2)))

(define (render-last bd)
  (cond [(string=? (game-over bd) "X")
         (place-image (text "X WINS" FONT-SIZE "green") (/ SIZE 2) (/ SIZE 2) (render-board bd))]
        [(string=? (game-over bd) "O")
         (place-image (text "O WINS" FONT-SIZE "green") (/ SIZE 2) (/ SIZE 2) (render-board bd))]
        [(string=? (game-over bd) "DRAW")
         (place-image (text "DRAW" FONT-SIZE "green") (/ SIZE 2) (/ SIZE 2) (render-board bd))]))











(check-expect (mouse-handler BD0 10 10 "button-up") BD0) 
(check-expect (mouse-handler BD1 10 10 "button-down") BD1) 

(check-expect (mouse-handler BD0 0 0 "button-down") 
              (make-board (list "X" B  B
                                B  B  B
                                B  B  B) false 0))
(check-expect (mouse-handler
               (make-board (list "X" B  B
                                 B "O" B
                                 B  B  B) true 0) SIZE SIZE "button-down")
              (make-board (list "X" B  B
                                B "O" B
                                B  B "X" ) false 0)) 

(check-expect (mouse-handler
               (make-board (list "X" B  B
                                 B "O" B
                                 B  B  B) true 0) SIZE SIZE "button-down")
              (make-board (list "X" B  B
                                B "O" B
                                B  B "X" ) false 0)) 

(check-expect (mouse-handler
               (make-board (list "X" B  B
                                 B "O" B
                                 B  B  B) true 0) (/ SIZE 2) (/ SIZE 2) "button-down")
              (make-board (list "X" B  B
                                B "O" B
                                B  B  B ) true 0)) 


(define (mouse-handler bd mouse-x mouse-y m-event)
  (if (mouse=? m-event "button-down")
      (local [(define pos (determine-pos mouse-x mouse-y))
              (define (piece-there? pos0)
                (if (not (false? (list-ref (board-lop bd) pos)))
                    false
                    true))
              ]
        (if (and (not (false? (board-player-turn bd)))
                 (piece-there? pos))
            (insert-piece bd pos "X")
            bd))
      bd))










(check-expect (determine-pos 0 0) 0) 
(check-expect (determine-pos (/ SIZE 6) (/ SIZE 6)) 0) 
(check-expect (determine-pos (/ SIZE 2) (/ SIZE 6)) 1) 
(check-expect (determine-pos (+ (* 2 BOX-SIDE) (/ SIZE 6)) (/ SIZE 6)) 2) 
(check-expect (determine-pos (/ SIZE 6) (/ SIZE 2)) 3) 
(check-expect (determine-pos (/ SIZE 2) (/ SIZE 2)) 4) 
(check-expect (determine-pos (+ (* 2 BOX-SIDE) (/ SIZE 6)) (/ SIZE 2)) 5) 
(check-expect (determine-pos (/ SIZE 6) (+ (* 2 BOX-SIDE) (/ SIZE 6))) 6) 
(check-expect (determine-pos (/ SIZE 2) (+ (* 2 BOX-SIDE) (/ SIZE 6))) 7) 
(check-expect (determine-pos (+ (* 2 BOX-SIDE) (/ SIZE 6))
                             (+ (* 2 BOX-SIDE) (/ SIZE 6))) 8) 
(check-expect (determine-pos SIZE SIZE) 8) 


(define (determine-pos x-pos y-pos)
  (local [
          
          (define (row-num y-pos)
            (cond [(<= y-pos BOX-SIDE) 0]
                  [(and (<= y-pos (* 2 BOX-SIDE))
                        (> y-pos BOX-SIDE)) 3]
                  [(> y-pos (* 2 BOX-SIDE)) 6]))
          
          (define (col-num x-pos)
            (cond [(<= x-pos BOX-SIDE) 0]
                  [(and (<= x-pos (* 2 BOX-SIDE))
                        (> x-pos BOX-SIDE)) 1]
                  [(> x-pos (* 2 BOX-SIDE)) 2]))
          ]

          
    (+ (row-num y-pos) (col-num x-pos))))








(check-expect (key-handler BD0 "0") BD0)
(check-expect (key-handler BD1 "1")
              (make-board LOP1 false 1))
(check-expect (key-handler BD1 "2")
              (make-board LOP1 false 2))

(define (key-handler bd ke)
  (cond [(key=? ke "0")
         (make-board (board-lop bd) (board-player-turn bd) 0)]
        [(key=? ke "1")
         (make-board (board-lop bd) (board-player-turn bd) 1)]
        [(key=? ke "2")
         (make-board (board-lop bd) (board-player-turn bd) 2)]
        [else bd]))









(check-expect (game-over BD0) false)
(check-expect (game-over BD1) false)
(check-expect (game-over BDSX1) "X")
(check-expect (game-over BDSX2) "X")
(check-expect (game-over BDSX3) "X")
(check-expect (game-over BDSX4) "X")
(check-expect (game-over BDSO1) "O")
(check-expect (game-over BDSO2) "O")
(check-expect (game-over BDSO3) "O")
(check-expect (game-over BDSO4) "O")
(check-expect (game-over BDS1) "X")
(check-expect (game-over BDS2) "DRAW")

(define (game-over bd)

  (local [
          
          (define (solution? sol)
  
            (cond
              [(andmap (λ (n) (and
                               (not (false? (list-ref (board-lop bd) n)))
                               (string=? "O" (list-ref (board-lop bd) n)))) sol) "O"]
          
              [(andmap (λ (n) (and
                               (not (false? (list-ref (board-lop bd) n)))
                               (string=? "X" (list-ref (board-lop bd) n)))) sol) "X"]
              [(andmap (λ (n) (not (false? n))) (board-lop bd)) "DRAW"]
        
        
              [else false]))
          
          (define los (filter (λ (n) (not (false? n))) (map solution? SOLUTIONS)))

          
          
          (define (check-for-solution los)
            (cond [(empty? los) "DRAW"]
                  [else
                   (if (string=? (first los) "DRAW")
                       (check-for-solution (rest los))
                       (first los))]))
                                

          ]
    
    (cond [(empty? los) false]
          [(string=? (first los) "DRAW")
           (check-for-solution los)]
          [else (first los)])))








(check-expect (stop-game? BDSX1) true)
(check-expect (stop-game? BDSO1) true)
(check-expect (stop-game? BDS1) true)
(check-expect (stop-game? BDS2) true)
(check-expect (stop-game? BD1) false)

(define (stop-game? bd)
  (if (not (false? (game-over bd)))
      true
      false))

                     
                     
                                         



