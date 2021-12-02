

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname FINALKYLIEANNAASSIGN4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 600) 
(define FONT-SIZE (/ SIZE 6))

(define s1 (square (/ SIZE 3) "outline" "orange"))
(define r1 (beside s1 s1 s1))
(define b1 (above r1 r1 r1))

(define MTS (place-image b1 (/ SIZE 2) (/ SIZE 2) (empty-scene SIZE SIZE)))

(define V1 (/ SIZE 3))
(define V2 (* 2 (/ SIZE 3)))
(define H1 (/ SIZE 3))
(define H2 (* 2 (/ SIZE 3)))













 
























(define SIZE-OF-ICON (/ (/ SIZE 6) 100))

(define X .) 

(define X-IMG (scale SIZE-OF-ICON X))
(define O .) 
(define O-IMG (scale SIZE-OF-ICON O))




  
(define-struct Pos (index))





(define (r-c->pos r c) (+ (* r 3) c))



(define-struct Box (pos img))






(define-struct Game (board diff))







(define B false) 

(define EMPTYBOX1 (make-Box 0 B))
(define EMPTYBOX2 (make-Box 1 B))
(define EMPTYBOX3 (make-Box 2 B))
(define EMPTYBOX4 (make-Box 3 B))
(define EMPTYBOX5 (make-Box 4 B))
(define EMPTYBOX6 (make-Box 5 B))
(define EMPTYBOX7 (make-Box 6 B))
(define EMPTYBOX8 (make-Box 7 B))
(define EMPTYBOX9 (make-Box 8 B))
    

(define EMPTYBOARD
  (list EMPTYBOX1 EMPTYBOX2 EMPTYBOX3
        EMPTYBOX4 EMPTYBOX5 EMPTYBOX6
        EMPTYBOX7 EMPTYBOX8 EMPTYBOX9))

(define BOARD1  
  (list EMPTYBOX1 EMPTYBOX2 (make-Box 2 X-IMG)
        (make-Box 3 O-IMG) EMPTYBOX5 EMPTYBOX6
        (make-Box 6 O-IMG) EMPTYBOX7 (make-Box 8 X-IMG)))

(define BOARD2  
  (list (make-Box 0 O-IMG) EMPTYBOX2 (make-Box 2 X-IMG)
        (make-Box 3 O-IMG) EMPTYBOX5 EMPTYBOX6
        (make-Box 6 O-IMG) EMPTYBOX7 (make-Box 8 X-IMG)))

(define BOARD3  
  (list (make-Box 0 X-IMG) (make-Box 1 X-IMG) (make-Box 2 X-IMG)
        EMPTYBOX4 EMPTYBOX5 EMPTYBOX6
        (make-Box 6 O-IMG) EMPTYBOX7 (make-Box 8 X-IMG)))

(define BOARD4  
  (list (make-Box 0 X-IMG) EMPTYBOX2 (make-Box 2 X-IMG)
        EMPTYBOX4 (make-Box 4 X-IMG) EMPTYBOX6
        (make-Box 6 O-IMG) EMPTYBOX7 (make-Box 8 X-IMG)))

(define BOARD5 
  (list EMPTYBOX1 EMPTYBOX2 (make-Box 2 X-IMG)
        EMPTYBOX4 EMPTYBOX5 EMPTYBOX6
        (make-Box 6 O-IMG) EMPTYBOX8 (make-Box 8 X-IMG)))
         

(define FULLBOARD
  (list (make-Box 0 X-IMG) (make-Box 1 O-IMG) (make-Box 2 X-IMG)
        (make-Box 3 X-IMG) (make-Box 4 O-IMG) (make-Box 5 O-IMG)
        (make-Box 6 O-IMG) (make-Box 7 X-IMG) (make-Box 8 X-IMG)))
  
(define ROWS
  (list (list 0 1 2)
        (list 3 4 5)
        (list 6 7 8)))

(define COLS
  (list (list 0 3 6)
        (list 1 4 7)
        (list 2 5 8)))

(define DIAGS
  (list (list 0 4 8)
        (list 2 4 6)))

(define START (make-Game EMPTYBOARD 0))

(define GAME1 
  (make-Game BOARD1 1))
(define GAME2 
  (make-Game BOARD5 2))
  






(define (main Game)
  (big-bang Game
    (on-mouse update-game)                 
    (to-draw render)                       
    (on-key choose-diff)                   
    (stop-when is-finished last-board)))   
    







 


(check-expect (update-game GAME1 (/ SIZE 4) (/ SIZE 4) "button-down")           
              (make-Game (list (make-Box 0 X-IMG) EMPTYBOX2 (make-Box 2 X-IMG)
                               (make-Box 3 O-IMG) EMPTYBOX5 EMPTYBOX6
                               (make-Box 6 O-IMG) EMPTYBOX7 (make-Box 8 X-IMG))
                         (Game-diff GAME1)))

(check-expect (update-game GAME2 (/ SIZE 2) (/ SIZE 2) "button-down")           
              (make-Game (list EMPTYBOX1 EMPTYBOX2 (make-Box 2 X-IMG)
                               EMPTYBOX4 (make-Box 4 X-IMG) EMPTYBOX6
                               (make-Box 6 O-IMG) EMPTYBOX8 (make-Box 8 X-IMG))
                         (Game-diff GAME2)))

(check-expect (update-game GAME2 (/ SIZE 2) (/ SIZE 2) "drag")                  
              GAME2)




(define (update-game game x y mouse-event)
  (local [
          (define (which-box x0 y0)
            (local [(define (getc mouse-x)
                      (cond [(<= mouse-x V1) 0]
                            [(>= mouse-x V2) 2]
                            [else 1]))

                    (define (getr mouse-y)
                      (cond [(<= mouse-y H1) 0]
                            [(>= mouse-y H2) 2]
                            [else 1]))]
              
              (r-c->pos (getr y0) (getc x0))))
          
          (define (computer-move pos game0)
            (make-move pos game0 O-IMG))]
    
    (cond [(string=? mouse-event "button-down")
           (if (isvalid-move (which-box x y) game)
               (make-move (which-box x y) game X-IMG)
               game)]
          
          [(string=? mouse-event "button-up")
           (if (image=? (Box-img (list-ref (Game-board game) (which-box x y)))
                        X-IMG)
               (computer-move (pick-best-move game O-IMG) game)
               game)] 
          [else
           game])))







(check-expect (render GAME1) (place-images (list X-IMG O-IMG O-IMG X-IMG)
                                           (list
                                            (make-posn
                                             (- (* (/ (add1 (modulo 2 3)) 3) SIZE) (/ SIZE 6))
                                             (- H1 (/ SIZE 6)))
                                            (make-posn
                                             (- (* (/ (add1 (modulo 3 3)) 3) SIZE) (/ SIZE 6))
                                             (- H2 (/ SIZE 6)))
                                            (make-posn
                                             (- (* (/ (add1 (modulo 6 3)) 3) SIZE) (/ SIZE 6))
                                             (- SIZE (/ SIZE 6)))
                                            (make-posn
                                             (- (* (/ (add1 (modulo 8 3)) 3) SIZE) (/ SIZE 6))
                                             (- SIZE (/ SIZE 6))))
                                           MTS))



(define (render game)
  (local [
          (define (find-images game0)     
            (make-Game (filter (λ (n) (not (false? (Box-img n)))) (Game-board game0)) (Game-diff game0)))
            
          (define (find-x box)            
            (- (* (/ (add1 (modulo (Box-pos box) 3)) 3) SIZE) (/ SIZE 6)))

          (define (find-y box)            
            (cond [(>= (Box-pos box) 6) (- SIZE (/ SIZE 6))]
                  [(>= (Box-pos box) 3) (- H2 (/ SIZE 6))]
                  [(>= (Box-pos box) 0) (- H1 (/ SIZE 6))]))]
    
    (cond [(empty? (Game-board (find-images game))) MTS]
          [else
           (place-image (Box-img (first (Game-board (find-images game))))
                        (find-x  (first (Game-board (find-images game))))
                        (find-y  (first (Game-board (find-images game))))
                        (render  (make-Game (rest (Game-board (find-images game))) (Game-diff game))))])))








(check-expect (choose-diff GAME1 "0") (make-Game (Game-board GAME1) 0)) 
(check-expect (choose-diff GAME1 "1") (make-Game (Game-board GAME1) 1)) 
(check-expect (choose-diff GAME1 "2") (make-Game (Game-board GAME1) 2)) 
(check-expect (choose-diff GAME1 "a") (make-Game (Game-board GAME1) 1)) 

(define (choose-diff game key)
  (if (key-event? key)
      (cond [(string=? key "0") (make-Game (Game-board game) 0)]
            [(string=? key "1") (make-Game (Game-board game) 1)]
            [(string=? key "2") (make-Game (Game-board game) 2)]
            [else game]
            )
      game))









(check-expect (is-finished (make-Game BOARD3 1)) true)                   
(check-expect (is-finished (make-Game BOARD1 1)) false)                  
(check-expect (is-finished (make-Game FULLBOARD 1)) true)                
(check-expect (is-finished (make-Game EMPTYBOARD 0)) false)              

(define (is-finished game)
  (cond [(board-full? game) true] 
        [(who-won? game X-IMG) true]
        [(who-won? game O-IMG) true]
        [else
         false]))








(check-expect (last-board (make-Game BOARD3 1))                                     
              (place-image (text "X WON" FONT-SIZE "green") (/ SIZE 2) (/ SIZE 2)
                           (render (make-Game BOARD3 2))))
(check-expect (last-board (make-Game BOARD2 1))                                     
              (place-image (text "O WON" FONT-SIZE "red") (/ SIZE 2) (/ SIZE 2)
                           (render (make-Game BOARD2 1))))
(check-expect (last-board (make-Game FULLBOARD 0))                                  
              (place-image (text "DRAW" FONT-SIZE "black") (/ SIZE 2) (/ SIZE 2)
                           (render (make-Game FULLBOARD 0))))
(check-expect (last-board (make-Game BOARD5 0))                                     
              (render (make-Game BOARD5 0)))


(define (last-board game)
  (cond [(who-won? game X-IMG)
         (place-image (text "X WON" FONT-SIZE "green") (/ SIZE 2) (/ SIZE 2) (render game))]
        [(who-won? game O-IMG)
         (place-image (text "O WON" FONT-SIZE "red") (/ SIZE 2) (/ SIZE 2) (render game))]
        [(board-full? game) (place-image (text "DRAW" FONT-SIZE "black") (/ SIZE 2) (/ SIZE 2) (render game))]
        [else (render game)]))






(check-expect (who-won? (make-Game BOARD3 1) X-IMG) true)  
(check-expect (who-won? (make-Game BOARD2 1) O-IMG) true)  
(check-expect (who-won? (make-Game BOARD5 1) O-IMG) false) 

(define (who-won? game img)
  (cond [(three-in-row? game ROWS img) true]
        [(three-in-row? game COLS img) true]
        [(three-in-row? game DIAGS img) true]
        [else false]))






(check-expect (board-full? (make-Game FULLBOARD 1)) true)      
(check-expect (board-full? (make-Game EMPTYBOARD 1)) false)    
(check-expect (board-full? (make-Game BOARD5 1)) false)        

(define (board-full? game)
  (local [
          (define (length lst)
            (cond
              [(empty? lst)  0]
              [(cons? lst)   (+ 1 (length (rest lst)))]))
          
          (define (find-images game0)     
            (filter (λ (n) (not (false? (Box-img n)))) (Game-board game0)))]
    
    (if (= (length (find-images game)) 9)
        true
        false)))







(check-expect (three-in-row? (make-Game BOARD1 0) ROWS O-IMG) false) 
(check-expect (three-in-row? (make-Game BOARD3 2) ROWS X-IMG) true)  
(check-expect (three-in-row? (make-Game BOARD2 0) COLS O-IMG) true)  
(check-expect (three-in-row? (make-Game BOARD3 0) COLS O-IMG) false) 
(check-expect (three-in-row? (make-Game BOARD4 0) DIAGS X-IMG) true) 
(check-expect (three-in-row? (make-Game BOARD3 2) DIAGS X-IMG) false) 

(define (three-in-row? game section img)
  (local [
          (define (for-section game0 lop img0)
            (cond [(empty? lop) true]
                  [else
                   (if (false? (Box-img (list-ref (Game-board game0) (first lop))))
                       false
                       (if (image=? (Box-img (list-ref (Game-board game0) (first lop))) img0)
                           (for-section game0 (rest lop) img0)
                           false))]))

          (define (for-board game0 lop img0)
            (cond [(empty? lop) false]
                  [else
                   (if (for-section game0 (first lop) img0)
                       true
                       (for-board game0 (rest lop) img0))]))]

    (for-board game section img)))







(check-expect (pick-best-move GAME2 O-IMG) 5) 
(check-expect (pick-best-move GAME2 X-IMG) 5) 
(check-expect (pick-best-move GAME1 O-IMG) 0) 
(check-expect (pick-best-move GAME1 X-IMG) 5) 


(define (pick-best-move game img)
  (local [
          (define (rand-pos num game0) 
            (local [
                    (define RAND (floor (random num)))]
              
              (if (isvalid-move RAND game0)
                  RAND
                  (rand-pos num game0))))
          (define (list-valid game0 img0 n lovp) 
            (cond [(> n 8) lovp] 
                  [else
                   (if (isvalid-move n game0)
                       (list-valid game0 img0 (add1 n) (append (list n) lovp))
                       (list-valid game0 img0 (add1 n) lovp))]))
          
          (define (find-win game0 lop img0)  
            (cond [(empty? lop) false]
                  [(who-won? (make-move (first lop) game0 img0) img0) (first lop)]
                  [else
                   (find-win game0 (rest lop) img0)]))]
    
    (cond [(= (Game-diff game) 1)
           (if (false? (find-win game (list-valid game img 0 empty) img))
               (rand-pos 9 game) 
               (find-win game (list-valid game img 0 empty) img))]
          [(= (Game-diff game) 2)
           (if (false? (find-win game (list-valid game img 0 empty) img))
               (if (false? (find-win game (list-valid game X-IMG 0 empty) X-IMG))
                   (rand-pos 9 game)
                   (find-win game (list-valid game X-IMG 0 empty) X-IMG))
               (find-win game (list-valid game img 0 empty) img))]
          [else
           (rand-pos 9 game)])))

 






(check-expect (isvalid-move 0 (make-Game BOARD2 1)) 
              false) 
(check-expect (isvalid-move 1 (make-Game BOARD2 1))  
              true)

               
(define (isvalid-move pos game)
  (if (image? (Box-img (list-ref (Game-board game) pos)))
      false
      true))
  






(check-expect (make-move 0 (make-Game EMPTYBOARD 1) O-IMG)
              (make-Game (list (make-Box 0 O-IMG) EMPTYBOX2 EMPTYBOX3
                               EMPTYBOX4 EMPTYBOX5 EMPTYBOX6
                               EMPTYBOX7 EMPTYBOX8 EMPTYBOX9) 1))



(define (make-move pos game img)
  (make-Game 
   (append
    (take (Game-board game) pos)
    (list (make-Box pos img))
    (drop (Game-board game) (add1 pos))) (Game-diff game)))