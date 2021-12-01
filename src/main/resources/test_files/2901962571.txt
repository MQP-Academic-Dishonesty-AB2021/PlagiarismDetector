

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)








(define-struct box (value x y))








(define-struct board (boxes turn winner diff))



(define START (make-board 
               (list 
                (make-box false 0 0) (make-box false 1 0) (make-box false 2 0)
                (make-box false 0 1) (make-box false 1 1) (make-box false 2 1) 
                (make-box false 0 2) (make-box false 1 2) (make-box false 2 2))
               true false 0)) 


(define empty-board (make-board (list 
                                 (make-box false 0 0)
                                 (make-box false 1 0)
                                 (make-box false 2 0)
                                 (make-box false 0 1) 
                                 (make-box false 1 1) 
                                 (make-box false 2 1) 
                                 (make-box false 0 2) 
                                 (make-box false 1 2) 
                                 (make-box false 2 2))
                                true false 0))

(define empty-board-computer (make-board (list 
                                          (make-box false 0 0)
                                          (make-box false 1 0)
                                          (make-box false 2 0)
                                          (make-box false 0 1) 
                                          (make-box false 1 1) 
                                          (make-box false 2 1) 
                                          (make-box false 0 2) 
                                          (make-box false 1 2) 
                                          (make-box false 2 2))
                                         false false 0))



(define example-board-neutral 
  (make-board (list 
               (make-box false 0 0) (make-box "x" 1 0)   (make-box false 2 0)
               (make-box "x" 0 1)   (make-box "o" 1 1)   (make-box false 2 1) 
               (make-box "x" 0 2)   (make-box false 1 2) (make-box "o" 2 2))
                  true false 0))

(define example-board-xwin 
  (make-board (list 
               (make-box "x" 0 0)   (make-box false 1 0) (make-box false 2 0)
               (make-box false 0 1) (make-box "x" 1 1)   (make-box "o" 2 1) 
               (make-box "o" 0 2)   (make-box false 1 2) (make-box "x" 2 2))
                 true "X wins" 0))

(define example-board-owin
  (make-board (list 
               (make-box "o" 0 0)   (make-box "o" 1 0) (make-box "o" 2 0)
               (make-box false 0 1) (make-box "x" 1 1) (make-box "x" 2 1) 
               (make-box false 0 2) (make-box "x" 1 2) (make-box false 2 2))
                 false "O wins" 0))

(define example-board-owin-diff-1
  (make-board (list 
               (make-box false 0 0)   (make-box "o" 1 0) (make-box "o" 2 0)
               (make-box false 0 1) (make-box "x" 1 1) (make-box "x" 2 1) 
               (make-box false 0 2) (make-box "x" 1 2) (make-box false 2 2))
                 false false 1))

(define example-board-owin-diff-2
  (make-board (list 
               (make-box false 0 0)   (make-box "o" 1 0) (make-box "o" 2 0)
               (make-box false 0 1) (make-box "x" 1 1) (make-box "x" 2 1) 
               (make-box false 0 2) (make-box false 1 2) (make-box false 2 2))
                 false false 2))

(define example-board-o-block
  (make-board (list 
               (make-box false 0 0)   (make-box false 1 0) (make-box "o" 2 0)
               (make-box false 0 1) (make-box "x" 1 1) (make-box "x" 2 1) 
               (make-box false 0 2) (make-box false 1 2) (make-box "o" 2 2))
                 false false 2))

(define example-board-draw 
  (make-board (list 
               (make-box "o" 0 0) (make-box "x" 1 0) (make-box "x" 2 0)
               (make-box "x" 0 1) (make-box "o" 1 1) (make-box "o" 2 1) 
               (make-box "o" 0 2) (make-box "x" 1 2) (make-box "x" 2 2))
                 true "It's a draw" 0))

(define example-board-one-val 
  (make-board (list 
               (make-box "x" 0 0) (make-box false 1 0) (make-box false 2 0)
               (make-box false 0 1) (make-box false 1 1) (make-box false 2 1) 
               (make-box false 0 2) (make-box false 1 2) (make-box false 2 2))
                 true false 0))

(define example-board-one-val-inside 
  (make-board (list 
               (make-box false 0 0) (make-box "x" 1 0) (make-box false 2 0)
               (make-box false 0 1) (make-box false 1 1) (make-box false 2 1) 
               (make-box false 0 2) (make-box false 1 2) (make-box false 2 2))
                 true false 0))

(define example-board-ofinalmove
  (make-board (list 
               (make-box false 0 0) (make-box "x" 1 0)   (make-box false 2 0)
               (make-box false 0 1)   (make-box "o" 1 1)   (make-box false 2 1) 
               (make-box "x" 0 2)   (make-box "x" 1 2) (make-box "o" 2 2))
              false false 1))





(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))



(define RECT-HORIZONTAL (rectangle (- SIZE (/ SIZE 10))
                                   (/ SIZE 40) "solid" "black"))
(define RECT-VERTICAL   (rectangle (/ SIZE 40)
                                   (- SIZE (/ SIZE 10)) "solid" "black"))


(define RECT-HORIZ-UPPER-POS (make-posn (/ SIZE 2) (/ SIZE 3)))
(define RECT-HORIZ-LOWER-POS (make-posn (/ SIZE 2) (* 2 (/ SIZE 3))))
(define RECT-VERTICAL-LEFT-POS (make-posn (/ SIZE 3) (/ SIZE 2)))
(define RECT-VERTICAL-RIGHT-POS (make-posn (* 2 (/ SIZE 3)) (/ SIZE 2)))


(define LIST-OF-RECT-IMG (list RECT-HORIZONTAL
                               RECT-HORIZONTAL
                               RECT-VERTICAL
                               RECT-VERTICAL))


(define LIST-OF-RECT-POS (list RECT-HORIZ-UPPER-POS
                               RECT-HORIZ-LOWER-POS
                               RECT-VERTICAL-LEFT-POS
                               RECT-VERTICAL-RIGHT-POS))


(define TEXT-SIZE (round (/ SIZE 4)))

(define X (text "X" TEXT-SIZE "firebrick"))
(define O (text "O" TEXT-SIZE "steel blue"))


(define RESULT-TEXT-SIZE (round (/ SIZE 6)))

(define RESULT-TEXT-COLOR "blue violet")

(define RESULT-RECT (rectangle SIZE (* 2 (/ SIZE 5)) 220 "gainsboro"))



(define board-image
  (place-images LIST-OF-RECT-IMG LIST-OF-RECT-POS 
                (square SIZE "solid" "white")))



                                                                            





(define CENTER-POSITIONS (list (/ SIZE 6) (/ SIZE 2) (* 5 (/ SIZE 6)))) 












(check-expect(update-board empty-board (make-box "x" 0 0))
             (make-board (list 
                          (make-box "x" 0 0)
                          (make-box false 1 0)
                          (make-box false 2 0)
                          (make-box false 0 1) 
                          (make-box false 1 1) 
                          (make-box false 2 1) 
                          (make-box false 0 2) 
                          (make-box false 1 2) 
                          (make-box false 2 2))
                         false false 0)) 


(check-expect(update-board empty-board-computer (make-box "o" 2 1))
             (make-board (list 
                          (make-box false 0 0)
                          (make-box false 1 0)
                          (make-box false 2 0)
                          (make-box false 0 1) 
                          (make-box false 1 1) 
                          (make-box "o" 2 1)
                          (make-box false 0 2) 
                          (make-box false 1 2) 
                          (make-box false 2 2))
                         true false 0)) 

(check-expect(update-board empty-board (make-box "x" 1 0))
             (make-board (list 
                          (make-box false 0 0)
                          (make-box "x" 1 0)
                          (make-box false 2 0)
                          (make-box false 0 1) 
                          (make-box false 1 1) 
                          (make-box false 2 1) 
                          (make-box false 0 2) 
                          (make-box false 1 2) 
                          (make-box false 2 2))
                         false false 0))

(check-expect(update-board empty-board (make-box "x" 0 2))
             (make-board (list 
                          (make-box false 0 0)
                          (make-box false 1 0)
                          (make-box false 2 0)
                          (make-box false 0 1) 
                          (make-box false 1 1) 
                          (make-box false 2 1) 
                          (make-box "x" 0 2)
                          (make-box false 1 2) 
                          (make-box false 2 2))
                         false false 0))

(define (update-board board box)
  
  (local [(define (create-lob box lob) 
            
            (cond [(empty? lob) empty]
                  [(is-same? lob box)
                   (cons box (create-lob box (rest lob)))]
                  [else 
                   (cons (first lob) (create-lob box (rest lob)))]))] 
    (make-board (create-lob box (board-boxes board))
                (not (board-turn board))
                
                
                (check-game-finished 
                 (make-board (create-lob box (board-boxes board)) 
                             (board-turn board)
                             (board-winner board) (board-diff board)))
                (board-diff board))))






(check-expect (is-same? (board-boxes empty-board) (make-box "x" 0 0)) true) 

(check-expect (is-same? (board-boxes empty-board) (make-box "y" 0 1)) false) 

(check-expect (is-same? (board-boxes empty-board) (make-box false 1 1)) false) 


(define (is-same? lob box)
  (if (and (= (box-x (first lob)) (box-x box)) 
           (= (box-y (first lob)) (box-y box)))
      true
      false))





(define (main board)
  (big-bang board
    (on-tick computer-turn) 
    (on-mouse player-turn)  
    (on-key set-difficulty)
    (to-draw render)))







(check-expect(player-turn empty-board 101 101 "button-up") empty-board)

(check-expect(player-turn empty-board-computer 101 101 "button-down") empty-board-computer)

(check-expect(player-turn example-board-xwin 101 101 "button-down")  example-board-xwin) 



(check-expect(player-turn example-board-neutral 100 101 "button-down")
             example-board-neutral)
(check-expect(player-turn example-board-neutral 203 101 "button-down")
             (update-board example-board-neutral (make-box "x" 2 1)))


(check-expect(player-turn empty-board 101 101 "button-down")
             (update-board empty-board (make-box "x" 1 1)))


(check-expect(player-turn empty-board 201 11 "button-down")
             (update-board empty-board (make-box "x" 2 0)))




(define (player-turn board x y m) 
  
  (local [(define (find-coor coor)
            (cond [(< coor (/ SIZE 3)) 0]
                  [(> coor (* 2 (/ SIZE 3))) 2]
                  [else 1]))]
    
    (local [(define (place-move box lob)
              (cond [(empty? lob) board]
                    [else (if (and (is-same? lob box) 
                                   (not (box-filled? (first lob))))
                              (update-board board (make-box "x" (box-x box) 
                                                            (box-y box)))
                              (place-move box (rest lob)))]))]
      
      
      (if (and (board-turn board) (false? (board-winner board)) 
               (string=? m "button-down"))
          
          (place-move (make-box "x" (find-coor x) (find-coor y)) 
                      (board-boxes board))
          
          board))))









(check-expect (box-filled? (make-box "x" 1 0)) true) 

(check-expect (box-filled? (make-box "o" 2 2)) true) 

(check-expect (box-filled? (make-box false 0 2)) false) 


(define (box-filled? box)
  (if (false? (box-value box))
      false
      true))








(define (find-open-box board)
  (local [(define r (random 9))] 
    (cond 
      
      [(empty? (filter false? (map (λ (b) (box-value b)) (board-boxes board)))) 
       false]
      
      [(box-filled? (list-ref (board-boxes board) r)) 
       (find-open-box board)]
      
      [else (local [(define found-open-box (list-ref (board-boxes board) r))]
              (make-box "o" (box-x found-open-box) (box-y found-open-box)))])))








(check-expect (computer-turn example-board-ofinalmove)
              (update-board (make-board (board-boxes example-board-ofinalmove)
                                        (board-turn example-board-ofinalmove)
                                        (board-winner example-board-ofinalmove)
                                        (board-diff example-board-ofinalmove))
                            (make-box "o" 0 0)))


(check-expect (computer-turn example-board-owin-diff-1) (update-board example-board-owin-diff-1 (make-box "o" 0 0)))

(check-expect (computer-turn example-board-owin-diff-2) (update-board example-board-owin-diff-2 (make-box "o" 0 0)))

(check-expect (computer-turn example-board-o-block) (update-board example-board-o-block (make-box "o" 0 1)))	





(define (computer-turn board)
  (local [(define (find-winning-move board) 
            (local [(define (box-at box-index) 
                      (list-ref (board-boxes board) box-index))
                    
                    (define (check-winning-moves board spots)
                      
                      (cond [(empty? spots) false]
                            
                            [(and (not (box-filled? (box-at (first spots))))
                                  (equal? (check-game-finished
                                           (make-board
                                             (list-set (board-boxes board) 
                                                       (first spots)
                                                       (make-box (if (board-turn board) "x" "o")
                                                                 (box-x (box-at (first spots)))
                                                                 (box-y (box-at (first spots)))))
                                              (board-turn board)
                                              (board-winner board)
                                              (board-diff board)))
                                          (if (board-turn board)
                                              "X Wins!"
                                              "O Wins!")))
                             (make-box
                              (if (board-turn board)
                                  "x"
                                  "o")
                              (box-x (box-at (first spots)))
                              (box-y (box-at (first spots))))]
                            
                            [else (check-winning-moves board (rest spots))]))]
              
              (check-winning-moves board (build-list 9 (λ (x) x)))))

          
          (define opponent-winning-move (find-winning-move
                                         (make-board (board-boxes board)
                                                     (not (board-turn board))
                                                     false
                                                     (board-diff board))))

          
          (define computer-winning-move (find-winning-move board))
          
          (define open-box (find-open-box board))]
    
    
    (if (and (false? (board-winner board)) (false? (board-turn board)))
        
        
        
        
        (cond [(and (equal? 1 (board-diff board))
                    (not (false? computer-winning-move)))
               (update-board board (find-winning-move board))]
              
              [(equal? 2 (board-diff board))
               (if (not (false? computer-winning-move))
                   (update-board board (find-winning-move board))
                   (if (not (false? opponent-winning-move))
                       (update-board board (make-box "o"
                                                     (box-x opponent-winning-move)
                                                     (box-y opponent-winning-move)))
                       (update-board board open-box)))]
              
              [else (update-board board open-box)])
        
        
        board)))







(check-expect (check-game-finished example-board-neutral) false)  
(check-expect (check-game-finished example-board-xwin) "X Wins!") 
(check-expect (check-game-finished example-board-owin) "O Wins!") 
(check-expect (check-game-finished example-board-draw) "Draw...") 

(define (check-game-finished board) 
  
  (local [(define (filter-board b)
            (indexes-of 
             (map (λ (box) (if (board-turn b)
                               (equal? (box-value box) "x")
                               (equal? (box-value box) "o"))) 
                  (board-boxes b)) true))
          
          (define filtered-board (filter-board board))
          
          (define win-states 
            (list (list 0 1 2) (list 3 4 5) (list 6 7 8) 
                  (list 0 3 6) (list 1 4 7) (list 2 5 8) 
                  (list 0 4 8) (list 2 4 6))) 
          
          
          (define (compare-win-states fb ws)
            (cond [(empty? ws) false]
                  [(contains-win-state? fb (first ws)) true]
                  [else (compare-win-states fb (rest ws))]))
          (define (contains-win-state? fb f)
            (if (empty? f) 
                true
                (and (number? (index-of fb (first f))) 
                     (contains-win-state? fb (rest f)))))]
    
    
    (cond [(compare-win-states filtered-board win-states) 
           (if (board-turn board) 
               "X Wins!" 
               "O Wins!")] 
          [(false? (find-open-box board)) "Draw..."] 
          [else false]))) 








(check-expect (set-difficulty START "0") START) 

(check-expect (set-difficulty START "1")
              (make-board 
               (list 
                (make-box false 0 0) (make-box false 1 0) (make-box false 2 0)
                (make-box false 0 1) (make-box false 1 1) (make-box false 2 1) 
                (make-box false 0 2) (make-box false 1 2) (make-box false 2 2))
               true false 1))

(check-expect (set-difficulty START "2")
              (make-board 
               (list 
                (make-box false 0 0) (make-box false 1 0) (make-box false 2 0)
                (make-box false 0 1) (make-box false 1 1) (make-box false 2 1) 
                (make-box false 0 2) (make-box false 1 2) (make-box false 2 2))
               true false 2))

(check-expect (set-difficulty START "a") START)

(check-expect (set-difficulty START "9") START)


(define (set-difficulty board ke)
  (make-board (board-boxes board) (board-turn board) (board-winner board) (
              (λ (d) (if (or (false? d) (> d 2))
                         0
                         d)) (string->number ke))))












(check-expect (render START) (place-image board-image (/ SIZE 2) (/ SIZE 2) MTS)) 
(check-expect (render example-board-one-val)
              (place-image X (list-ref CENTER-POSITIONS (box-x (list-ref (board-boxes example-board-neutral) 0)))
                           (list-ref CENTER-POSITIONS (box-y (list-ref (board-boxes example-board-neutral) 0)))
                           (place-image board-image (/ SIZE 2) (/ SIZE 2) MTS)))
(check-expect (render example-board-one-val-inside) 
              (place-image X (list-ref CENTER-POSITIONS (box-x (list-ref (board-boxes example-board-neutral) 1)))
                           (list-ref CENTER-POSITIONS (box-y (list-ref (board-boxes example-board-neutral) 0)))
                           (place-image board-image (/ SIZE 2) (/ SIZE 2) MTS)))





(define (render board)
  (local [(define (render-ins lob) 
            
            (cond [(empty? lob) (place-image board-image 
                                             (/ SIZE 2) (/ SIZE 2) MTS)]
                  
                  [else
                   (place-image
                    (get-image (box-value (first lob)))
                    (list-ref CENTER-POSITIONS (box-x (first lob))) 
                    
                    (list-ref CENTER-POSITIONS (box-y (first lob))) 
                    
                    (render-ins (rest lob)))])) 
          
          
          
          (define (get-image str)
            (cond [(string=? str "x") X]
                  [(string=? str "o") O]
                  [else (error "impossible argument: not x or o")]))]

    
    (if (string? (board-winner board))
        
        (place-image (overlay
                      (text (board-winner board) 
                            RESULT-TEXT-SIZE 
                            RESULT-TEXT-COLOR)
                      RESULT-RECT)
                     (/ SIZE 2)
                     (/ SIZE 2)
                     (render-ins (filter box-filled?(board-boxes board))))
        
        (render-ins (filter box-filled? (board-boxes board))))))