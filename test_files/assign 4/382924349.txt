

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TicTacToe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 500) 
(define mid (/ SIZE 2))
(define MTS (empty-scene SIZE SIZE))







(define oPlayer (text "O" (round (/ SIZE 9)) "blue"))
(define xPlayer (text "X" (round (/ SIZE 9)) "red"))
(define emptySquare empty-image)

(define grid
  (local [(define gridSquare (square (/ SIZE 3) "outline" "orange"))]
    (overlay
     (square SIZE "outline" "white")
     (above
      (beside
       gridSquare gridSquare gridSquare)
      (beside
       gridSquare gridSquare gridSquare)
      (beside
       gridSquare gridSquare gridSquare)))))

(define x-win-message (text "X Wins!" (round (/ SIZE 7)) "green"))
(define o-win-message (text "O Wins!" (round (/ SIZE 7)) "green")) 
(define draw-message (text "Draw!" (round (/ SIZE 7)) "green"))
                         





(define-struct board (move-list current-player win-state difficulty))








(define START (make-board (list emptySquare emptySquare emptySquare
                                emptySquare emptySquare emptySquare 
                                emptySquare emptySquare emptySquare) true 0 0)) 

(define B1 (make-board (list xPlayer oPlayer emptySquare
                             xPlayer emptySquare emptySquare
                             emptySquare emptySquare emptySquare) false 0 0)) 

(define B2 (make-board (list xPlayer oPlayer emptySquare
                             xPlayer emptySquare oPlayer 
                             xPlayer emptySquare emptySquare) false 1 0)) 
 
(define B3 (make-board (list xPlayer emptySquare oPlayer
                             emptySquare oPlayer xPlayer
                             oPlayer emptySquare xPlayer) true 1 0)) 

(define B4 (make-board (list xPlayer oPlayer xPlayer
                             oPlayer oPlayer xPlayer
                             xPlayer xPlayer oPlayer) true 2 0)) 

(define B5 (make-board (list xPlayer     emptySquare  oPlayer
                             emptySquare xPlayer      oPlayer
                             xPlayer     emptySquare  emptySquare) false 0 1)) 

(define B6 (make-board (list xPlayer     emptySquare  oPlayer
                             emptySquare xPlayer      emptySquare
                             oPlayer     emptySquare  emptySquare) false 0 2)) 

(define B7 (make-board (list xPlayer     emptySquare  oPlayer
                             xPlayer     xPlayer      oPlayer
                             emptySquare emptySquare  emptySquare) false 0 2)) 






(define (main board)
  (big-bang board
    (on-mouse  player-click)      
    (to-draw   draw-board)        
    (on-tick   board-change)      
    (on-key    change-difficulty) 
    ))                              






(check-expect (board-change B3) 
              (make-board (list xPlayer emptySquare oPlayer
                                emptySquare oPlayer xPlayer
                                oPlayer emptySquare xPlayer) true 1 0))

(check-expect (board-change B4) 
              (make-board (list xPlayer oPlayer xPlayer
                                oPlayer oPlayer xPlayer
                                xPlayer xPlayer oPlayer) true 2 0)) 

(check-expect (board-change B5) 
              (make-board (list xPlayer     emptySquare  oPlayer
                                emptySquare xPlayer      oPlayer
                                xPlayer     emptySquare  oPlayer) true 0 1))

(check-expect (board-change B6) 
              (make-board (list xPlayer     emptySquare  oPlayer
                                emptySquare xPlayer      emptySquare
                                oPlayer     emptySquare  oPlayer) true 0 2))
              
(check-expect (board-change B7) 
              (make-board (list xPlayer     emptySquare  oPlayer
                                xPlayer     xPlayer      oPlayer
                                emptySquare emptySquare  oPlayer) true 0 2))

(define (board-change aBoard)
  (local [(define (change-inner aBoard)
            (cond [(or (= 1 (board-win aBoard)) (= 2 (board-win aBoard)))
                   (make-board (board-move-list aBoard)
                               (board-current-player aBoard)
                               (board-win aBoard) (board-difficulty aBoard))]   
                  [(not (board-current-player aBoard)) 
                   (computer-move aBoard)]
                  [else aBoard]))


          
          
          
          
          

          (define (computer-move aBoard)
            
            (cond [(= (board-difficulty aBoard) 0) (random-computer-move aBoard empty)] 
                  [(= (board-difficulty aBoard) 1) (level-one-difficulty aBoard random-computer-move empty)] 
                  [else (level-two-difficulty aBoard)])) 

          
          
          (define (level-one-difficulty aBoard fn e-list)
            (local [(define l-lom (find-all-empty-squares(board-move-list aBoard)))]
              (local [(define (dif1-move aBoard l-lom fn)
                        (cond [(empty? l-lom) (fn aBoard e-list)]
                              [( = 1 (board-win (new-board aBoard (first l-lom))))
                               (new-board aBoard (first l-lom))]
                              [else
                               (dif1-move aBoard (rest l-lom) fn)]))]
                (dif1-move aBoard l-lom fn))))


          
          
          
          (define (level-two-difficulty aBoard)
            (local [(define l-lox (find-all-empty-squares(board-move-list aBoard)))]
              (local [(define (dif2-move aBoard l-lox)
                        (cond [(empty? l-lox) (random-computer-move aBoard empty)]
                              [( = 1 (board-win (new-board (make-board (board-move-list aBoard)
                                                                       (not(board-current-player aBoard))
                                                                       (board-win-state aBoard) (board-difficulty aBoard))
                                                           (first l-lox))))
                               (new-board aBoard (first l-lox))]
                              [else
                               (dif2-move aBoard (rest l-lox))]))]
                (level-one-difficulty aBoard dif2-move l-lox))))


          
          
          (define (random-computer-move aBoard e-list)
            (new-board aBoard (random-element (find-all-empty-squares (board-move-list aBoard)))) 
            )
    

          
          
          (define (find-all-empty-squares Loi)
            (local [(define (find-empty Loi position-acc position-list)
                      (cond [(empty? Loi) position-list]
                            [(equal? (first Loi) empty-image)
                             (find-empty (rest Loi) (add1 position-acc) (cons position-acc position-list))]
                            [else
                             (find-empty (rest Loi) (add1 position-acc) position-list)]))]
              (find-empty Loi 0 empty)))
 

          
          
          (define (random-element loe)
            (list-ref loe (random (length loe))))]
    (change-inner aBoard)))
 

 




(define add-coord (+ mid (round(/ SIZE 3)))) 
(define sub-coord (- mid (round(/ SIZE 3))))


(check-expect (draw-board B1) (place-images (cons emptySquare
                                                  (append (list grid)
                                                          (board-move-list B1)))
                                            (list (make-posn mid mid)
                                                  (make-posn mid mid)
                                                  (make-posn sub-coord sub-coord) (make-posn  mid sub-coord) (make-posn add-coord sub-coord)
                                                  (make-posn sub-coord mid) (make-posn mid mid) (make-posn add-coord mid)
                                                  (make-posn sub-coord add-coord) (make-posn  mid add-coord) (make-posn add-coord add-coord)) MTS))

(check-expect (draw-board B3) (place-images (cons o-win-message
                                                  (append (list grid)
                                                          (board-move-list B3)))
                                            (list (make-posn mid mid)
                                                  (make-posn mid mid)
                                                  (make-posn sub-coord sub-coord) (make-posn  mid sub-coord) (make-posn add-coord sub-coord)
                                                  (make-posn sub-coord mid) (make-posn mid mid) (make-posn add-coord mid)
                                                  (make-posn sub-coord add-coord) (make-posn  mid add-coord) (make-posn add-coord add-coord)) MTS))

(check-expect (draw-board B5) (place-images (cons emptySquare
                                                  (append (list grid)
                                                          (board-move-list B5)))
                                            (list (make-posn mid mid)
                                                  (make-posn mid mid)
                                                  (make-posn sub-coord sub-coord) (make-posn  mid sub-coord) (make-posn add-coord sub-coord)
                                                  (make-posn sub-coord mid) (make-posn mid mid) (make-posn add-coord mid)
                                                  (make-posn sub-coord add-coord) (make-posn  mid add-coord) (make-posn add-coord add-coord)) MTS))

(define (draw-board aBoard)
  (local [(define (draw-board-inner aBoard)
            (place-images (cons (choose-end-state-image aBoard)
                                (append (list grid)
                                        (board-move-list aBoard)))
                          (list (make-posn mid mid)
                                (make-posn mid mid)
                                (make-posn sub-coord sub-coord) (make-posn  mid sub-coord) (make-posn add-coord sub-coord)
                                (make-posn sub-coord mid) (make-posn mid mid) (make-posn add-coord mid)
                                (make-posn sub-coord add-coord) (make-posn  mid add-coord) (make-posn add-coord add-coord)) MTS))
                      
                      
          
          
          
          (define (choose-end-state-image aBoard)
            (cond 
              [(and ( = (board-win-state aBoard) 1) (board-current-player aBoard))
               o-win-message]
              [(and (= (board-win-state aBoard) 1) (not (board-current-player aBoard)))
               x-win-message]
              [(and ( = (board-win-state aBoard) 2) (board-full? aBoard))
               draw-message]
              [else
               empty-image]))]
    (draw-board-inner aBoard)))
     





(check-expect (player-click START ( - (/ SIZE 3) (/ (/ SIZE 3) 2)) ( + (/ SIZE 3) (/ (/ SIZE 3) 2)) "button-down")
              (make-board (list emptySquare emptySquare emptySquare
                                xPlayer emptySquare emptySquare 
                                emptySquare emptySquare emptySquare) false 0 0))

(check-expect (player-click START (+ (* 2 (/ SIZE 3)) 20) (- (/ SIZE 3) 20) "button-down") (make-board (list emptySquare emptySquare xPlayer
                                                                                                             emptySquare emptySquare emptySquare 
                                                                                                             emptySquare emptySquare emptySquare) false 0 0))
                                                                            
(check-expect (player-click START (- (* 2 (/ SIZE 3)) 20) (+ (/ SIZE 3) 20) "button-down") (make-board (list emptySquare emptySquare emptySquare
                                                                                                             emptySquare xPlayer     emptySquare 
                                                                                                             emptySquare emptySquare emptySquare) false 0 0))                                                                                                                                                   

(define (player-click aBoard x y me)
  (local [(define sz (/ SIZE 3))]
    (cond [(mouse=? me "button-down")
           (cond
             [(or (= (board-win-state aBoard) 1) (=  (board-win-state aBoard) 2)) aBoard]
             [(<= x sz) (cond [(<= y sz) (new-board aBoard 0)]
                              [(>= y (* 2 sz)) (new-board aBoard 6)]
                              [else (new-board aBoard 3)])]
             [(>= x (* 2 sz)) (cond [(<= y sz) (new-board aBoard 2)]
                                    [(>= y (* 2 sz)) (new-board aBoard 8)]
                                    [else (new-board aBoard 5)])]
             [else (cond [(<= y sz) (new-board aBoard 1)]
                         [(>= y (* 2 sz)) (new-board aBoard 7)]
                         [else (new-board aBoard 4)])])]      
               
          [else aBoard])))





(check-expect (new-board START 3) (make-board (list emptySquare emptySquare emptySquare
                                                    xPlayer emptySquare emptySquare 
                                                    emptySquare emptySquare emptySquare) false 0 0))

(check-expect (new-board START 2) (make-board (list emptySquare emptySquare xPlayer
                                                    emptySquare emptySquare emptySquare 
                                                    emptySquare emptySquare emptySquare) false 0 0))


(define (new-board aBoard position)
  (local [(define (new-board-l aBoard position)
            (cond [(equal? emptySquare (list-ref (board-move-list aBoard) position))
                   (make-board (update-board aBoard position) (not(board-current-player aBoard)) (board-win-state aBoard) (board-difficulty aBoard))]
                  [else aBoard]))
          
          
          
          (define (update-board bBoard pos)
            (append (take (board-move-list bBoard) pos)
                    (list (which-player bBoard))
                    (drop (board-move-list bBoard) (add1 pos))))

          
          
          (define (which-player bBoard)
            (if (board-current-player bBoard)
                xPlayer
                oPlayer))]

    (new-board-l aBoard position)))






(check-expect (board-win B4) 2)

(define (board-win bBoard)
  (local [
          
          (define (won? aBoard)
            (cond[(check-row? 0 aBoard) true]
                 [(check-row? 3 aBoard) true]
                 [(check-row? 6 aBoard) true]
                 [(check-column? 0 aBoard) true] 
                 [(check-column? 1 aBoard) true]
                 [(check-column? 2 aBoard) true]
                 [(check-diagonal-right? 0 aBoard) true]
                 [(check-diagonal-left? 2 aBoard) true]
                 [else false]))

          
          
          
          (define (check-row? num bBoard)
            (triple-equal-image? (list-ref (board-move-list bBoard) num)
                                 (list-ref (board-move-list bBoard) (+ 1 num))
                                 (list-ref (board-move-list bBoard) (+ 2 num))))
                       
          
          
          
          (define (check-column? num bBoard)
            (triple-equal-image? (list-ref (board-move-list bBoard) num)
                                 (list-ref (board-move-list bBoard) (+ 3 num))
                                 (list-ref (board-move-list bBoard) (+ 6 num))))
                       
          
          
          
          (define (check-diagonal-right? num bBoard)
            (triple-equal-image? (list-ref (board-move-list bBoard) num)
                                 (list-ref (board-move-list bBoard) (+ 4 num))
                                 (list-ref (board-move-list bBoard) (+ 8 num))))
                       
          
          
          
          (define (check-diagonal-left? num bBoard)
            (triple-equal-image? (list-ref (board-move-list bBoard) num)
                                 (list-ref (board-move-list bBoard) (+ 2 num))
                                 (list-ref (board-move-list bBoard) (+ 4 num))))

          
          
          (define (triple-equal-image? img1 img2 img3)
            (cond [(not (emptySquare? img1))  
                   (and (equal? img1 img2)
                        (equal? img1 img3)
                        (equal? img2 img3))]
                  [else false]))
       

          (define (board-win-l aBoard)

            (cond [(won? aBoard) 1] 
                  [(board-full? aBoard) 2] 
                  [else 0])) ]

    (board-win-l bBoard)))





(define (board-full? aBoard)
  (not (ormap emptySquare? (board-move-list aBoard))))




(define (emptySquare? img)
  (equal? empty-image img)
  ) 
 




(check-expect (change-difficulty START "2") (make-board (list emptySquare emptySquare emptySquare
                                                              emptySquare emptySquare emptySquare 
                                                              emptySquare emptySquare emptySquare) true 0 2))
(check-expect (change-difficulty B5 "0") (make-board (list xPlayer     emptySquare  oPlayer
                                                           emptySquare xPlayer      oPlayer
                                                           xPlayer     emptySquare  emptySquare) false 0 0))
                                                   
(define (change-difficulty aBoard ke)
  (cond [(integer? (string->number ke))
         (make-board (board-move-list aBoard) (board-current-player aBoard) (board-win-state aBoard) (string->number ke))]
        [else 
         aBoard])) 
         
