

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tictactoe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 







(define SIZE 900) 
(define MTS (empty-scene SIZE SIZE))






(define line_color "Black")
(define XPOS (/ SIZE 2))
(define YPOS (/ SIZE 2))


(define player_mark (text "X" (/ SIZE 6) "red"))

(define computer_mark (text "O" (/ SIZE 6) "blue"))


(define winning-boards (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 1 4 7) (list 2 5 8) (list 3 6 9)  (list 1 5 9) (list 3 5 7)))

(define OFFSET (/ (/ SIZE 3) 2))


(define board_square (square (/ SIZE 3) "outline" line_color))

(define pos1 (make-posn 0 0))
(define pos2 (make-posn (/ SIZE 3) 0))
(define pos3 (make-posn (* (/ SIZE 3) 2) 0))
(define pos4 (make-posn 0 (/ SIZE 3)))
(define pos5 (make-posn (/ SIZE 3) (/ SIZE 3)))
(define pos6 (make-posn (* (/ SIZE 3) 2) (/ SIZE 3)))
(define pos7 (make-posn 0 (* (/ SIZE 3) 2)))
(define pos8 (make-posn (/ SIZE 3) (* (/ SIZE 3) 2)))
(define pos9 (make-posn (* (/ SIZE 3) 2) (* (/ SIZE 3) 2)))






(check-expect (add-offset (make-posn 200 350)) (make-posn (+ OFFSET 200) (+ OFFSET 350))) 
(check-expect (add-offset (make-posn 100 120)) (make-posn (+ OFFSET 100) (+ OFFSET 120))) 
(check-expect (add-offset (make-posn 0 0)) (make-posn OFFSET OFFSET)) 

(define (add-offset pos)
  (make-posn (+ OFFSET (posn-x pos))
             (+ OFFSET (posn-y pos)))
  )

(define board_grid (place-images (list board_square board_square board_square
                                       board_square board_square board_square
                                       board_square board_square board_square)
                                 (map add-offset (list pos1 pos2 pos3
                                                       pos4 pos5 pos6
                                                       pos7 pos8 pos9))
                                 MTS))


(define B  (list 1 2 3
                 4 5 6
                 7 8 9))


(define-struct ws (board-image x-spots o-spots diff))






(define START (make-ws board_grid empty empty 0))

(define WS1 (make-ws
             board_grid
             (list 1 2 3 4)
             (list 5 6 7 8 9)
             0))

(define WS2 (make-ws
             board_grid
             (list 1)
             (list 2 4)
             1))

(define WS3 (make-ws board_grid empty empty 2))


           





(check-expect (coor->pos 0 0) 1) 
(check-expect (coor->pos (+ (/ SIZE 3) 1) 0) 2)
(check-expect (coor->pos (/ SIZE 3) 0) 2)
(check-expect (coor->pos (* (/ SIZE 3) 2) 0) 3)

(check-expect (coor->pos 0  (/ SIZE 3)) 4)
(check-expect (coor->pos (+ (/ SIZE 3) 1) (/ SIZE 3)) 5)
(check-expect (coor->pos (* (/ SIZE 3) 2) (/ SIZE 3)) 6)

(check-expect (coor->pos 0 (* (/ SIZE 3) 2)) 7)
(check-expect (coor->pos (/ SIZE 3) (* (/ SIZE 3) 2)) 8)
(check-expect (coor->pos (* (/ SIZE 3) 2) (* (/ SIZE 3) 2)) 9)

(define (coor->pos x y)
  (cond
    [(and (< x (/ SIZE 3)) (< y (/ SIZE 3))) 1]
    [(and (< x (* (/ SIZE 3) 2)) (< y (/ SIZE 3))) 2]
    [(and (< x SIZE) (< y (/ SIZE 3))) 3]
     
    [(and (< x (/ SIZE 3)) (< y (* (/ SIZE 3) 2))) 4]
    [(and (< x (* (/ SIZE 3) 2)) (< y (* (/ SIZE 3) 2))) 5]
    [(and (< x SIZE) (< y (* (/ SIZE 3) 2))) 6]
     
    [(and (< x (/ SIZE 3)) (< y SIZE)) 7]
    [(and (< x (* (/ SIZE 3) 2)) (< y SIZE)) 8]
    [(and (< x SIZE) (< y SIZE)) 9]))








  

(define board1 (make-ws board_grid (list 1 2 3 4 5) (list 6 7 8 9) 0))

(check-expect (place-move 3 board1)
              (make-ws (ws-board-image board1)
                       (ws-x-spots board1)
                       (ws-o-spots board1)
                       0)) 

(define board2 (make-ws board_grid (list 2 3 4 5) (list 1 6 8 9) 0))
  
(check-expect (place-move 7 board2)
              (make-ws (ws-board-image board2)
                       (cons 7 (ws-x-spots board2))
                       (ws-o-spots board2)
                       0)) 
                       

(define (place-move pos ws)
  (local
    
    
    [(define occupied?
       (not (or (member? pos (ws-x-spots ws)) (member? pos (ws-o-spots ws))))
       )
     (define (game-over? x-spots o-spots)
       (or (check-winning? x-spots winning-boards)
           (check-winning? o-spots winning-boards)
           (draw? ws)))]
    
    (cond
      
      [(and occupied?
            (= (+ (length (ws-x-spots ws))
                  (length (ws-o-spots ws))) 8)) 
       (make-ws (ws-board-image ws)
                (cons pos (ws-x-spots ws))
                (ws-o-spots ws)
                (ws-diff ws)
                )]
      
      [occupied?
       (make-ws (ws-board-image ws)
                (cons pos (ws-x-spots ws))
                (if
                 (not (game-over?
                       (cons pos (ws-x-spots ws))
                       (ws-o-spots ws)))
                 (make-possible-boards 
                  (make-ws (ws-board-image ws)
                           (cons pos (ws-x-spots ws))
                           (ws-o-spots ws)
                           (ws-diff ws)))
                 (ws-o-spots ws))
                (ws-diff ws))]
      
      [else ws])
    ))





(check-expect (draw-move START 100 90 "enter") START) 

(check-expect (draw-move (make-ws board_grid (list 1) (list 3) 0) 0 0 "button-down") (make-ws board_grid (list 1) (list 3) 0)) 
(check-expect (draw-move (make-ws board_grid (list 2 6 7 9) (list 3 4 5 8) 0) 0 0 "button-down") (make-ws board_grid (list 1 2 6 7 9) (list 3 4 5 8) 0)) 

(define (draw-move ws x y event)
  (if (or (check-winning? (ws-x-spots ws) winning-boards)
          (check-winning? (ws-o-spots ws) winning-boards)
          (draw? ws))
      ws
      (if (string=? event "button-down")
          (place-move (coor->pos x y) ws)
          ws)))
                  






(check-expect (make-image-list empty computer_mark) empty) 
(check-expect (make-image-list (list 1 2 3) computer_mark) (list computer_mark computer_mark computer_mark)) 
(check-expect (make-image-list (list 4) player_mark) (list player_mark)) 

(define (make-image-list lon img)
  (local
    [(define (make-image-list len img img-list)
       (cond
         [(<= len 0) img-list]
         [else (make-image-list (sub1 len) img (cons img img-list))]
         ))]
    (make-image-list (length lon) img empty)))





(check-expect (bd-pos->screen-pos 3) (make-posn (- (* (/ SIZE 3) 3) (/ SIZE 6))
                                                (- (* (/ SIZE 3) 1) (/ SIZE 6)))) 

(check-expect (bd-pos->screen-pos 9) (make-posn (- (* (/ SIZE 3) 3) (/ SIZE 6))
                                                (- (* (/ SIZE 3) 3) (/ SIZE 6)))) 

(check-expect (bd-pos->screen-pos 6) (make-posn (- (* (/ SIZE 3) 3) (/ SIZE 6))
                                                (- (* (/ SIZE 3) 2) (/ SIZE 6)))) 

(check-expect (bd-pos->screen-pos 4) (make-posn (- (* (/ SIZE 3) 1) (/ SIZE 6))
                                                (- (* (/ SIZE 3) 2) (/ SIZE 6)))) 

(check-expect (bd-pos->screen-pos 2) (make-posn (- (* (/ SIZE 3) 2) (/ SIZE 6))
                                                (- (* (/ SIZE 3) 1) (/ SIZE 6)))) 

(define (bd-pos->screen-pos pos)
  (local [(define y-mult
            (if (<= pos 3)
                1
                (if (<= pos 6)
                    2
                    (if (<= pos 9)
                        3
                        3))))
          (define x-mult
            (if (= (modulo pos 3) 0)
                3
                (modulo pos 3)))]
    (make-posn (- (* (/ SIZE 3) x-mult) (/ SIZE 6))
               (- (* (/ SIZE 3) y-mult) (/ SIZE 6)))))




(check-expect (make-list-posn empty) empty) 
(check-expect (make-list-posn (list 1 2 3)) (list (bd-pos->screen-pos 3) (bd-pos->screen-pos 2) (bd-pos->screen-pos 1))) 
(check-expect (make-list-posn (list 4 6 3 7 8)) (list (bd-pos->screen-pos 8) (bd-pos->screen-pos 7) (bd-pos->screen-pos 3) (bd-pos->screen-pos 6) (bd-pos->screen-pos 4))) 
 
(define (make-list-posn lon)
  (local
    [(define (make-list-posn lon lo-posn)
       (cond
         [(empty? lon) lo-posn]
         [else (make-list-posn (rest lon) (cons (bd-pos->screen-pos (first lon)) lo-posn))]))]
    (make-list-posn lon empty)))





(check-expect (check-winning? (list 1 4 3) empty) false) 
(check-expect (check-winning? (list 1 2 3) winning-boards) true) 
(check-expect (check-winning? (list 1 3) winning-boards) false) 
(check-expect (check-winning? (list 1 5 9) winning-boards) true) 

(define (check-winning? player-spots winning-boards)
  (cond
    [(empty? winning-boards) false]
    [(andmap (λ (n) (member? n player-spots)) (first winning-boards)) true]
    [else (check-winning? player-spots (rest winning-boards))]))




(check-expect (draw? (make-ws board_grid empty empty 0)) false) 
(check-expect (draw? (make-ws board_grid (list 1 2 3 4 5 6) (list 7 8 9) 0)) true) 
(check-expect (draw? (make-ws board_grid (list 1 2 6 7 9) (list 3 4 5 8) 0)) true)

(define (draw? ws) (>= (+ (length (ws-o-spots ws)) (length (ws-x-spots ws))) 9))




(check-expect (get-possible-move-list
               WS1) empty) 

(check-expect (get-possible-move-list
               WS2)
              (list 3 5 6 7 8 9)) 

(check-expect (get-possible-move-list
               WS3)
              (list 1 2 3 4 5 6 7 8 9)) 
               
(define (get-possible-move-list ws)
  
  (filter positive?
          (build-list
           9
           (lambda (x)
             (if
              (not
               (member?
                (add1 x)
                (append
                 (ws-x-spots ws)
                 (ws-o-spots ws))))
              (add1 x)
              -1)))))







(check-expect (make-possible-boards (make-ws board_grid (list 1 2 6 7 9) (list 3 4 5) 2)) (list 8 3 4 5))



(check-expect (make-possible-boards (make-ws board_grid (list 2 5 7) (list 6 9) 1)) (list 3 6 9)) 

(check-expect (make-possible-boards (make-ws board_grid (list 2 4 8) (list 6 9) 2)) (list 3 6 9)) 
(check-expect (make-possible-boards (make-ws board_grid (list 2 3 7) (list 6 9) 2)) (list 1 6 9)) 

                                    
(define (make-possible-boards ws)
  (local
    [(define (make-possible-boards lop ws)
       (cond
         [(or (empty? lop)(= (ws-diff ws) 0))
          (local
            [ (define (computer-move current-spots new-spot)
                (if (not
                     (or (member? new-spot current-spots)
                         (member? new-spot(ws-x-spots ws))))
                    (cons new-spot current-spots)
                    (computer-move current-spots (+ 1 (random 8)))))]
            (computer-move (ws-o-spots ws) (+ 1 (random 8)))) 
          ]


         [(check-winning? (cons (first lop) (ws-o-spots ws))
                          winning-boards)
          (cons (first lop) (ws-o-spots ws))]
         [(= (ws-diff ws) 2) 
          (if (check-winning? (cons (first lop) (ws-x-spots ws))
                              winning-boards)
              (cons (first lop) (ws-o-spots ws))
              (make-possible-boards (rest lop) ws))]
      
         [else (make-possible-boards (rest lop) ws)]))]
    (make-possible-boards (get-possible-move-list ws) ws)
    ))
 




(check-expect (change-diff START "1") (make-ws (ws-board-image START) (ws-x-spots START) (ws-o-spots START) 1)) 
(check-expect (change-diff START "2") (make-ws (ws-board-image START) (ws-x-spots START) (ws-o-spots START) 2)) 
(check-expect (change-diff START "e") (make-ws (ws-board-image START) (ws-x-spots START) (ws-o-spots START) (ws-diff START))) 
(check-expect (change-diff WS2 "0") (make-ws (ws-board-image WS2) (ws-x-spots WS2) (ws-o-spots WS2) 0)) 

(define (change-diff ws k)
  (cond
    [(key=? k "0") (make-ws (ws-board-image ws) (ws-x-spots ws) (ws-o-spots ws) 0)]
    [(key=? k "1") (make-ws (ws-board-image ws) (ws-x-spots ws) (ws-o-spots ws) 1)]
    [(key=? k "2") (make-ws (ws-board-image ws) (ws-x-spots ws) (ws-o-spots ws) 2)]
    [else (make-ws (ws-board-image ws) (ws-x-spots ws) (ws-o-spots ws) (ws-diff ws))]))





(check-expect (get-text START) "") 
(check-expect (get-text (make-ws board_grid (list 1 2 3) empty 0)) "X WON") 
(check-expect (get-text (make-ws board_grid empty (list 1 5 9) 2)) "O WON") 
(check-expect (get-text (make-ws board_grid (list 1 2 6 7 9) (list 3 4 5 8) 1)) "DRAW") 

(define (get-text ws)
  (cond 
    [(check-winning? (ws-x-spots ws) winning-boards) "X WON"]
    [(check-winning? (ws-o-spots ws) winning-boards) "O WON"]
    [(draw? ws) "DRAW"]
    [else ""] ))





(check-expect (render-board START) (place-images
   (append
    (list (text (get-text START) (/ SIZE 6) "green"))
    (make-image-list (ws-x-spots START) player_mark)
    (make-image-list (ws-o-spots START) computer_mark))
                                 
   (append
    (list (make-posn (/ SIZE 2) (/ SIZE 2)))
    (make-list-posn (ws-x-spots START))
    (make-list-posn (ws-o-spots START)))

   board_grid)) 
   
   (check-expect (render-board WS1) (place-images
   (append
    (list (text (get-text WS1) (/ SIZE 6) "green"))
    (make-image-list (ws-x-spots WS1) player_mark)
    (make-image-list (ws-o-spots WS1) computer_mark))
                                 
   (append
    (list (make-posn (/ SIZE 2) (/ SIZE 2)))
    (make-list-posn (ws-x-spots WS1))
    (make-list-posn (ws-o-spots WS1)))

   board_grid)) 


(define (render-board ws)
  (place-images
   (append
    (list (text (get-text ws) (/ SIZE 6) "green"))
    (make-image-list (ws-x-spots ws) player_mark)
    (make-image-list (ws-o-spots ws) computer_mark))
                                 
   (append
    (list (make-posn (/ SIZE 2) (/ SIZE 2)))
    (make-list-posn (ws-x-spots ws))
    (make-list-posn (ws-o-spots ws)))

   board_grid))



(define main (big-bang START
               (to-draw render-board)
               (on-mouse draw-move)
               (on-key change-diff)
               ))



                


