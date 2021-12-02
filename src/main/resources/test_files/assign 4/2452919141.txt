

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe v2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 400) 
(define MTS (empty-scene SIZE SIZE))

(define LINE-STROKE (* SIZE 0.015))
(define LINE-LENGTH (* SIZE 0.875))
(define LINE-COLOR "black")

(define X-SIZE (* SIZE 0.28))
(define O-DIAM (* SIZE 0.3))
(define MARK-STROKE (* SIZE 0.02))
(define X-COLOR "orange")
(define O-COLOR "blue")

(define X-IMG
  (scene+line
   (scene+line
    (empty-scene X-SIZE X-SIZE)
    X-SIZE 0 0 X-SIZE
    (make-pen X-COLOR (round MARK-STROKE) "solid" "round" "round"))
   0 0 X-SIZE X-SIZE
   (make-pen X-COLOR (round MARK-STROKE) "solid" "round" "round")))

(define O-IMG
  (circle (/ O-DIAM 2) "outline" (make-pen O-COLOR (round MARK-STROKE) "solid" "round" "round")))

(define TEXT-SIZE (* SIZE 0.2))
(define TEXT-COLOR "black")



(define-struct cell (player id img xmin xmax ymin ymax))








(define-struct ws (board turn dif game-over))












(define (main ws)
  (big-bang ws
    (on-tick update)
    (to-draw render)
    (on-key key-press)
    (on-mouse mouse-click)))









(define (update ws)
  (if (false? (ws-game-over ws))
      
      (cond [(string=? (ws-turn ws) "X") 
             (make-ws
              (ws-board ws)
              "X"
              (ws-dif ws)
              (game-over? (ws-board ws)))]

            [else 
             (make-ws
              
              (cond [(= (ws-dif ws) 0) (make-computer-move-random (ws-board ws))]
                    [(= (ws-dif ws) 1) (make-computer-move-1 (ws-board ws))]
                    [(= (ws-dif ws) 2) (make-computer-move-2 (ws-board ws))]
                    )
              "X"
              (ws-dif ws)
              (game-over? (ws-board ws)))] )
      ws)) 


(check-expect (update (make-ws B4I "O" 1 false))
              (make-ws (set-move B4I 6 "O") "X" 1 false))

(check-expect (update (make-ws B5I "O" 1 false))
              (make-ws (set-move B5I 5 "O") "X" 1 false))


(check-expect (update (make-ws B6I "O" 2 false))
              (make-ws (set-move B6I 1 "O") "X" 2 false))





(define (render ws)
  (if (false? (ws-game-over ws))
      
      (local [(define line-pen (make-pen LINE-COLOR (round LINE-STROKE) "solid" "round" "round"))

              
              
              (define lines
                (scene+line
                 (scene+line
                  (scene+line
                   (scene+line
                    MTS
                    (/ (- SIZE LINE-LENGTH)2) (* SIZE 2/3)
                    (- SIZE (/ (- SIZE LINE-LENGTH)2)) (* SIZE 2/3)
                    line-pen)
                   (/ (- SIZE LINE-LENGTH)2) (* SIZE 1/3)
                   (- SIZE (/ (- SIZE LINE-LENGTH)2)) (* SIZE 1/3)
                   line-pen)
                  (* SIZE 2/3) (/ (- SIZE LINE-LENGTH)2)
                  (* SIZE 2/3) (- SIZE (/ (- SIZE LINE-LENGTH)2))
                  line-pen)
                 (* SIZE 1/3) (/ (- SIZE LINE-LENGTH)2)
                 (* SIZE 1/3) (- SIZE (/ (- SIZE LINE-LENGTH)2))
                 line-pen)
                )

              
              
              
              
              (define (center-of-pos pos)
                (local [(define x (remainder pos 3))
                        (define y (quotient  pos 3))]
                  (list
                   (* SIZE 1/6 (+ (* 2 x) 1))
                   (* SIZE 1/6 (+ (* 2 y) 1)))))

              
              
              (define (draw-moves loc)
                (cond [(empty? loc) lines]
                      [else
                       (place-image
                        (cell-img (first loc))
                        (first  (center-of-pos (cell-id (first loc))))
                        (second (center-of-pos (cell-id (first loc))))
                        (draw-moves (rest loc)))]))]

        
        (draw-moves (ws-board ws)))

      (place-image (text
                    (cond
                      
                      [(boolean? (ws-game-over ws)) "draw!"] 
                      [else (string-append
                             (ws-game-over ws)
                             " won!")])
                    TEXT-SIZE TEXT-COLOR)
                   (* SIZE 0.5) (* SIZE 0.5) MTS)))


(check-expect (render (make-ws B6I "O" 1 false))
              (place-image
               X-IMG
               (* SIZE 1/6)
               (* SIZE 1/6)
               (place-image
                X-IMG
                (* SIZE 5/6)
                (* SIZE 1/6)
                (place-image
                 O-IMG
                 (* SIZE 1/2)
                 (* SIZE 1/2)
                 (scene+line
                  (scene+line
                   (scene+line
                    (scene+line
                     MTS
                     (/ (- SIZE LINE-LENGTH)2) (* SIZE 2/3)
                     (- SIZE (/ (- SIZE LINE-LENGTH)2)) (* SIZE 2/3)
                     (make-pen LINE-COLOR (round LINE-STROKE) "solid" "round" "round"))
                    (/ (- SIZE LINE-LENGTH)2) (* SIZE 1/3)
                    (- SIZE (/ (- SIZE LINE-LENGTH)2)) (* SIZE 1/3)
                    (make-pen LINE-COLOR (round LINE-STROKE) "solid" "round" "round"))
                   (* SIZE 2/3) (/ (- SIZE LINE-LENGTH)2)
                   (* SIZE 2/3) (- SIZE (/ (- SIZE LINE-LENGTH)2))
                   (make-pen LINE-COLOR (round LINE-STROKE) "solid" "round" "round"))
                  (* SIZE 1/3) (/ (- SIZE LINE-LENGTH)2)
                  (* SIZE 1/3) (- SIZE (/ (- SIZE LINE-LENGTH)2))
                  (make-pen LINE-COLOR (round LINE-STROKE) "solid" "round" "round"))))))

(check-expect (render (make-ws B1X "O" 1 "X"))
              (place-image (text
                            "X won!"
                            TEXT-SIZE TEXT-COLOR)
                           (* SIZE 0.5) (* SIZE 0.5) MTS))





(define (mouse-click ws mouse-x mouse-y button-pressed)
  (if (and (string=? (ws-turn ws) "X") (mouse=? button-pressed "button-down"))
      (local [(define (pos-from-x-y x y)
                
                

                
                (+
                 (floor (/ (* x 3) SIZE))
                 (* (floor (/ (* y 3) SIZE)) 3)))

              
              (define (clamp num min max)
                (cond [(< num min) min]
                      [(> num max) max]
                      [else num]))

              
              (define new-board
                (set-move
                 (ws-board ws)
                 (pos-from-x-y
                  (clamp mouse-x 1 (- SIZE 1))
                  (clamp mouse-y 1 (- SIZE 1)))
                 "X"))]
        (make-ws
         new-board
         "O"
         (ws-dif ws)
         (game-over? new-board)))
      ws))


(check-expect (mouse-click (make-ws B3I "X" 2 false) (* SIZE 1/2) (* SIZE 5/6) "button-down")
              (make-ws (set-move B3I 7 "X") "O" 2 "X"))





(define (key-press ws key)
  (make-ws
   (ws-board ws)
   (ws-turn ws)
   (cond [(key=? key "0") 0]
         [(key=? key "1") 1]
         [(key=? key "2") 2]
         [else (ws-dif ws)])
   (ws-game-over ws)))


(check-expect (key-press (make-ws B4I "O" 2 false) "1")
              (make-ws B4I "O" 1 false))


(check-expect (key-press (make-ws B6I "O" 0 false) "b")
              (make-ws B6I "O" 0 false))






(define (make-computer-move-random board)
  (local [(define possible-moves (open-squares board))]
    (set-move
     board
     (list-ref possible-moves (random (length possible-moves)))
     "O")
    )
  )


(define (make-computer-move-1 board)
  (local [(define possible-moves (open-squares board))
          (define (find-move moves)

            
            (cond [(empty? moves)
                   (list-ref possible-moves (random (length possible-moves)))]
                  
                  [else
                   
                   (if (not (false? (game-over? (set-move
                                                 board
                                                 (first moves)
                                                 "O"))))
                       
                       (if (string=? (game-over? (set-move
                                                  board
                                                  (first moves)
                                                  "O"))
                                     "O")
                           (first moves)
                           (find-move (rest moves)))
                       (find-move (rest moves)))]))]

    (set-move
     board
     (find-move possible-moves)
     "O")))


(check-expect (make-computer-move-1 B4I)
              (set-move B4I 6 "O"))

(check-expect (make-computer-move-1 B5I)
              (set-move B5I 5 "O"))


(define (make-computer-move-2 board)
  (local [(define possible-moves (open-squares board))
          (define (game-ender? pos move)
            (string? (game-over? (set-move
                                  board
                                  pos
                                  move))))
            
          (define (find-winning-move moves)
            (cond [(empty? moves) false]
                  [else
                   (if (game-ender? (first moves) "O")
                       (first moves)
                       (find-winning-move (rest moves)))]))

          (define (find-blocking-move moves)
            (cond [(empty? moves)
                   (list-ref possible-moves (random (length possible-moves)))]
                  [else
                   (if (game-ender? (first moves) "X")
                       (first moves)
                       (find-blocking-move (rest moves)))]))]
    (set-move
     board
     (if (not (false? (find-winning-move possible-moves)))
         (find-winning-move possible-moves)
         (find-blocking-move possible-moves))
     "O")))


(check-expect (make-computer-move-2 B5I)
              (set-move B5I 5 "O"))


(check-expect (make-computer-move-2 B6I)
              (set-move B6I 1 "O"))




(define (set-move board pos move)
  (local [(define old-cell (list-ref board pos))]
    (list-set board pos (make-cell
                         move
                         pos
                         (select-img move)
                         (cell-xmin old-cell)
                         (cell-xmax old-cell)
                         (cell-ymin old-cell)
                         (cell-ymax old-cell)))))


(check-expect (set-move B3I 7 "X")
              (make-board (list "X" "X" "O"
                                "O" "X" " "
                                " " "X" "O")))

(check-expect (set-move B6I 3 "O")
              (make-board (list "X" " " "X"
                                "O" "O" " "
                                " " " " " ")))





(define (get-move board pos)
  (cell-player (list-ref board pos)))

(check-expect (get-move B2O 8)
              "O")
(check-expect (get-move B4I 1)
              "X")






(define (select-img m)
  (cond [(string=? m "X") X-IMG]
        [(string=? m "O") O-IMG]
        [else empty-image]))


(check-expect (select-img "X")
              X-IMG)
(check-expect (select-img "O")
              O-IMG)
(check-expect (select-img " ")
              empty-image)
        





(define (make-board lst)
  (list
   
   (make-cell (first   lst) 0 (select-img (first lst))
              0            (* SIZE 1/3) 0            (* SIZE 1/3)) 
   
   (make-cell (second  lst) 1 (select-img (second lst))
              (* SIZE 1/3) (* SIZE 2/3) 0            (* SIZE 1/3)) 
   
   (make-cell (third   lst) 2 (select-img (third lst))
              (* SIZE 2/3) SIZE         0            (* SIZE 1/3)) 
   

   (make-cell (fourth  lst) 3 (select-img (fourth lst))
              0            (* SIZE 1/3) (* SIZE 1/3) (* SIZE 2/3)) 
   
   (make-cell (fifth   lst) 4 (select-img (fifth lst))
              (* SIZE 1/3) (* SIZE 2/3) (* SIZE 1/3) (* SIZE 2/3)) 
   
   (make-cell (sixth   lst) 5 (select-img (sixth lst))
              (* SIZE 2/3) SIZE         (* SIZE 1/3) (* SIZE 2/3)) 
   

   (make-cell (seventh lst) 6 (select-img (seventh lst))
              0            (* SIZE 1/3) (* SIZE 2/3) SIZE)         
   
   (make-cell (eighth  lst) 7 (select-img (eighth lst))
              (* SIZE 1/3) (* SIZE 2/3) (* SIZE 2/3) SIZE)         
   
   (make-cell (ninth   lst) 8 (select-img (ninth lst))
              (* SIZE 2/3) SIZE         (* SIZE 2/3) SIZE)))       


(define B1X (make-board (list "X" "O" "X"
                              "X" "O" "O"
                              "X" " " " "))) 
(define B2O (make-board (list "X" "X" "O"
                              "O" "X" "O"
                              "X" " " "O"))) 
(define B3I (make-board (list "X" "X" "O"
                              "O" "X" " "
                              " " " " "O"))) 
(define B4I (make-board (list "X" "X" "O"
                              " " "O" " "
                              " " "X" " "))) 
(define B5I (make-board (list "X" "X" "O"
                              "O" "O" " "
                              "X" "X" " "))) 
(define B6I (make-board (list "X" " " "X"
                              " " "O" " "
                              " " " " " "))) 






(define (open-squares board)
  (map (λ (c) (cell-id c)) (filter (λ (c) (string=? (cell-player c) " ")) board)))

(check-expect (open-squares B1X) (list 7 8))
(check-expect (open-squares B2O) (list 7))
(check-expect (open-squares B3I) (list 5 6 7))














  

(define (game-over? board)
  (local [(define (three? player)
            (λ (lst)
              (and
               (string=? player (get-move board (first  lst)))
               (string=? player (get-move board (second lst)))
               (string=? player (get-move board (third  lst)))
               )))
          (define possible-threes
            (list
             (list 0 1 2)
             (list 3 4 5)
             (list 6 7 8)

             (list 0 3 6)
             (list 1 4 7)
             (list 2 5 8)

             (list 0 4 8)
             (list 2 4 6)))]
    (cond
      
      [(ormap (three? "X") possible-threes)"X"]
      [(ormap (three? "O") possible-threes)"O"]
      
      [(andmap
        (λ (c) (not (string=? (cell-player c) " ")))
        board) true]
      
      [else false])))
      
(check-expect (game-over? B1X) "X")
(check-expect (game-over? B2O) "O")
(check-expect (game-over? B3I) false)




(define START
  (make-ws (make-board (list
                        " " " " " "
                        " " " " " "
                        " " " " " "))
           "X"
           0
           false))