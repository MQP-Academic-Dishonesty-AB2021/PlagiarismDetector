

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 4 - James Schibley Bryce Lukens|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 501) 
(define MTS (empty-scene SIZE SIZE))






(define-struct ws (board x-turn? win-state difficulty))


(define WS1 (make-ws (list "" "" "" "" "" "" "" "" "") true "" 0))
(define WS2 (make-ws (list "X" "X" "" "" "" "" "" "" "O") false "" 2))
(define WS3 (make-ws (list "O" "O" "" "" "" "" "" "X" "X") false "" 2))
(define WS4 (make-ws (list "X" "X" "O" "O" "X" "X" "X" "O" "O") false "tie" 0))
(define WS5 (make-ws (list "" "" "" "" "" "" "" "" "") false "" 0))
(define WS6 (make-ws (list "X" "X" "" "" "" "" "" "O" "O") false "" 1))





(check-expect (empty-board 0)
              (local
                [(define PEN (make-pen "goldenrod"
                                       (floor (/ 0 30))
                                       "solid"
                                       "round" "round"))
                 (define BORDER (/ 0 10))
                 (define (empty-board size PEN BORDER)
                   (add-line
                    (add-line
                     (add-line
                      (add-line
                       (empty-scene 0 0)
                       BORDER (* 2 (/ 0 3)) (- 0 BORDER) (* 2 (/ 0 3)) PEN)
                      (* 2 (/ 0 3)) BORDER (* 2 (/ 0 3)) (- 0 BORDER) PEN )
                     BORDER (/ 0 3) (- 0 BORDER) (/ 0 3) PEN)
                    (/ 0 3) BORDER (/ 0 3) (- 0 BORDER) PEN)
                   )]
                (empty-board 0 PEN BORDER)))

(check-expect (empty-board 500)
              (local
                [(define PEN
                   (make-pen
                    "goldenrod" (floor (/ 500 30)) "solid" "round" "round"))
                 (define BORDER (/ 500 10))
                 (define (empty-board size PEN BORDER)
                   (add-line
                    (add-line
                     (add-line
                      (add-line
                       (empty-scene 500 500)
                       BORDER (* 2 (/ 500 3))
                       (- 500 BORDER)
                       (* 2 (/ 500 3))
                       PEN)
                      (* 2 (/ 500 3))
                      BORDER
                      (* 2 (/ 500 3))
                      (- 500 BORDER)
                      PEN )
                     BORDER (/ 500 3) (- 500 BORDER) (/ 500 3) PEN)
                    (/ 500 3) BORDER (/ 500 3) (- 500 BORDER) PEN)
                   )]
                (empty-board 500 PEN BORDER)))


(define (empty-board size)
  (local
    [(define PEN (make-pen "goldenrod"
                           (floor (/ size 30)) "solid" "round" "round"))
     (define BORDER (/ size 10))
     (define (empty-board size PEN BORDER)
       (add-line
        (add-line
         (add-line
          (add-line
           (empty-scene size size)
           BORDER (* 2 (/ size 3))
           (- size BORDER)
           (* 2 (/ size 3))
           PEN)
          (* 2 (/ size 3)) BORDER
          (* 2 (/ size 3)) (- size BORDER) PEN )
         BORDER (/ size 3) (- size BORDER) (/ size 3) PEN)
        (/ size 3) BORDER (/ size 3) (- size BORDER) PEN)
       )]
    (empty-board size PEN BORDER)))



(define WS (make-ws (list "" "" "" "" "" "" "" "" "") true "" 0))



(define (main WS)
  (big-bang WS
    (on-tick computer-make-move)
    (on-mouse make-move)
    (on-key change-difficulty)
    (to-draw render-ws)
    
    )
  )




(check-expect (make-move WS1 1 1 "button-up")
              (make-ws (list "X" "" "" "" "" "" "" "" "") false "" 0))
(check-expect (make-move WS1 (/ SIZE 2)
                         (/ SIZE 2) "button-up")
              (make-ws (list "" "" "" "" "X" "" "" "" "") false "" 0))
(check-expect (make-move WS4 1 1 "button-up") WS4)

(define (make-move ws x y mouse)
  (if (and (mouse=? mouse "button-up")
           (= (string-length (ws-win-state ws)) 0)
           (string=? "" (list-ref (ws-board ws) (get-position-index x y)))
           (> x 0)
           (> y 0)
           (< x (- SIZE 1))
           (< y (- SIZE 1)))
      (set-win-state (make-ws
                      (insert (ws-board ws) "X" (get-position-index x y))
                      (not (ws-x-turn? ws))
                      (ws-win-state ws)
                      (ws-difficulty ws)))
      ws)
  )




(check-expect (get-position-index 1 1) 0)
(check-expect (get-position-index (/ SIZE 2) 1) 1)
(check-expect (get-position-index (- SIZE 1) (- SIZE 1)) 8)


(define (get-position-index x y)
  (+ (floor (/ x (/ SIZE 3))) (* 3 (floor (/ y (/ SIZE 3)))))
  )




(check-expect (insert (list "") "" 0) (list ""))
(check-expect (insert
               (list "" "" "" "" "" "" "" "" "") "X" 1)
              (list "" "X" "" "" "" "" "" "" ""))
(check-expect (insert
               (list "X" "X" "O" "O" "X" "X" "X" "O" "O") "X" 8)
              (list "X" "X" "O" "O" "X" "X" "X" "O" "X"))

(define (insert list element index)
  (local
    [(define (insert list element index acc)
       (cond
         [(= index acc)
          (cons element (rest list))]
         [else
          (cons (first list) (insert (rest list) element index (+ acc 1)))]
         )
       )]
    (insert list element index 0)
    )
  )




(check-expect (render-ws WS1)
              (place-image
               (text
                (cond
                  [(= (string-length "") 0)
                   ""]
                  [(= (string-length "") 1)
                   (string-append "" " WINS!")]
                  [else
                   "TIE!"])
                (floor (/ SIZE 6)) "green")
               (/ SIZE 2) (/ SIZE 2)
               (draw-board (list "" "" "" "" "" "" "" "" ""))
               )
              )
(check-expect (render-ws WS2)
              (place-image
               (text
                (cond
                  [(= (string-length "") 0)
                   ""]
                  [(= (string-length "") 1)
                   (string-append "" " WINS!")]
                  [else
                   "TIE!"])
                (floor (/ SIZE 6)) "green")
               (/ SIZE 2) (/ SIZE 2)
               (draw-board (list "X" "X" "" "" "" "" "" "" "O"))
               )
              )
(check-expect (render-ws WS4)
              (place-image
               (text
                (cond
                  [(= (string-length "tie") 0)
                   ""]
                  [(= (string-length "tie") 1)
                   (string-append "tie" " WINS!")]
                  [else
                   "TIE!"])
                (floor (/ SIZE 6)) "green")
               (/ SIZE 2) (/ SIZE 2)
               (draw-board (list "X" "X" "O" "O" "X" "X" "X" "O" "O"))
               )
              )
              


(define (render-ws ws)
  (place-image (text
                (cond
                  [(= (string-length (ws-win-state ws)) 0)
                   ""]
                  [(= (string-length (ws-win-state ws)) 1)
                   (string-append (ws-win-state ws) " WINS!")]
                  [else
                   "TIE!"])
                (floor (/ SIZE 6)) "green")
               (/ SIZE 2) (/ SIZE 2)
               (draw-board (ws-board ws))
               )
  )




(check-expect (draw-board (list "" "" "" "" "" "" "" "" ""))
              (local
                [(define (draw-board board pos)
                   (cond
                     [(empty? board) (empty-board SIZE)]
                     [else
                      (place-image
                       (text (first board)
                             (floor (/ SIZE 4))
                             (if (string=? (first board) "X") "red" "blue"))
                       (+ (* (floor (/ SIZE 3))
                             (modulo pos 3)) (floor (/ SIZE 6)))
                       (+ (* (/ SIZE 3)
                             (floor (/ pos 3))) (floor (/ SIZE 6)))
                       (draw-board (rest board) (+ 1 pos)))
                      ]
                     )
                   )]
                (draw-board (list "" "" "" "" "" "" "" "" "") 0)
                )
              )

(check-expect (draw-board (list "X" "X" "" "" "" "" "" "" "O"))
              (local
                [(define (draw-board board pos)
                   (cond
                     [(empty? board) (empty-board SIZE)]
                     [else
                      (place-image
                       (text (first board)
                             (floor (/ SIZE 4))
                             (if (string=? (first board) "X") "red" "blue"))
                       (+ (* (floor (/ SIZE 3))
                             (modulo pos 3)) (floor (/ SIZE 6)))
                       (+ (* (/ SIZE 3)  (floor
                                          (/ pos 3))) (floor (/ SIZE 6)))
                       (draw-board (rest board) (+ 1 pos)))
                      ]
                     )
                   )]
                (draw-board (list "X" "X" "" "" "" "" "" "" "O") 0)
                )
              )

(check-expect (draw-board (list "X" "X" "O" "O" "X" "X" "X" "O" "O"))
              (local
                [(define (draw-board board pos)
                   (cond
                     [(empty? board) (empty-board SIZE)]
                     [else
                      (place-image
                       (text (first board)
                             (floor (/ SIZE 4))
                             (if (string=? (first board) "X") "red" "blue"))
                       (+ (* (floor (/ SIZE 3))
                             (modulo pos 3)) (floor (/ SIZE 6)))
                       (+ (* (/ SIZE 3)  (floor
                                          (/ pos 3))) (floor (/ SIZE 6)))
                       (draw-board (rest board) (+ 1 pos)))
                      ]
                     )
                   )]
                (draw-board (list "X" "X" "O" "O" "X" "X" "X" "O" "O") 0)
                )
              )

              

(define (draw-board board)
  (local
    [(define (draw-board board pos)
       (cond
         [(empty? board) (empty-board SIZE)]
         [else
          (place-image
           (text
            (first board)
            (floor (/ SIZE 4))
            (if (string=? (first board) "X") "red" "blue"))
           (+ (* (floor (/ SIZE 3))
                 (modulo pos 3)) (floor (/ SIZE 6)))
           (+ (* (/ SIZE 3)  (floor
                              (/ pos 3))) (floor (/ SIZE 6)))
           (draw-board (rest board) (+ 1 pos)))
          ]
         )
       )]
    (draw-board board 0)
    )
  )



(check-expect (computer-make-move  WS4) WS4)
(check-expect (computer-make-move  WS1) WS1)
(check-expect (computer-make-move  WS2)
              (set-win-state
               (make-ws
                (2-deep-search
                 (list "X" "X" "" "" "" "" "" "" "O")) true "" 2)))
(check-random (computer-make-move  WS5)
              (set-win-state
               (make-ws
                (insert
                 (list "" "" "" "" "" "" "" "" "") "O"
                 (create-random-move
                  (list "" "" "" "" "" "" "" "" "")
                  (random 9))) true "" 0)))
(check-expect (computer-make-move  WS6)
              (set-win-state
               (make-ws
                (1-deep-search
                 (list "X" "X" "" "" "" "" "" "O" "O")) true "" 1)))

(define (computer-make-move ws)
  (if (and (not (ws-x-turn? ws)) (= (string-length (ws-win-state ws)) 0))
      (cond
        [(= (ws-difficulty ws) 0)
         (set-win-state
          (make-ws
           (insert (ws-board ws)
                   "O"
                   (create-random-move (ws-board ws) (random 9)))
           (not (ws-x-turn? ws)) (ws-win-state ws) (ws-difficulty ws)))
         ]
        [(= (ws-difficulty ws) 1)
         (set-win-state
          (make-ws
           (1-deep-search (ws-board ws))
           (not (ws-x-turn? ws))
           (ws-win-state ws)
           (ws-difficulty ws)))]
        [(= (ws-difficulty ws) 2)
         (set-win-state
          (make-ws
           (2-deep-search (ws-board ws))
           (not (ws-x-turn? ws))
           (ws-win-state ws) (ws-difficulty ws)))]        
        )
      ws
      )
  )




(check-expect (create-random-move
               (list "" "" "" "" "" "" "" "" "") 0) 0)
(check-random (create-random-move
               (list "X" "" "" "" "" "" "" "" "") 0) (random 9))
(check-expect (create-random-move
               (list "X" "X" "X" "X" "X" "X" "X" "X" "") 0) 8)


(define (create-random-move board move)
  (if (string=? "" (list-ref board move))
      move
      (create-random-move board (random 9))
      )
  )





(check-expect (set-win-state
               (make-ws
                (list "X" "X" "X" "" "" "" "" "O" "O") true "" 2))
              (make-ws (list "X" "X" "X" "" "" "" "" "O" "O") true "X" 2))
(check-expect (set-win-state
               (make-ws
                (list "O" "O" "O" "X" "" "" "" "X" "X") true "" 2))
              (make-ws (list "O" "O" "O" "X" "" "" "" "X" "X") true "O" 2))
(check-expect (set-win-state
               (make-ws
                (list "X" "X" "O" "O" "X" "X" "X" "O" "O") false "" 0))
              (make-ws (list "X" "X" "O" "O" "X" "X" "X" "O" "O") false "tie" 0))



(define (set-win-state ws)
  (cond
    [(check-win 0 1 2 (ws-board ws))
     (make-ws (ws-board ws)
              (ws-x-turn? ws)
              (list-ref (ws-board ws) 0)
              (ws-difficulty ws))]
    [(check-win 3 4 5 (ws-board ws))
     (make-ws (ws-board ws)
              (ws-x-turn? ws)
              (list-ref (ws-board ws) 3)
              (ws-difficulty ws))]
    [(check-win 6 7 8 (ws-board ws))
     (make-ws (ws-board ws)
              (ws-x-turn? ws)
              (list-ref (ws-board ws) 6)
              (ws-difficulty ws))]
    [(check-win 0 3 6 (ws-board ws))
     (make-ws (ws-board ws)
              (ws-x-turn? ws)
              (list-ref (ws-board ws) 0)
              (ws-difficulty ws))]
    [(check-win 1 4 7 (ws-board ws))
     (make-ws (ws-board ws)
              (ws-x-turn? ws)
              (list-ref (ws-board ws) 1)
              (ws-difficulty ws))]
    [(check-win 2 5 8 (ws-board ws))
     (make-ws (ws-board ws)
              (ws-x-turn? ws)
              (list-ref (ws-board ws) 2)
              (ws-difficulty ws))]
    [(check-win 0 4 8 (ws-board ws))
     (make-ws (ws-board ws)
              (ws-x-turn? ws)
              (list-ref (ws-board ws) 0)
              (ws-difficulty ws))]
    [(check-win 2 4 6 (ws-board ws))
     (make-ws (ws-board ws)
              (ws-x-turn? ws)
              (list-ref (ws-board ws) 2)
              (ws-difficulty ws))]
    [(is-tie? (ws-board ws))
     (make-ws (ws-board ws)
              (ws-x-turn? ws)
              "tie"
              (ws-difficulty ws))]
    [else (make-ws (ws-board ws)
                   (ws-x-turn? ws)
                   (ws-win-state ws)
                   (ws-difficulty ws))]
    )
  )




(check-expect (check-win-state (list "" "" "" "" "" "" "" "" "" )) false)
(check-expect (check-win-state (list "X" "X" "X" "" "" "" "" "O" "O")) true)
(check-expect (check-win-state (list "X" "" "" "O" "O" "O" "X" "" "X")) true)
(check-expect (check-win-state (list "X" "X" "O" "O" "X" "X" "X" "O" "O"))
              false)


(define (check-win-state board)
  (cond
    [(check-win 0 1 2 board)
     true]
    [(check-win 3 4 5 board)
     true]
    [(check-win 6 7 8 board)
     true]
    [(check-win 0 3 6 board)
     true]
    [(check-win 1 4 7 board)
     true]
    [(check-win 2 5 8 board)
     true]
    [(check-win 0 4 8 board)
     true]
    [(check-win 2 4 6 board)
     true]
    [else false]))




(check-expect (check-win 0 1 2 (list "" "" "" "" "" "" "" "" "" )) false)
(check-expect (check-win 0 1 2 (list "X" "X" "X" "" "" "" "" "O" "O")) true)
(check-expect (check-win 3 4 5 (list "X" "" "" "O" "O" "O" "X" "" "X")) true)
(check-expect (check-win 0 1 2 (list "X" "X" "O" "O" "X" "X" "X" "O" "O"))
              false)

(define (check-win m1 m2 m3 board)
  (and (not (string=? (list-ref board m1) "")) (string=?
                                                (list-ref board m1)
                                                (list-ref board m2)
                                                (list-ref board m3)
                                                )
       )
  )



(check-expect (is-tie? (list "" "" "" "" "" "" "" "" "")) false)
(check-expect (is-tie? (list "X" "O" "X" "O" "X" "X" "O" "X" "O")) true)
(check-expect (is-tie? (list "X" "O" "X" "O" "X" "X" "" "X" "O")) false)

(define (is-tie? board)
  (not (ormap (λ (n) (string=? n "")) board))
  )



(check-expect (change-difficulty WS1 "1")
              (make-ws (list "" "" "" "" "" "" "" "" "") true "" 1))
(check-expect (change-difficulty WS2 "0")
              (make-ws (list "X" "X" "" "" "" "" "" "" "O") false "" 0))
(check-expect (change-difficulty WS3 "1")
              (make-ws (list "O" "O" "" "" "" "" "" "X" "X") false "" 1))
(check-expect (change-difficulty WS4 "a")
              WS4)
                                         
(define (change-difficulty ws key)
  (cond
    [(key=? key "0")
     (make-ws (ws-board ws) (ws-x-turn? ws) (ws-win-state ws) 0)]
    [(key=? key "1")
     (make-ws (ws-board ws) (ws-x-turn? ws) (ws-win-state ws) 1)]
    [(key=? key "2")
     (make-ws (ws-board ws) (ws-x-turn? ws) (ws-win-state ws) 2)]
    [else
     ws]
    )
  )




(define-struct next-move (index turn))



(check-expect (1-deep-search
               (list "X" "X" "" "O" "O" "" "" "" "X"))
              (list "X" "X" "" "O" "O" "O" "" "" "X"))
(check-random (1-deep-search
               (list "" "" "" "" "" "" "" "" ""))
              (insert (list "" "" "" "" "" "" "" "" "") "O"
                      (create-random-move (list "" "" "" "" "" "" "" "" "")
                                          (random 9))))
(check-expect (1-deep-search
               (list "O" "X" "" "" "O" "" "X" "" ""))
              (list "O" "X" "" "" "O" "" "X" "" "O"))


(define (1-deep-search board)
  (local
    [(define result (check-next-move board (get-next-board board "O") "O"))]
    (if (false? result)
        (insert board "O" (create-random-move board (random 9)))
        (insert board "O" (next-move-index result))
        )
    )
  )



(check-random (2-deep-search (list "" "" "" "" "" "" "" "" ""))
              (insert (list "" "" "" "" "" "" "" "" "") "O"
                      (create-random-move (list "" "" "" "" "" "" "" "" "")
                                          (random 9))))
(check-expect (2-deep-search (list "X" "X" "" "O" "O" "" "" "" "X"))
              (list "X" "X" "" "O" "O" "O" "" "" "X"))
(check-expect (2-deep-search (list "X" "X" "" "" "" "" "" "" "O"))
              (list "X" "X" "O" "" "" "" "" "" "O"))


(define (2-deep-search board)
  (local
    [(define result (check-next-move board (get-next-board board "O") "O"))]
    (if (false? result)
        (local
          [(define result2 (check-next-move board
                                            (get-next-board board "X") "X"))]
          (if (false? result2)
              (insert board "O" (create-random-move board (random 9)))
              (insert board "O" (next-move-index result2))
              )
          )
        (insert board "O" (next-move-index result))
        )
    )
  )




(check-expect (check-next-move (list "" "" "" "" "" "" "" "" "")
                               (list
                                (make-next-move 0 "X")
                                (make-next-move 1 "X")
                                (make-next-move 2 "X")
                                (make-next-move 3 "X")
                                (make-next-move 4 "X")
                                (make-next-move 5 "X")
                                (make-next-move 6 "X")
                                (make-next-move 7 "X")
                                (make-next-move 8 "X")) "X") false)
                               
(check-expect (check-next-move
               (list "X" "O" "X" "O" "O" "X" "" "X" "")
               (list (make-next-move 6 "O") (make-next-move 8 "O")) "O") false)
(check-expect (check-next-move
               (list "X" "X" "" "" "" "" "" "" "O")
               (list (make-next-move 2 "X") (make-next-move 3 "X")
                     (make-next-move 4 "X") (make-next-move 5 "X")
                     (make-next-move 6 "X") (make-next-move 7 "X")) "X")
              (make-next-move 2 "X")) 

(define (check-next-move board lom turn)
  (cond
    [(empty? lom) false]
    [(check-win-state (insert board (next-move-turn (first lom))
                              (next-move-index (first lom))))
     (first lom)]
    [else (check-next-move board(rest lom) turn)]
    )
  )




(check-expect (get-next-board (list "" "" "" "" "" "" "" "" "") "X")
              (list
               (make-next-move 0 "X")
               (make-next-move 1 "X")
               (make-next-move 2 "X")
               (make-next-move 3 "X")
               (make-next-move 4 "X")
               (make-next-move 5 "X")
               (make-next-move 6 "X")
               (make-next-move 7 "X")
               (make-next-move 8 "X"))
              )

(check-expect (get-next-board (list "X" "O" "X" "O" "O" "X" "" "X" "") "O")
              (list (make-next-move 6 "O") (make-next-move 8 "O"))
              )

(define (get-next-board board turn)
  (local
    [(define (get-next-board board2 turn)
       (cond
         [(empty? board2) empty]
         [(string=? (first board2) "")
          (cons (make-next-move (- 9 (length board2)) turn)
                (get-next-board (rest board2) turn))]
         [else (get-next-board (rest board2) turn)]
         )
       )]
    (get-next-board board turn)
    )
  )