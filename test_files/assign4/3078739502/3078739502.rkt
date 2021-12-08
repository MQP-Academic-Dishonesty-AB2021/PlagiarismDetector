

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1102 assignment 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 900) 
(define MTS (empty-scene SIZE SIZE))







(define OFFSET (/ SIZE 10))


(define PEN-WIDTH (/ SIZE 30))


(define L1 (line 0 (- SIZE (* 2 OFFSET)) 
                 (make-pen 
                  "dim gray"
                  PEN-WIDTH
                  "solid"
                  "round" 
                  "round")))


(define BOARD-IMAGE
  (place-image 
   L1 (/ SIZE 3) (/ SIZE 2)
   (place-image 
    L1 (* 2/3 SIZE) (/ SIZE 2)
    (place-image 
     (rotate 90 L1)
     (/ SIZE 2) 
     (/ SIZE 3)
     (place-image 
      (rotate 90 L1)
      (/ SIZE 2)
      (* 2/3 SIZE)
      MTS)))))
                  

(define X "X")


(define O "O")


(define B false)


(define SQUARE-SIZE (/ SIZE 3))


(define CENTER (/ SIZE 2))




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




(define UNITS 
  (append ROWS COLS DIAGS))


(define X-COLOR "red")


(define X-IMG
  (text "X" (/ SIZE 4) X-COLOR))


(define O-COLOR "blue")


(define O-IMG
  (text "O" (/ SIZE 4) O-COLOR))


(define FONT-SIZE (/ SIZE 6))


(define FONT-COLOR "black")


(define WIN-COLOR "green")













(define BD1
  (list B B B
        B B B
        B B B)) 


(define BD2
  (list B X B
        O B B
        B B X))


(define BD3
  (list B X O
        B O X
        O B B)) 


(define BD4 
  (list X O X
        O X O
        X O X)) 


(define BD5 
  (list X O X 
        O X O
        X O B))  


(define BD6 
  (list B O X 
        O X O
        X O X)) 


(define BD7
  (list O B B
        B X B
        B B B))


(define BD8
  (list X O B
        B X B
        B O B))
   

(define BD9
  (list O X X
        B O B
        X B B))
   

(define BD10
  (list X O X
        X O X
        B X O))


(define BD11
  (list O X X
        B O B
        X B O))


(define BD12
  (list X O B
        B X B
        B O X))


(define BD13
  (list X O O
        O X X
        X X O))











(define-struct ws (bd winner player difficulty))


(define WS1 (make-ws BD1 "" X 0))
(define WS2 (make-ws BD3 O O 0))
(define WS3 (make-ws BD2 "" O 0))
(define WS4 (make-ws BD7 "" X 0))
(define WS5 (make-ws BD8 "" X 0))
(define WS5-2 (make-ws BD8 "" X 2))
(define WS6 (make-ws BD9 "" O 0))
(define WS6-1 (make-ws BD9 "" O 1))
(define WS7 (make-ws BD10 "" O 0))
(define WS8 (make-ws BD13 "draw" O 0))
(define WS9 (make-ws BD12 "X" O 0))
(define WS10 (make-ws BD8 "" O 2))
(define WS11 (make-ws BD11 "O" X 0))


(define START (make-ws BD1 "" X 0))

 










(define (main ws)
  (big-bang ws
    (on-tick advance-ws-comp)
    (to-draw render-ws)
    (on-key update-difficulty)
    (on-mouse advance-ws-user)))












(define (find-all-valid-pos bd) 
  (local 
    [(define (find-all-valid-pos-inner bd acc) 
       (cond [(empty? bd) empty]
             [(false? (first bd)) 
              (cons acc (find-all-valid-pos-inner (rest bd) (+ 1 acc)))]
             [else 
              (find-all-valid-pos-inner (rest bd) (+ 1 acc))]))]
    (find-all-valid-pos-inner bd 0)))




(check-expect (find-all-valid-pos BD1)
              (build-list 9 identity))


(check-expect (find-all-valid-pos BD2)
              (list 0 2 4 5 6 7))


(check-expect (find-all-valid-pos BD3)
              (list 0 3 7 8))          


(check-expect (find-all-valid-pos BD4)
              empty)


(check-expect (find-all-valid-pos empty)
              empty)


(check-expect (find-all-valid-pos BD5)
              (list 8))


(check-expect (find-all-valid-pos BD6)
              (list 0))











      
(define (advance-ws-comp ws)
  (local
    
    
    
    
    
    
    
    [(define (choose-pos-comp-depth ws valid-pos player-ref)
       (cond [(empty? valid-pos) 
         
              
              (if (and (= (ws-difficulty ws) 2) 
                       (string=? player-ref O))
             
                  
                  
                  (choose-pos-comp-depth 
                   ws 
                   (find-all-valid-pos 
                    (ws-bd ws)) 
                   X)
             
                  
                  (choose-pos-comp
                   (find-all-valid-pos (ws-bd ws))))]

             
             
             [(string=?
               (check-win-state 
                (list-set (ws-bd ws) (first valid-pos) player-ref) 
                player-ref) 
               player-ref)
              (first valid-pos)]

             
             [else 
              (choose-pos-comp-depth ws (rest valid-pos) player-ref)]))


     
     
     
     
     (define (choose-pos-comp lop) 
       (list-ref lop (random (length lop))))

     
     
     (define (advance-ws-comp-inner ws)
       (if (and (string=? (ws-winner ws) "") 
                (string=? (ws-player ws) O)) 
           (if
            (or
             (= (ws-difficulty ws) 1)
             (= (ws-difficulty ws) 2)) 
            (update-ws
             ws
             (choose-pos-comp-depth 
              ws
              (find-all-valid-pos (ws-bd ws))
              O))
            (update-ws
             ws
             (choose-pos-comp 
              (find-all-valid-pos (ws-bd ws)))))
           ws))] 
  
    (advance-ws-comp-inner ws))) 





(check-expect (advance-ws-comp WS1) WS1)


(check-expect (advance-ws-comp WS2) WS2)


(check-expect (advance-ws-comp WS10)
              (make-ws
               (list-set (ws-bd WS10) 8 O)
               ""
               X
               2))



(check-expect (advance-ws-comp WS6-1)
              (make-ws
               (list-set (ws-bd WS6-1) 8 O)
               "O"
               X
               1))

  









(define (advance-ws-user ws x y mouse-event)
  (local
    
    
    
    
    [(define (norm x)
       (min 2 
            (floor (/ x SQUARE-SIZE))))

     
     
     
     (define (x-y->pos x y) 
       (+ (* (norm y) 3)
          (norm x)))

     
     (define pos (x-y->pos x y))]
    
    (if (and (string=? (ws-player ws) X) 
             
             (string=? mouse-event "button-down") 
             (string=? (ws-winner ws) "") 
             
             (member pos (find-all-valid-pos (ws-bd ws)))) 
        (update-ws ws pos)
        ws)))





(check-expect (advance-ws-user WS1 (sub1 SIZE) 5 "button-down")
              (make-ws (list B B X B B B B B B)
                       ""
                       O
                       0))

(check-expect (advance-ws-user WS1 CENTER CENTER "button-down")
              (make-ws (list B B B B X B B B B)
                       ""
                       O
                       0))

(check-expect (advance-ws-user WS3 2 5 "button-down")
              WS3)


(check-expect (advance-ws-user WS4 -1 0 "button-down") WS4)


(check-expect (advance-ws-user WS1 10 5 "drag") WS1)


(check-expect (advance-ws-user WS2 10 5 "button-down") WS2)


      
  






(define (update-ws ws pos)
  (local [(define (next-player player) 
            (if (string=? player X)
                O 
                X))

          (define new-bd (list-set (ws-bd ws) pos (ws-player ws)))]
    
    (make-ws new-bd
             
             (check-win-state new-bd (ws-player ws))
             
             (next-player (ws-player ws))
             
             (ws-difficulty ws))))




(check-expect (update-ws WS1 2)
              (make-ws (list B B X B B B B B B)
                       "" O 0))


(check-expect (update-ws WS3 5)
              (make-ws (list B X B O B O B B X)
                       "" X 0))


(check-expect (update-ws WS5 8)
              (make-ws (list X O B B X B B O X)
                       X O 0))


(check-expect (update-ws WS6 8)
              (make-ws (list O X X B O B X B O)
                       O X 0))


(check-expect (update-ws WS7 6)
              (make-ws (list X O X X O X O X O)
                       "draw" X 0)) 

      





















(define (check-win-state bd player)
  (local
    
    
    [(define (read-unit u) 
       (map read-pos u)) 
          
     
     
     (define (read-pos p) 
       (list-ref bd p))
          
     
     
     (define (read-units LOU) 
       (map read-unit LOU))
          
     
     
     
     (define (winning-unit? u) 
       (andmap player? u)) 
          
     
     
     
     (define (player? p) 
       (if (not (false? p))
           (string=? player p)
           false))
     
     
     
     
     (define draw?
       (empty? (find-all-valid-pos bd)))]
        
    
    
    
    (if (ormap winning-unit? 
               (read-units UNITS))
        player
        (if draw?
            "draw"
            ""))))
          

      

(check-expect (check-win-state BD11 O) O)
      

(check-expect (check-win-state BD12 X) X)
      

(check-expect (check-win-state BD11 X) "")
  

(check-expect (check-win-state BD12 O) "")
  

(check-expect (check-win-state (list B B B
                                     B B B
                                     B B B) X) "")
  

(check-expect (check-win-state BD2 O) "")


(check-expect (check-win-state BD13 O) "draw")
  






(define (render-ws ws)
  (local 
    
    
    [(define (choose-image player)
       (cond [(and (string? player) (string=? player X)) X-IMG]
             [(and (string? player) (string=? player O)) O-IMG]
             [else empty-image]))
    
     
     
     (define (render-squares bd pos)
       (cond [(empty? bd) BOARD-IMAGE]
             [else
              (place-image
               (choose-image (first bd))
               (pos->x pos)
               (pos->y pos)
               (render-squares (rest bd) (+ 1 pos)))]))
     
     
     
     
     (define (pos->x p) 
       (+ 
        (* SQUARE-SIZE 
           (modulo p 3)) 
        (/ SQUARE-SIZE 2)))

     
     
     
     (define (pos->y p) 
       (+
        (* SQUARE-SIZE 
           (floor 
            (/ p 3))) 
        (/ SQUARE-SIZE 2)))

     
     
     
     (define (choose-winner ws)
       (cond [(string=? "draw" (ws-winner ws))
              (text "It's a draw!" FONT-SIZE WIN-COLOR)]
             [(string=? X (ws-winner ws))
              (text "X won!" FONT-SIZE WIN-COLOR)]
             [(string=? O (ws-winner ws))
              (text "O won!" FONT-SIZE WIN-COLOR)]
             [else 
              empty-image]))]
  
    
    
    (overlay
     (choose-winner ws)
     (render-squares (ws-bd ws) 0))))




(check-expect (render-ws WS5)
              (place-image
               X-IMG
               (+ (* SQUARE-SIZE 0) 
                  (/ SQUARE-SIZE 2))
               (+ (* SQUARE-SIZE 0) 
                  (/ SQUARE-SIZE 2))
               (place-image
                O-IMG
                (+ (* SQUARE-SIZE 1) 
                   (/ SQUARE-SIZE 2))
                (+ (* SQUARE-SIZE 0) 
                   (/ SQUARE-SIZE 2))
                (place-image
                 empty-image
                 (+ (* SQUARE-SIZE 2) 
                    (/ SQUARE-SIZE 2))
                 (+ (* SQUARE-SIZE 0) 
                    (/ SQUARE-SIZE 2))
                 (place-image
                  empty-image
                  (+ (* SQUARE-SIZE 0) 
                     (/ SQUARE-SIZE 2))
                  (+ (* SQUARE-SIZE 1) 
                     (/ SQUARE-SIZE 2))
                  (place-image
                   X-IMG
                   (+ (* SQUARE-SIZE 1) 
                      (/ SQUARE-SIZE 2))
                   (+ (* SQUARE-SIZE 1) 
                      (/ SQUARE-SIZE 2))
                   (place-image
                    empty-image
                    (+ (* SQUARE-SIZE 2) 
                       (/ SQUARE-SIZE 2))
                    (+ (* SQUARE-SIZE 1) 
                       (/ SQUARE-SIZE 2))
                    (place-image
                     empty-image
                     (+ (* SQUARE-SIZE 0) 
                        (/ SQUARE-SIZE 2))
                     (+ (* SQUARE-SIZE 2) 
                        (/ SQUARE-SIZE 2))
                     (place-image
                      O-IMG
                      (+ (* SQUARE-SIZE 1) 
                         (/ SQUARE-SIZE 2))
                      (+ (* SQUARE-SIZE 2) 
                         (/ SQUARE-SIZE 2))
                      (place-image
                       empty-image
                       (+ (* SQUARE-SIZE 2) 
                          (/ SQUARE-SIZE 2))
                       (+ (* SQUARE-SIZE 2) 
                          (/ SQUARE-SIZE 2))
                       BOARD-IMAGE))))))))))



(check-expect (render-ws WS2)
              (overlay
               (text "O won!" FONT-SIZE WIN-COLOR)
               (place-image
                empty-image
                (+ (* SQUARE-SIZE 0) 
                   (/ SQUARE-SIZE 2))
                (+ (* SQUARE-SIZE 0) 
                   (/ SQUARE-SIZE 2))
                (place-image
                 X-IMG
                 (+ (* SQUARE-SIZE 1) 
                    (/ SQUARE-SIZE 2))
                 (+ (* SQUARE-SIZE 0) 
                    (/ SQUARE-SIZE 2))
                 (place-image
                  O-IMG
                  (+ (* SQUARE-SIZE 2) 
                     (/ SQUARE-SIZE 2))
                  (+ (* SQUARE-SIZE 0) 
                     (/ SQUARE-SIZE 2))
                  (place-image
                   empty-image
                   (+ (* SQUARE-SIZE 0) 
                      (/ SQUARE-SIZE 2))
                   (+ (* SQUARE-SIZE 1) 
                      (/ SQUARE-SIZE 2))
                   (place-image
                    O-IMG
                    (+ (* SQUARE-SIZE 1) 
                       (/ SQUARE-SIZE 2))
                    (+ (* SQUARE-SIZE 1) 
                       (/ SQUARE-SIZE 2))
                    (place-image
                     X-IMG
                     (+ (* SQUARE-SIZE 2) 
                        (/ SQUARE-SIZE 2))
                     (+ (* SQUARE-SIZE 1) 
                        (/ SQUARE-SIZE 2))
                     (place-image
                      O-IMG
                      (+ (* SQUARE-SIZE 0) 
                         (/ SQUARE-SIZE 2))
                      (+ (* SQUARE-SIZE 2) 
                         (/ SQUARE-SIZE 2))
                      (place-image
                       empty-image
                       (+ (* SQUARE-SIZE 1) 
                          (/ SQUARE-SIZE 2))
                       (+ (* SQUARE-SIZE 2) 
                          (/ SQUARE-SIZE 2))
                       (place-image
                        empty-image
                        (+ (* SQUARE-SIZE 2) 
                           (/ SQUARE-SIZE 2))
                        (+ (* SQUARE-SIZE 2) 
                           (/ SQUARE-SIZE 2))
                        BOARD-IMAGE)))))))))))


(check-expect (render-ws WS8)
              (overlay
               (text "It's a draw!" FONT-SIZE WIN-COLOR)
               (place-image
                X-IMG
                (+ (* SQUARE-SIZE 0) 
                   (/ SQUARE-SIZE 2))
                (+ (* SQUARE-SIZE 0) 
                   (/ SQUARE-SIZE 2))
                (place-image
                 O-IMG
                 (+ (* SQUARE-SIZE 1) 
                    (/ SQUARE-SIZE 2))
                 (+ (* SQUARE-SIZE 0) 
                    (/ SQUARE-SIZE 2))
                 (place-image
                  O-IMG
                  (+ (* SQUARE-SIZE 2) 
                     (/ SQUARE-SIZE 2))
                  (+ (* SQUARE-SIZE 0) 
                     (/ SQUARE-SIZE 2))
                  (place-image
                   O-IMG
                   (+ (* SQUARE-SIZE 0) 
                      (/ SQUARE-SIZE 2))
                   (+ (* SQUARE-SIZE 1) 
                      (/ SQUARE-SIZE 2))
                   (place-image
                    X-IMG
                    (+ (* SQUARE-SIZE 1) 
                       (/ SQUARE-SIZE 2))
                    (+ (* SQUARE-SIZE 1) 
                       (/ SQUARE-SIZE 2))
                    (place-image
                     X-IMG
                     (+ (* SQUARE-SIZE 2) 
                        (/ SQUARE-SIZE 2))
                     (+ (* SQUARE-SIZE 1) 
                        (/ SQUARE-SIZE 2))
                     (place-image
                      X-IMG
                      (+ (* SQUARE-SIZE 0) 
                         (/ SQUARE-SIZE 2))
                      (+ (* SQUARE-SIZE 2) 
                         (/ SQUARE-SIZE 2))
                      (place-image
                       X-IMG
                       (+ (* SQUARE-SIZE 1) 
                          (/ SQUARE-SIZE 2))
                       (+ (* SQUARE-SIZE 2) 
                          (/ SQUARE-SIZE 2))
                       (place-image
                        O-IMG
                        (+ (* SQUARE-SIZE 2) 
                           (/ SQUARE-SIZE 2))
                        (+ (* SQUARE-SIZE 2) 
                           (/ SQUARE-SIZE 2))
                        BOARD-IMAGE)))))))))))
        

(check-expect (render-ws WS9)
              (overlay
               (text "X won!" FONT-SIZE WIN-COLOR)
               (place-image
                X-IMG
                (+ (* SQUARE-SIZE 0) 
                   (/ SQUARE-SIZE 2))
                (+ (* SQUARE-SIZE 0) 
                   (/ SQUARE-SIZE 2))
                (place-image
                 O-IMG
                 (+ (* SQUARE-SIZE 1) 
                    (/ SQUARE-SIZE 2))
                 (+ (* SQUARE-SIZE 0) 
                    (/ SQUARE-SIZE 2))
                 (place-image
                  empty-image
                  (+ (* SQUARE-SIZE 2) 
                     (/ SQUARE-SIZE 2))
                  (+ (* SQUARE-SIZE 0) 
                     (/ SQUARE-SIZE 2))
                  (place-image
                   empty-image
                   (+ (* SQUARE-SIZE 0) 
                      (/ SQUARE-SIZE 2))
                   (+ (* SQUARE-SIZE 1) 
                      (/ SQUARE-SIZE 2))
                   (place-image
                    X-IMG
                    (+ (* SQUARE-SIZE 1) 
                       (/ SQUARE-SIZE 2))
                    (+ (* SQUARE-SIZE 1) 
                       (/ SQUARE-SIZE 2))
                    (place-image
                     empty-image
                     (+ (* SQUARE-SIZE 2) 
                        (/ SQUARE-SIZE 2))
                     (+ (* SQUARE-SIZE 1) 
                        (/ SQUARE-SIZE 2))
                     (place-image
                      empty-image
                      (+ (* SQUARE-SIZE 0) 
                         (/ SQUARE-SIZE 2))
                      (+ (* SQUARE-SIZE 2) 
                         (/ SQUARE-SIZE 2))
                      (place-image
                       O-IMG
                       (+ (* SQUARE-SIZE 1) 
                          (/ SQUARE-SIZE 2))
                       (+ (* SQUARE-SIZE 2) 
                          (/ SQUARE-SIZE 2))
                       (place-image
                        X-IMG
                        (+ (* SQUARE-SIZE 2) 
                           (/ SQUARE-SIZE 2))
                        (+ (* SQUARE-SIZE 2) 
                           (/ SQUARE-SIZE 2))
                        BOARD-IMAGE)))))))))))
















 























(define (update-difficulty ws key-event)
  (local [(define (is-valid-difficulty s)
            (string=? s key-event))]
    (if (ormap is-valid-difficulty (list "0" "1" "2"))
        (make-ws (ws-bd ws)
                 (ws-winner ws)
                 (ws-player ws)
                 (string->number key-event))
        ws)))




(check-expect (update-difficulty WS1 "0") WS1)


(check-expect (update-difficulty WS3 "1") (make-ws BD2 "" O 1))


(check-expect (update-difficulty WS4 "2") (make-ws BD7 "" X 2))


(check-expect (update-difficulty WS5-2 "0") WS5)


(check-expect (update-difficulty WS6 "j") WS6)























 



 




 





