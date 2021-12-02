

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe V20|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define DISPLAY-DIFFICULTY? true) 



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)

(define SIZE 900) 
(define X-COLOR "red")
(define O-COLOR "blue")
(define MTS (empty-scene SIZE SIZE))
(define FONT-SIZE (floor (/ SIZE 6)))
(define winning-combos
  (list (list 1 2 3)
        (list 4 5 6)
        (list 7 8 9)
        (list 1 4 7)
        (list 2 5 8)
        (list 3 6 9)
        (list 1 5 9)
        (list 7 5 3)))










(define-struct spot(player index))


(define top-right(make-spot "X" 3))
(define bottom-left(make-spot "O" 7))
(define center(make-spot "" 5))











(define board1 (list (make-spot "" 1)  (make-spot "" 2)  (make-spot "" 3)
                     (make-spot "X" 4) (make-spot "" 5)  (make-spot "O" 6)
                     (make-spot "O" 7) (make-spot "X" 8) (make-spot "X" 9)))


(define board2 (list (make-spot "" 1)  (make-spot "" 2)  (make-spot "" 3)
                     (make-spot "" 4)  (make-spot "" 5)  (make-spot "" 6)
                     (make-spot "" 7)  (make-spot "" 8)  (make-spot "" 9)))


(define board3 (list (make-spot "" 1)  (make-spot "X" 2) (make-spot "" 3)
                     (make-spot "" 4)  (make-spot "X" 5) (make-spot "O" 6)
                     (make-spot "O" 7) (make-spot "X" 8) (make-spot "" 9)))


(define board4 (list (make-spot "" 1)  (make-spot "" 2)   (make-spot "O" 3)
                     (make-spot "X" 4) (make-spot "X" 5)  (make-spot "O" 6)
                     (make-spot "O" 7) (make-spot "X" 8)  (make-spot "O" 9)))


(define board5 (list (make-spot "X" 1)  (make-spot "O" 2)  (make-spot "X" 3)
                     (make-spot "X" 4)  (make-spot "X" 5)  (make-spot "O" 6)
                     (make-spot "O" 7)  (make-spot "X" 8)  (make-spot "O" 9)))


(define board6 (list (make-spot "X" 1)  (make-spot "" 2)  (make-spot "" 3)
                     (make-spot "" 4)  (make-spot "O" 5)  (make-spot "" 6)
                     (make-spot "" 7)  (make-spot "" 8)  (make-spot "" 9)))


(define board7 (list (make-spot "O" 1)  (make-spot "O" 2)  (make-spot "" 3)
                     (make-spot "X" 4)  (make-spot "X" 5)  (make-spot "" 6)
                     (make-spot "" 7)  (make-spot "" 8)  (make-spot "" 9)))


(define board8 (list (make-spot "O" 1)  (make-spot "" 2)  (make-spot "" 3)
                     (make-spot "X" 4)  (make-spot "O" 5)  (make-spot "" 6)
                     (make-spot "" 7)  (make-spot "" 8)  (make-spot "" 9)))


(define board9 (list (make-spot "" 1)   (make-spot "" 2)   (make-spot "O" 3)
                     (make-spot "X" 4)  (make-spot "O" 5)  (make-spot "X" 6)
                     (make-spot "X" 7)  (make-spot "O" 8)  (make-spot "X" 9)))










(define (empty-spots los)
  (cond [(empty? los) empty]
        [else
         (if (string=? "" (spot-player (first los)))
             (cons (first los)
                   (empty-spots (rest los)))
             (empty-spots (rest los)))]))
                     
 






(define-struct game (player los O-difficulty game-result))
(define game1 (make-game 1 board2 0 ""))
(define game2 (make-game 1 board1 9 ""))
(define game3 (make-game 1 board3 0 "X WINS!"))
(define game4 (make-game 1 board6 0 ""))
(define game5 (make-game 1 board7 9 ""))
(define game6 (make-game 1 board8 9 ""))
(define game7 (make-game 1 board4 0 "O WINS!"))










(define (X-move current-game x y me)
  (local [
          
          
          (define (in-empty-list? spot los)
            (local [(define empty-spots-list (empty-spots los))]
              (ormap (λ(list-spot) (= (spot-index spot) (spot-index list-spot))) los)))
          
          (define (get-clicked-spot game x y)
            (local [(define unit (* (/ 5 18) SIZE))]
              (cond [(< x (- (/ SIZE 2) (/ unit 2)))
                     (cond
                       [(< y (- (/ SIZE 2) (/ unit 2)))
                        (make-spot "" 1)]
                       [(> y (+ (/ SIZE 2) (/ unit 2)))
                        (make-spot "" 7)]
                       [else 
                        (make-spot "" 4)])]
                    [(> x (+ (/ SIZE 2) (/ unit 2)))
                     (cond
                       [(< y (- (/ SIZE 2) (/ unit 2)))
                        (make-spot "" 3)]
                       [(> y (+ (/ SIZE 2) (/ unit 2)))
                        (make-spot "" 9)]
                       [else 
                        (make-spot "" 6)])]
                    [else 
                     (cond
                       [(< y (- (/ SIZE 2) (/ unit 2)))
                        (make-spot "" 2)]
                       [(> y (+ (/ SIZE 2) (/ unit 2)))
                        (make-spot "" 8)]
                       [else 
                        (make-spot "" 5)])])))]
    
    (cond [(not (string=? (game-game-result current-game) "")) 
           current-game]
          [(mouse=? me "button-down")
           (local [(define spot (get-clicked-spot current-game x y))
                   
                   
                   (define (move game)
                     (if (in-empty-list? spot (empty-spots (game-los current-game)))
                         (place-X-or-O-in-game spot current-game)
                         current-game))]
             (move current-game))]
          [else
           current-game])))


(define (place-X-or-O-in-game spot game)
  (local
    [
     
     
     (define (get-spot-player game los index)
       (if (= index (spot-index (first los)))
           (if (string=? (spot-player (first los)) "")
               "false"
               (spot-player (first los)))
           (get-spot-player game (rest los) index)))
     
     
     
     
     
     (define (three-in-a-row game loi)
       (local
         [(define (not-false? index)
            (not (string=? "false" (get-spot-player game (game-los game) index))))
     
          (define (get-player-in-spot index)
            (get-spot-player game (game-los game) index))
     
          (define index1 (first loi))
          (define index2 (first (rest loi)))
          (define index3 (first (rest (rest loi))))]
    
         (and
          (string=? (get-player-in-spot index1)
                    (get-player-in-spot index2)
                    (get-player-in-spot index3))
          (not-false? index1)
          (not-false? index2)
          (not-false? index3))))
     
     
     
     
     (define (check-if-won game)
       (cond [(ormap (λ(num) (three-in-a-row game num)) winning-combos) true]
             [else false]))
     
     
     
     
     
     (define (get-game-player player)
       (cond [(= player -1) "O"]
             [else "X"]))
     
     
     
     
     (define (after-move game spot los index) 
       (if (= index (spot-index spot))
           (cons (make-spot (get-game-player (game-player game)) (spot-index spot))
                 (rest los))
           (cons (first los)
                 (after-move game spot (rest los) (add1 index)))))

     
     
     
     
     (define (update-game-spot game spot index) 
       (game-over?
        (make-game (* -1 (game-player game))
                   (after-move game spot (game-los game) index)
                   (game-O-difficulty game)
                   (game-game-result game))))

     
     
     (define (is-tie? game)
       (andmap (λ (s) (not (string=? (spot-player s) ""))) (game-los game)))

     
     
     
     
     (define (game-over? game) 
       (cond
         [(check-if-won game)
          (make-game
           (game-player game)
           (game-los game)
           (game-O-difficulty game)
           (string-append (get-game-player (* -1 (game-player game))) " WINS!"))]
         [(is-tie? game)
          (make-game
           (game-player game)
           (game-los game)
           (game-O-difficulty game)
           "TIE!")]
         [else
          game]))
     
     
     
     
     (define (place-O-in-game game-after-X)
       (local
         [(define empty-spots? (empty-spots (game-los game-after-X)))
          (define empty-spots-length (length empty-spots?))]
         (if (check-if-won game-after-X)
             game-after-X
             (cond [(empty? empty-spots?) game-after-X]
                   [else
                    (local
                      [(define spot (minimax game-after-X game-after-X))]
                      (update-game-spot game-after-X spot 1))]))))

     (define place-X-in-game (update-game-spot game spot 1))

     
     
     
     

     
     (define-struct try (spot value))

     
     (define (minimax ws game-after-X)
       (make-spot "O" (spot-index (try-spot (max-value ws empty 0 game-after-X)))))

     
     
     
     

     (define (who-won game)
       (cond [(three-in-a-row game (list 1 2 3))
              (get-spot-player game (game-los game) 3)]
             [(three-in-a-row game (list 4 5 6))
              (get-spot-player game (game-los game) 4)]
             [(three-in-a-row game (list 7 8 9))
              (get-spot-player game (game-los game) 7)]
             [(three-in-a-row game (list 1 4 7))
              (get-spot-player game (game-los game) 4)]
             [(three-in-a-row game (list 2 5 8))
              (get-spot-player game (game-los game) 5)]
             [(three-in-a-row game (list 3 6 9))
              (get-spot-player game (game-los game) 3)]
             [(three-in-a-row game (list 1 5 9))
              (get-spot-player game (game-los game) 5)]
             [(three-in-a-row game (list 7 5 3))
              (get-spot-player game (game-los game) 5)]
             [else "T"]))

     
     
     
     
     
     
     
     (define (just-blocked? spot game)
       (local [(define (just-blocked?--3spots spot1 spot2 spot3)
                 (cond [(and (string=? (spot-player spot1) "X")
                             (string=? (spot-player spot2) "X")
                             (string=? (spot-player spot3) "O")
                             (= (spot-index spot3) (spot-index spot)))
                        true]
                       [(and (string=? (spot-player spot1) "X")
                             (string=? (spot-player spot2) "O")
                             (string=? (spot-player spot3) "X")
                             (= (spot-index spot2) (spot-index spot)))
                        true]
                       [(and (string=? (spot-player spot1) "O")
                             (string=? (spot-player spot2) "X")
                             (string=? (spot-player spot3) "X")
                             (= (spot-index spot1) (spot-index spot)))
                        true]
                       [else false]))

               (define (get-spot0 game los index)
                 (if (= index (spot-index (first los)))
                     (first los)
                     (get-spot0 game (rest los) index)))
               
               (define (get-3spots index1 index2 index3)
                 (list (get-spot0 game (game-los game) index1)
                       (get-spot0 game (game-los game) index2)
                       (get-spot0 game (game-los game) index3)))
               
               (define all-3spots
                 (list (get-3spots 1 2 3)
                       (get-3spots 4 5 6)
                       (get-3spots 7 8 9)
                       (get-3spots 1 4 7)
                       (get-3spots 2 5 8)
                       (get-3spots 3 6 9)
                       (get-3spots 1 5 9)
                       (get-3spots 7 5 3)))]
         (ormap (λ (lon)
                  (just-blocked?--3spots (list-ref lon 0) (list-ref lon 1) (list-ref lon 2)))
                all-3spots)))
     
     (define (max-value ws currenttry howdeep game-after-X)
       (local [(define minimax-player "O")]
         (cond [
                (> howdeep (game-O-difficulty ws))
                (cond [(empty? currenttry)
                       (make-try 
                        (list-ref
                         (empty-spots (game-los game-after-X))
                         (random (length (empty-spots (game-los game-after-X)))))
                        0)]
                      [else currenttry])]

               
               [(or (not (string=? (game-game-result (game-over? game-after-X)) ""))
                    (and
                     (not (empty? currenttry)) (just-blocked? (try-spot currenttry) game-after-X)))
                (make-try (try-spot currenttry)
                          (cond
                            [(just-blocked? (try-spot currenttry) game-after-X)
                             2]
                            [(string=? (who-won game-after-X) "O")
                             1]
                            [(string=? (who-won game-after-X) "X")
                             -1]
                            [else 0]))]

               
               
               
               
               
          
               
               [else
                (local [
                        (define (board-valid? los index-for-test)
                          (cond [(empty? los) false]
                                [(and (string=? (spot-player (first los)) "")
                                      (= (spot-index (first los)) index-for-test)) true]
                                [(= (spot-index (first los)) index-for-test) false]
                                [else (board-valid? (rest los) index-for-test)]))
                        
                        
                        
                        (define (possible-moves los currentindex)
                          (cond [(>= currentindex 10) empty]
                                [(board-valid? los currentindex)
                                 (local [(define (replacespot los0 spot0 acc)
                                           (cond [(empty? los0) acc]
                                                 [(= (spot-index spot0) (spot-index (first los0)))
                                                  (replacespot (rest los0) spot0
                                                               (cons spot0 acc))]
                                                 [else (replacespot (rest los0) spot0
                                                                    (cons (first los0) acc))]))]
                                   (cons (reverse (replacespot los
                                                               (make-spot "O" currentindex) empty))
                                         (possible-moves los (add1 currentindex))))]
                                [else (possible-moves los (add1 currentindex))]))
                        
                        
                        
                        (define (max-value-try lot currentbest)
                          (cond [(empty? lot)
                                 currentbest]
                                [(empty? currentbest)
                                 (max-value-try (rest lot) (first lot))]
                                [(= (try-value (first lot))
                                    (try-value currentbest))
                                 (local [(define rng (random 2))]
                                   (cond [(= rng 1) (max-value-try (rest lot) (first lot))]
                                         [else (max-value-try (rest lot) currentbest)]))]
                                [(> (try-value (first lot))
                                    (try-value currentbest))
                                 (max-value-try (rest lot) (first lot))]
                                [else (max-value-try (rest lot) currentbest)]))

                        
                        
                        (define (difference-spot reflos newlos)
                          (cond [(empty? reflos) empty]
                                [(equal? (first reflos)
                                         (first newlos))
                                 (difference-spot (rest reflos)
                                                  (rest newlos))]
                                [else (first newlos)]))]
                  (max-value-try (map (λ (board)
                                        (min-value
                                         (make-game
                                          (game-player ws)
                                          board
                                          (game-O-difficulty ws)
                                          (game-game-result ws))
                                    
                                         (cond [(empty? currenttry)
                                                (make-try (difference-spot (game-los game-after-X)
                                                                           board)
                                                          0)]
                                               [else currenttry])
                                      
                                         (add1 howdeep)
                                    
                                         (make-game
                                          (game-player ws)
                                          board
                                          (game-O-difficulty ws)
                                          (game-game-result ws))))
                                      (possible-moves (game-los ws) 1)) empty))])))

     (define (min-value ws currenttry howdeep game-after-X)
       (local [(define minimax-player "X")]
         (cond [
                (> howdeep (game-O-difficulty ws))
                (cond [(empty? currenttry)
                       (make-try 
                        (list-ref
                         (empty-spots (game-los game-after-X))
                         (random (length (empty-spots (game-los game-after-X)))))
                        0)]
                      [else currenttry])]

               
               [(not (string=? (game-game-result (game-over? game-after-X)) ""))
                (make-try (try-spot currenttry)
                          (cond
                            [(string=? (who-won game-after-X) "O")
                             1]
                            [(string=? (who-won game-after-X) "X")
                             -1]
                            [else 0]))]

               
               
               
               
               
             
               
               [else
                (local [
                        (define (board-valid? los index-for-test)
                          (cond [(empty? los) false]
                                [(and (string=? (spot-player (first los)) "")
                                      (= (spot-index (first los)) index-for-test)) true]
                                [(= (spot-index (first los)) index-for-test) false]
                                [else (board-valid? (rest los) index-for-test)]))
                        
                        
                        (define (possible-moves los currentindex)
                          (cond [(>= currentindex 10) empty]
                                [(board-valid? los currentindex)
                                 (local [(define (replacespot los0 spot0 acc)
                                           (cond [(empty? los0) acc]
                                                 [(= (spot-index spot0) (spot-index (first los0)))
                                                  (replacespot (rest los0) spot0
                                                               (cons spot0 acc))]
                                                 [else (replacespot (rest los0) spot0
                                                                    (cons (first los0) acc))]))]
                                   (cons (reverse (replacespot los
                                                               (make-spot "X" currentindex) empty))
                                         (possible-moves los (add1 currentindex))))]
                                [else (possible-moves los (add1 currentindex))]))
                        
                        
                        
                        (define (max-value-try lot currentbest)
                          (cond [(empty? lot)
                                 currentbest]
                                [(empty? currentbest)
                                 (max-value-try (rest lot) (first lot))]
                                [(< (try-value (first lot))
                                    (try-value currentbest))
                                 (max-value-try (rest lot) (first lot))]
                                [else (max-value-try (rest lot) currentbest)]))
                        
                        
                        (define (difference-spot reflos newlos)
                          (cond [(empty? reflos) empty]
                                [(equal? (first reflos)
                                         (first newlos))
                                 (difference-spot (rest reflos)
                                                  (rest newlos))]
                                [else (first newlos)]))]
                  (max-value-try (map (λ (board)
                                        (max-value
                                         (make-game
                                          (game-player ws)
                                          board
                                          (game-O-difficulty ws)
                                          (game-game-result ws))
                                         (cond [(empty? currenttry)
                                                (make-try (difference-spot (game-los game-after-X)
                                                                           board)
                                                          0)]
                                               [else currenttry])
                                         (add1 howdeep)
                                         (make-game
                                          (game-player ws)
                                          board
                                          (game-O-difficulty ws)
                                          (game-game-result ws))))
                                      (possible-moves (game-los ws) 1)) empty))])))]
    (place-O-in-game place-X-in-game)))

(check-expect (place-X-or-O-in-game (make-spot "X" 5)
                                    (make-game
                                     1
                                     (list (make-spot "X" 1) (make-spot "O" 2) (make-spot "" 3)
                                           (make-spot "" 4) (make-spot "" 5) (make-spot "" 6)
                                           (make-spot "" 7) (make-spot "" 8) (make-spot "" 9))
                                     2 ""))
              (make-game
               1
               (list (make-spot "X" 1) (make-spot "O" 2) (make-spot "" 3)
                     (make-spot "" 4) (make-spot "X" 5) (make-spot "" 6)
                     (make-spot "" 7) (make-spot "" 8) (make-spot "O" 9))
               2 ""))


(define START (make-game 1 board2 0 ""))

(define (main game)
  (big-bang game
    
    (to-draw draw-board)
    
    (on-mouse X-move)
    
    
    (on-key change-O-difficulty)))








(define (change-O-difficulty ws key)
  (local [(define num-list (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
          (define (new-O-strategy key)
            (make-game (game-player ws) (game-los ws) (string->number key) (game-game-result ws)))]
    (cond [(ormap (λ(num) (string=? key num)) num-list)
           (new-O-strategy key)]
          [else ws])))
(check-expect (change-O-difficulty game1 "3")
              (make-game (game-player game1)
                         (game-los game1)
                         3
                         (game-game-result game1)))
(check-expect (change-O-difficulty game2 "9")
              (make-game (game-player game2)
                         (game-los game2)
                         9
                         (game-game-result game2)))
(check-expect (change-O-difficulty game3 "9")
              (make-game (game-player game3)
                         (game-los game3)
                         9
                         (game-game-result game3)))



(define (draw-board ws)
  (local [(define (win-lose-text ws) (text (game-game-result ws) (floor (* 1.5 FONT-SIZE)) "green"))

          (define (play-area ws)
            (local [(define play-area-size (- SIZE (/ SIZE 6)))]
              (display-moves ws play-area-size display-dividers)))

          (define (index-to-x spot unit)
            (local [(define column (- (remainder (+ 2 (spot-index spot)) 3) 1))]
              (+ (* column (/ unit 3)) (/ unit 2))))

          (define (index-to-y spot unit)
            (local [(define row (- (floor (/ (+ 2 (spot-index spot)) 3)) 1))]
              (+ (* row (/ unit 3)) (/ unit 6))))

          (define (display-moves ws size background)
            (local [(define baselos (game-los ws))
                    (define divider (/ size 3))

                    (define (place--spot spot)
                      (text (spot-player spot)
                            FONT-SIZE
                            (if (string=? (spot-player spot) "X")
                                "red"
                                "blue")))

                    (define (place--los los)
                      (cond [(empty? los) background]
                            [else (place-image (place--spot (first los))
                                               (index-to-x (first los) size)
                                               (index-to-y (first los) size)
                                               (place--los (rest los)))]))]
              (place--los baselos)))

          (define display-dividers
            (local [(define unit (- SIZE (/ SIZE 6)))
                    (define dividerW 24)
                    (define dividerCOLOR "grey")]
              (place-image
               (rectangle unit
                          (/ unit dividerW)
                          "solid"
                          dividerCOLOR)
               (/ unit 2)
               (/ unit 3)
               (place-image
                (rectangle unit
                           (/ unit dividerW)
                           "solid"
                           dividerCOLOR)
                (/ unit 2)
                (* 2 (/ unit 3))
                (place-image
                 (rectangle (/ unit dividerW)
                            unit
                            "solid"
                            dividerCOLOR)
                 (/ unit 3)
                 (/ unit 2)
                 (place-image
                  (rectangle (/ unit dividerW)
                             unit
                             "solid"
                             dividerCOLOR)
                  (* 2 (/ unit 3))
                  (/ unit 2)
                  (square unit "solid" "white")))))))]
    
    (place-image (text (string-append "O-difficulty: "
                                      (number->string (game-O-difficulty ws)))
                       (floor (/ FONT-SIZE 4))
                       (if DISPLAY-DIFFICULTY? "black" "white"))
                 (/ SIZE 2)
                 (/ SIZE 24)
                 (place-image (win-lose-text ws)
                              (/ SIZE 2)
                              (/ SIZE 2)
                              (place-image (play-area ws)
                                           (/ SIZE 2)
                                           (/ SIZE 2)
                                           MTS)))))

(check-expect (draw-board START)
              (local
                [(define ws START)
                 (define
                   (win-lose-text ws) (text (game-game-result ws) (floor (* 1.5 FONT-SIZE)) "green"))

          (define (play-area ws)
            (local [(define play-area-size (- SIZE (/ SIZE 6)))]
              (display-moves ws play-area-size display-dividers)))

          (define (index-to-x spot unit)
            (local [(define column (- (remainder (+ 2 (spot-index spot)) 3) 1))]
              (+ (* column (/ unit 3)) (/ unit 2))))

          (define (index-to-y spot unit)
            (local [(define row (- (floor (/ (+ 2 (spot-index spot)) 3)) 1))]
              (+ (* row (/ unit 3)) (/ unit 6))))

          (define (display-moves ws size background)
            (local [(define baselos (game-los ws))
                    (define divider (/ size 3))

                    (define (place--spot spot)
                      (text (spot-player spot)
                            FONT-SIZE
                            (if (string=? (spot-player spot) "X")
                                "red"
                                "blue")))

                    (define (place--los los)
                      (cond [(empty? los) background]
                            [else (place-image (place--spot (first los))
                                               (index-to-x (first los) size)
                                               (index-to-y (first los) size)
                                               (place--los (rest los)))]))]
              (place--los baselos)))

          (define display-dividers
            (local [(define unit (- SIZE (/ SIZE 6)))
                    (define dividerW 24)
                    (define dividerCOLOR "grey")]
              (place-image
               (rectangle unit
                          (/ unit dividerW)
                          "solid"
                          dividerCOLOR)
               (/ unit 2)
               (/ unit 3)
               (place-image
                (rectangle unit
                           (/ unit dividerW)
                           "solid"
                           dividerCOLOR)
                (/ unit 2)
                (* 2 (/ unit 3))
                (place-image
                 (rectangle (/ unit dividerW)
                            unit
                            "solid"
                            dividerCOLOR)
                 (/ unit 3)
                 (/ unit 2)
                 (place-image
                  (rectangle (/ unit dividerW)
                             unit
                             "solid"
                             dividerCOLOR)
                  (* 2 (/ unit 3))
                  (/ unit 2)
                  (square unit "solid" "white")))))))]
    
    (place-image (text (string-append "O-difficulty: "
                                      (number->string (game-O-difficulty ws)))
                       (floor (/ FONT-SIZE 4))
                       (if DISPLAY-DIFFICULTY? "black" "white"))
                 (/ SIZE 2)
                 (/ SIZE 24)
                 (place-image (win-lose-text ws)
                              (/ SIZE 2)
                              (/ SIZE 2)
                              (place-image (play-area ws)
                                           (/ SIZE 2)
                                           (/ SIZE 2)
                                           MTS)))))
              