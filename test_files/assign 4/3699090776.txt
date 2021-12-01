

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 500) 
(define MTS (empty-scene SIZE SIZE))



(define COLOR-X "red")
(define COLOR-O "blue")
(define COLOR-TEXT "green")
(define TEXT-SIZE (round (/ SIZE 12)))
(define BAR-LENGTH (* 0.9 SIZE))
(define BAR-WIDTH (/ SIZE 30))
(define BAR-COLOR "black")
(define BOARD-POS 
  (list 0 1 2 
        3 4 5
        6 7 8))
(define TEXT-DRAW "Draw")
(define TEXT-PLAYER "The Player wins")
(define TEXT-COMPUTER "The Computer wins")
(define PLAYER 1)
(define COMPUTER 2)
(define DRAW 3)
(define EMPTY 0)
(define EMPTY-GRID
  (list 0 0 0
        0 0 0
        0 0 0))
(define MID-SCENE (/ SIZE 2)) 
(define BORDER-TOP-LEFT (/ SIZE 3)) 
(define MID-TOP-LEFT (/ SIZE 6)) 
(define BORDER-BOTTOM-RIGHT (* 2/3 SIZE))
(define MID-BOTTOM-RIGHT (* 5/6 SIZE)) 
(define DRAW-GRID
  (local [(define vertical
            (rectangle BAR-WIDTH BAR-LENGTH "solid" BAR-COLOR))
          (define horizontal
            (rectangle BAR-LENGTH BAR-WIDTH "solid" BAR-COLOR))]
    (place-image horizontal
                 MID-SCENE
                 BORDER-TOP-LEFT
                 (place-image horizontal
                              MID-SCENE
                              BORDER-BOTTOM-RIGHT
                              (place-image vertical
                                           BORDER-TOP-LEFT
                                           MID-SCENE
                                           (place-image vertical
                                                        BORDER-BOTTOM-RIGHT
                                                        MID-SCENE
                                                        MTS))))))

(define WIN-STATES
  (list
   (list 0 1 2)
   (list 3 4 5)
   (list 6 7 8)
   (list 0 3 6)
   (list 1 4 7)
   (list 2 5 8)
   (list 0 4 8)
   (list 2 4 6)))


(define TEST-STATUS-P1 
  (list 1 1 1
        2 2 0
        0 0 0))
(define TEST-STATUS-P2 
  (list 1 1 1
        2 2 0
        0 0 0))
(define TEST-STATUS-P3 
  (list 1 2 1
        2 1 0
        0 0 1))
(define TEST-STATUS-P1E 
  (list 1 2 2
        1 2 2
        1 1 1))
(define TEST-STATUS-C1 
  (list 1 1 2
        1 0 2
        0 0 2))
(define TEST-STATUS-C2 
  (list 2 1 0
        1 2 1
        0 0 2))
(define TEST-STATUS-C3 
  (list 1 1 0
        2 2 2
        0 0 1))
(define TEST-STATUS-R1 
  (list 1 1 0
        2 0 2
        0 2 1))
(define TEST-STATUS-R2 
  (list 2 2 0
        1 0 1
        2 1 1))
(define TEST-STATUS-D1 
  (list 2 2 1
        1 1 2
        2 1 1))
(define TEST-STATUS-D2 
  (list 1 2 1
        1 2 2
        2 1 1))



(define-struct ws (status level))





         

(define START (make-ws EMPTY-GRID 0))

(define TEST-WORLD 
  (make-ws (list 1 2 1
                 1 2 2
                 2 1 1) 0)) 




(define (main ws)
  (big-bang ws 
    
    
    (to-draw   render)   
    
    
    (on-key    key-detect)    
    
    
    (on-mouse  mouse-detect)))  







(check-expect (render START)
              (place-image DRAW-GRID
                           MID-SCENE
                           MID-SCENE
                           MTS))


(check-expect (render TEST-WORLD)
              (local [(define (color letter)
                        (if (string=? letter "X")
                            COLOR-X
                            COLOR-O))
                      (define (draw-pos box) 
                        (cond
                          [(= box 0) (list MID-TOP-LEFT MID-TOP-LEFT)]
                          [(= box 1) (list MID-SCENE MID-TOP-LEFT)]
                          [(= box 2) (list MID-BOTTOM-RIGHT MID-TOP-LEFT)]
                          [(= box 3) (list MID-TOP-LEFT MID-SCENE)]
                          [(= box 4) (list MID-SCENE MID-SCENE)]
                          [(= box 5) (list MID-BOTTOM-RIGHT MID-SCENE)]
                          [(= box 6) (list MID-TOP-LEFT MID-BOTTOM-RIGHT)]
                          [(= box 7) (list MID-SCENE MID-BOTTOM-RIGHT)]
                          [(= box 8) (list MID-BOTTOM-RIGHT MID-BOTTOM-RIGHT)]))
                      (define (draw to-draw box status)
                        (place-image
                         (text to-draw TEXT-SIZE (color to-draw))
                         (first (draw-pos box))
                         (second (draw-pos box))
                         (if (< box 8)
                             (draw-symbol (rest status) (+ box 1))
                             (square SIZE "outline" "black"))))
                      (define (draw-symbol status box)
                        (cond
                          [(empty? status) (square SIZE "outline" "black")]
                          [(= (first status) PLAYER) (draw "X" box status)]
                          [(= (first status) COMPUTER) (draw "O" box status)]
                          [else (draw-symbol (rest status) (+ box 1))]))]
                (place-image (text "Draw" TEXT-SIZE COLOR-TEXT)
                             MID-SCENE
                             MID-SCENE
                             (place-image (draw-symbol (ws-status TEST-WORLD) 0)
                                          MID-SCENE
                                          MID-SCENE
                                          (place-image DRAW-GRID
                                                       MID-SCENE
                                                       MID-SCENE
                                                       MTS)))))


  

(define (render ws) 
  (local [
          
          (define (color letter)
            (if (string=? letter "X")
                COLOR-X
                COLOR-O))
                      
          
          
          (define (draw-pos box) 
            (cond
              [(= box 0) (list MID-TOP-LEFT MID-TOP-LEFT)]
              [(= box 1) (list MID-SCENE MID-TOP-LEFT)]
              [(= box 2) (list MID-BOTTOM-RIGHT MID-TOP-LEFT)]
              [(= box 3) (list MID-TOP-LEFT MID-SCENE)]
              [(= box 4) (list MID-SCENE MID-SCENE)]
              [(= box 5) (list MID-BOTTOM-RIGHT MID-SCENE)]
              [(= box 6) (list MID-TOP-LEFT MID-BOTTOM-RIGHT)]
              [(= box 7) (list MID-SCENE MID-BOTTOM-RIGHT)]
              [(= box 8) (list MID-BOTTOM-RIGHT MID-BOTTOM-RIGHT)]))

          
          
          (define (draw to-draw box status)
            (place-image
             (text to-draw TEXT-SIZE (color to-draw))
             (first (draw-pos box)) 
             (second (draw-pos box)) 
             (if (< box 8)
                 (draw-symbol (rest status) (+ box 1))
                 (square SIZE "outline" "black"))))
                      
          
          
          (define (draw-symbol status box)
            (cond
              [(empty? status) (square SIZE "outline" "black")]
              [(= (first status) PLAYER) (draw "X" box status)]
              [(= (first status) COMPUTER) (draw "O" box status)]
              [else (draw-symbol (rest status) (+ box 1))]))

          
          
          (define (draw-text status)
            (cond
              [(= (winner status) DRAW) TEXT-DRAW]
              [(= (winner status) PLAYER) TEXT-PLAYER]
              [(= (winner status) COMPUTER) TEXT-COMPUTER]
              [else ""]))]
    (place-image (text (draw-text (ws-status ws)) TEXT-SIZE COLOR-TEXT)
                 MID-SCENE
                 MID-SCENE
                 (place-image (draw-symbol (ws-status ws) 0)
                              MID-SCENE
                              MID-SCENE
                              (place-image DRAW-GRID
                                           MID-SCENE
                                           MID-SCENE
                                           MTS)))))






(check-expect (key-detect START "2")
              (make-ws (ws-status START)
                       2))

(check-expect (key-detect START "0")
              (make-ws (ws-status START)
                       0))

(check-expect (key-detect START "a")
              (make-ws (ws-status START)
                       (ws-level START)))

  

(define (key-detect ws ke)
  (local [(define level (string->number ke))]
    (if (number? level)
        (make-ws (ws-status ws)     
                 level)
        ws)))







(check-expect (check-ends? TEST-STATUS-P1) false)

(check-expect (check-ends? TEST-STATUS-P2) false)

(check-expect (check-ends? TEST-STATUS-P3) false)

(check-expect (check-ends? TEST-STATUS-P1E) false)

(check-expect (check-ends? TEST-STATUS-C1) false)

(check-expect (check-ends? TEST-STATUS-C2) false)

(check-expect (check-ends? TEST-STATUS-C3) false)

(check-expect (check-ends? (ws-status START)) true)

(check-expect (check-ends? TEST-STATUS-R1) true)

(check-expect (check-ends? TEST-STATUS-R2) true)

(check-expect (check-ends? TEST-STATUS-D1) false)

(check-expect (check-ends? TEST-STATUS-D2) false)

  

(define (check-ends? status)
  (if (= (winner status) 0) true false))








(check-expect (winner TEST-STATUS-P1) PLAYER)

(check-expect (winner TEST-STATUS-P2) PLAYER)

(check-expect (winner TEST-STATUS-P3) PLAYER)

(check-expect (winner TEST-STATUS-P1E) PLAYER)

(check-expect (winner TEST-STATUS-C1) COMPUTER)

(check-expect (winner TEST-STATUS-C2) COMPUTER)

(check-expect (winner TEST-STATUS-C3) COMPUTER)

(check-expect (winner (ws-status START)) 0)

(check-expect (winner TEST-STATUS-R1) 0)

(check-expect (winner TEST-STATUS-R2) 0)

(check-expect (winner TEST-STATUS-D1) DRAW)

(check-expect (winner TEST-STATUS-D2) DRAW)

  

(define (winner status)
  (local [(define (check-same status user low) 
            (if (= (list-ref status (first low))
                   (list-ref status (second low))
                   (list-ref status (third low)) user)
                true
                false))
          (define (check-same-low status user win-states)
            (cond
              [(empty? win-states) false]
              [(check-same status user (first win-states)) true]
              [else (check-same-low status user (rest win-states))]))
          (define (check-full n)
            (not (= 0 n)))]
    (cond
      [(check-same-low status PLAYER WIN-STATES) PLAYER]
      [(check-same-low status COMPUTER WIN-STATES) COMPUTER]
      [(andmap check-full status) DRAW]
      [else 0])))










(define (mouse-detect ws x y event)  
  (local [
          
          
          (define (get-box x y) 
            (cond
              [(and (<= x BORDER-TOP-LEFT) (<= y BORDER-TOP-LEFT)) 0]
              [(and (<= x BORDER-BOTTOM-RIGHT) (<= y BORDER-TOP-LEFT)) 1]
              [(<= y BORDER-TOP-LEFT) 2]
              [(and (<= x BORDER-TOP-LEFT) (<= y BORDER-BOTTOM-RIGHT)) 3]
              [(and (<= x BORDER-BOTTOM-RIGHT) (<= y BORDER-BOTTOM-RIGHT)) 4]
              [(<= y BORDER-BOTTOM-RIGHT) 5]
              [(<= x BORDER-TOP-LEFT) 6]
              [(<= x BORDER-BOTTOM-RIGHT) 7]
              [else 8]))

          
          
          (define (check-legal box status)
            (if (and (< box 9)
                     (= (list-ref status box) EMPTY))
                true
                false))

          
          
          (define (player-move x y status)
            (if (check-legal (get-box x y) status) 
                (get-box x y)
                false))

          
          
          (define (update-status box status player)
            (local [(define (updates lon box position status player)
                      (if (< position 9)
                          (if (= position box)
                              (updates (append lon (cons player empty))
                                       box (+ position 1) status player) 
                              (updates (append
                                        lon
                                        (cons (list-ref status position) empty))
                                       box (+ position 1) status player))
                          lon))]
              (updates (list) box 0 status player)))

          
          
          (define (update-player player-box status)
            (if (boolean? player-box)
                false
                (update-status player-box status PLAYER))) 

          
          
          
          (define (update-computer status ws)
            (local [(define level (ws-level ws))
                    (define statusWS
                      (if (boolean? status)
                          (ws-status ws)
                          status))
                    (define computer-box (get-computer level statusWS))]
              
              (if (and (check-ends? statusWS)
                       (not (boolean? status))
                       (not (boolean? computer-box)))
                  (update-status computer-box statusWS COMPUTER)
                  statusWS)))

          
          
          
          (define (get-computer level status)
            (local [(define get-random (random 9))
                    (define (get-level level status) 
                      (cond
                        [(>= level 2)
                         (minimax 2 status)] 
                        [(= 1 level)
                         (minimax 1 status)]
                        [else get-random]))] 
              (cond
                
                
                [(boolean? (get-level level status))
                 (get-computer 0 status)] 

                
                [(and (check-legal (get-level level status) status)
                      (check-ends? status))
                 (get-level level status)] 

                
                
                [(check-ends? status)
                 (get-computer level status)]

                
                [else false]))) 

          
          
          
          (define (legal-status box status player)
            (if (and (check-ends? status) (< box 9) (check-legal box status))
                (update-status box status player) 
                false))

          
          
          (define (generate-los status player)
            (local
              [(define (generate box status player los)
                 (cond
                   
                   [(and (< box 9) (boolean? (legal-status box status player)))
                    (generate (+ box 1) status player los)]

                   
                   [(< box 9)
                    (generate
                     (+ box 1)
                     status
                     player
                     (cons (legal-status box status player) los ))]

                   
                   [(and (boolean? (legal-status box status player))
                         (empty? los))
                    (list status)]

                   
                   [else los]))]
              (generate 0 status player (list)))) 

          
          (define (los-p status) (generate-los status PLAYER))

          
          (define (los-c status) (generate-los status COMPUTER))

          
          
          
          (define (max-1 status)
            (if (= (winner status) COMPUTER)
                1
                -1))

          
          
          
          (define (max-2 status)
            (cond
              [(= (winner status) COMPUTER) 1] 
              [(= (winner status) 0) 0]
              [(= (winner status) DRAW) 0]
              [else -1]))
          
          
          
          
          (define (max-1-los status) 
            (local [(define lop (los-c status)) 
                    (define (max1 status lop)
                      (cond
                        [(empty? lop) false]
                        [(= (max-1 (first lop)) 1)
                         (status->box status (first lop))]
                        [else (max1 status (rest lop))]))]
              (max1 status lop)))


          
          
          
          (define (max-2-los status)  
            (local [
                    (define lop-c1 (los-c status))
                    
                    (define (check-next-move lop-c)
                      (cond
                        
                        [(empty? lop-c) false]
                        
                        
                        [(empty? (los-p (first lop-c)))
                         (check-next-move (rest lop-c))]

                        
                        
                        [(= (max-1 (first lop-c)) 1)
                         (status->box status (first lop-c))]

                        
                        [(ormap negative? (map max-2 (los-p (first lop-c)))) 
                         (check-next-move (rest lop-c))]

                        
                        [(ormap zero? (map max-2 (los-p (first lop-c))))
                         (status->box status (first lop-c))]


                        [else (check-next-move (rest lop-c))]))]
              (check-next-move lop-c1)))

          
          
          (define (status->box status new) 
            (local [(define (s->b status new box)
                      (cond
                        [(empty? status) false]
                        [(= (first status) (first new))
                         (s->b (rest status) (rest new) (+ box 1))]
                        [else box]))]
              (s->b status new 0)))
                
          
          
          
          
            
          (define (minimax level status)
            (cond
              [(= level 1)
               (max-1-los status)]
              [(= level 2) (max-2-los status)]))
            
          
          
          
          (define (move x y ws)
            (update-computer
             (update-player     
              (player-move x y (ws-status ws)) (ws-status ws))
             ws))]
    (if (check-ends? (ws-status ws))
        (make-ws 
         (if (mouse=? event "button-down")
             (move x y ws)
             (ws-status ws))
         (ws-level ws))
        ws)))